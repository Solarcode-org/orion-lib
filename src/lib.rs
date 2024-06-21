//! > **Lexer, parser and runner for the Orion Programming Language.**
//!
//! ## Aspirations
//!
//! Out of the box, users get a polished lexer, parser and runner for Orion.
//!
//! ## Example
//!
//! Run
//! ```console
//! $ cargo add orion_lib
//! ```
//! Then use the functions
//! ```rust
//! use orion_lib::run_contents;
//! use color_eyre::Result;
//!
//! fn main() -> Result<()> {
//!     run_contents("$say(\"Hello, world!\")".to_string(), false)?;
//!     Ok(())
//! }
//! ```

#![deny(missing_docs)]
#![deny(rustdoc::invalid_rust_codeblocks)]

use std::iter::zip;

use lalrpop_util::{lalrpop_mod, ParseError};

use prelude::*;
use utils::ast::Expr;
use utils::orion::{Metadata, setup_functions, setup_variables, Variables};

use crate::utils::ast::{CompCode, OpCode, ReassignCode, Type};
use crate::utils::orion::{CustomFunctions, Function, Variable};

pub use crate::utils::jit::{encode, decode};
pub use color_eyre::install as setup_error_hooks;

mod error;
mod prelude;
mod types;
mod utils;

lalrpop_mod!(
    #[allow(missing_docs)]
    #[allow(clippy::type_complexity)]
    lrparser
);

/// Run the contents of an Orion file.
///
/// NOTE: The file's CONTENT must be provided, not its PATH.
///
/// ## Example
///
/// ```rust
/// use orion_lib::run_contents;
/// use color_eyre::Result;
///
/// fn main() -> Result<()> {
///     run_contents("$say(\"Hello, world!\")".to_string(), false)?;
///     Ok(())
/// }
/// ```
pub fn run_contents<S: ToString>(contents: S, use_braces: bool) -> Result<()> {
    let contents = contents.to_string();

    if contents.is_empty() {
        return Ok(());
    }

    let mut metadata = Metadata {
        functions: setup_functions(),
        ..Default::default()
    };

    let mut variables = setup_variables();

    let mut custom_functions = CustomFunctions::new();

    let lib = f!("{}\n", include_str!("./lib/std.or"));

    let result = lrparser::StatementsParser::new()
        .parse(false, lib.leak())
        .with_context(|| "Error in standard file.")?;

    for expr in result {
        run(expr, &metadata, &mut variables, &mut custom_functions).with_context(|| {
            "Error in \
        standard file."
        })?;
    }

    let result = lrparser::StatementsParser::new().parse(use_braces, contents.clone().leak());

    let result = if result.is_err() {
            Err(match result.err().unwrap() {
                ParseError::InvalidToken { location } => {
                    let loc = utils::location::location(location, contents);
                    ParseError::InvalidToken { location: loc }
                }

                ParseError::UnrecognizedEof { location, expected } => {
                    let loc = utils::location::location(location, contents);
                    ParseError::UnrecognizedEof {
                        location: loc,
                        expected,
                    }
                }

                ParseError::UnrecognizedToken { token, expected } => {
                    let (loc1, token, loc2) = token;
                    let loc1 = utils::location::location(loc1, &contents);
                    let loc2 = utils::location::location(loc2, contents);

                    ParseError::UnrecognizedToken {
                        token: (loc1, token, loc2),
                        expected,
                    }
                }

                ParseError::ExtraToken { token } => {
                    let (loc1, token, loc2) = token;
                    let loc1 = utils::location::location(loc1, &contents);
                    let loc2 = utils::location::location(loc2, contents);

                    ParseError::ExtraToken {
                        token: (loc1, token, loc2),
                    }
                }

                ParseError::User { error } => ParseError::User { error }, // ParseError::User
            })
        }
        else {
            result.map_err(|_| ParseError::User {
                error: "impossible",
            })
        };

    let ast: Vec<Option<Expr>> = result.unwrap_or_else(|e| {
        #[cfg(debug_assertions)]
        {
            panic!("\x1b[31m{e}\x1b[0m");
        }
        #[cfg(not(debug_assertions))]
        {
            eprintln!("\x1b[31m{e}\x1b[0m");
            std::process::exit(1)
        }
    });

    for (count, expr) in ast.iter().enumerate() {
        metadata.line = count + 1;

        run(
            expr.to_owned(),
            &metadata,
            &mut variables,
            &mut custom_functions,
        )?;
    }

    Ok(())
}

/// Run an Abstract Syntax Tree.
pub fn run_ast(ast: Vec<Option<Expr>>) -> Result<()> {
    let mut metadata = Metadata {
        functions: setup_functions(),
        ..Default::default()
    };

    let mut variables = setup_variables();

    let mut custom_functions = CustomFunctions::new();

    let lib = f!("{}\n", include_str!("./lib/std.or"));

    let result = lrparser::StatementsParser::new()
        .parse(false, lib.leak())
        .with_context(|| "Error in standard file.")?;

    for expr in result {
        run(expr, &metadata, &mut variables, &mut custom_functions).with_context(|| {
            "Error in \
        standard file."
        })?;
    }

    for (count, expr) in ast.iter().enumerate() {
        metadata.line = count + 1;

        run(
            expr.to_owned(),
            &metadata,
            &mut variables,
            &mut custom_functions,
        )?;
    }

    Ok(())
}


fn run(
    ast: Option<Expr>,
    meta: &Metadata,
    variables: &mut Variables,
    custom_functions: &mut CustomFunctions,
) -> Result<Option<Expr>> {
    let functions = &meta.functions;
    let line = meta.line;

    if ast.is_none() {
        return Ok(None);
    }

    match ast.unwrap() {
        Expr::FuncCall(func, args) => {
            if custom_functions.contains_key(&func) {
                let customs = custom_functions.clone();
                let (f_args, body) = customs.get(&func).unwrap();

                if args.len() != f_args.len() {
                    bail!(Errors::LineError {
                        line,
                        msg: f!(
                            "Invalid number of arguments! Expected: {}; Found: {}",
                            f_args.len(),
                            args.len()
                        )
                    })
                }

                let mut overrides = vec![];
                let vars = variables.clone();

                for (arg, f_arg) in zip(args, f_args) {
                    let arg = arg.eval(meta, variables, custom_functions)?;

                    if variables.contains_key(f_arg) {
                        overrides.push(vars.get_key_value(f_arg).unwrap());
                    }

                    variables.insert(f_arg.to_owned(), variable_expr_eq(arg, meta.scope + 1));
                }

                let res = body.to_owned().eval(meta, variables, custom_functions)?;

                for (key, over) in overrides {
                    variables.insert(key.to_owned(), over.to_owned());
                }

                return Ok(res);
            }

            let func: &Function = functions
                .get(func.as_str())
                .with_context(|| Errors::LineError {
                    line,
                    msg: f!("Could not find function `{func}`"),
                })?;

            Ok(match func {
                    Function::Void(f) => {
                        f(args, meta, variables, custom_functions)
                            .with_context(|| f!("Error on line {}", meta.line))?;
                        return Ok(None);
                    }
                    Function::String(f) => Some(Expr::String(
                            f(args, meta, variables, custom_functions)
                                .with_context(|| f!("Error on line {}", meta.line))?
                                .to_string(),
                        )
                    ),
                }
            )
        }

        Expr::Let(name, value) => {
            let value = value.eval(meta, variables, custom_functions)?;

            if variables.contains_key(&name) {
                bail!(Errors::LineError {
                    line,
                    msg: f!("Variable already exists. \
                    Maybe you meant to overwrite it? `{name} = value`"),
                })
            }

            variables.insert(name, variable_expr_eq(value, meta.scope));

            Ok(None)
        }
        Expr::TypeLet(ty, name, value) => {
            let value = value.eval(meta, variables, custom_functions)?;

            if let Some(ref value) = value {
                check_type(value, &ty, line)?;
            } else {
                bail!(Errors::LineError {
                    line,
                    msg: "Type does not match declaration.".to_string(),
                })
            }

            if variables.contains_key(&name) {
                bail!(Errors::LineError {
                    line,
                    msg: f!("Variable already exists. \
                    Maybe you meant to overwrite it? `{name} = value`"),
                })
            }

            variables.insert(name, variable_expr_eq(value, meta.scope));

            Ok(None)
        }
        Expr::Ident(ident) => {
            let var = variables.get(&ident).with_context(|| Errors::LineError {
                line,
                msg: f!("Could not find variable `{ident}`"),
            })?;

            let var = match var {
                Variable::String(s, _) => Some(Expr::String(s.to_string())),
                Variable::Int8(n, _) => Some(Expr::Int8(*n)),
                Variable::Int16(n, _) => Some(Expr::Int16(*n)),
                Variable::Int32(n, _) => Some(Expr::Int32(*n)),
                Variable::Int64(n, _) => Some(Expr::Int64(*n)),
                Variable::Uint8(n, _) => Some(Expr::Uint8(*n)),
                Variable::Uint16(n, _) => Some(Expr::Uint16(*n)),
                Variable::Uint32(n, _) => Some(Expr::Uint32(*n)),
                Variable::Uint64(n, _) => Some(Expr::Uint64(*n)),
                Variable::None(_) => None,
                Variable::Array(array, _) => Some(Expr::Array(array.to_vec())),
                Variable::Char(c, _) => Some(Expr::Char(*c)),
                Variable::Float(f, _) => Some(Expr::Float(*f)),
            };

            Ok(var)
        }
        Expr::Op(a, opcode, b) => {
            let a = a.eval(meta, variables, custom_functions)?;
            let b = b.eval(meta, variables, custom_functions)?;

            match opcode {
                OpCode::Add => W(a) + W(b),
                OpCode::Subtract => W(a) - W(b),
                OpCode::Multiply => W(a) * W(b),
                OpCode::Divide => W(a) / W(b),
            }
        }
        Expr::Compare(a, opcode, b) => {
            let a = W(a.eval(meta, variables, custom_functions)?);
            let b = W(b.eval(meta, variables, custom_functions)?);

            Ok(Some(match opcode {
                CompCode::Greater => Expr::Bool(a > b),
                CompCode::Lesser => Expr::Bool(a < b),
                CompCode::Equals => Expr::Bool(a == b),
                CompCode::NotEquals => Expr::Bool(a != b),
                CompCode::GreaterEquals => Expr::Bool(a >= b),
                CompCode::LesserEquals => Expr::Bool(a <= b),
            }))
        }
        Expr::Scope(lines) => {
            let mut value = None;
            let meta = Metadata {
                scope: meta.scope + 1,
                ..meta.to_owned()
            };

            for line in lines {
                value = run(line, &meta, variables, custom_functions)?;
            }

            garbage(variables, &(meta.scope - 1));

            Ok(value)
        }
        Expr::If(condition, scope) => {
            let condition = run(Some(*condition), meta, variables, custom_functions)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(Some(*scope), meta, variables, custom_functions)?
            } else {
                None
            };

            Ok(value)
        }
        Expr::IfElse(condition, if_code, else_code) => {
            let condition = run(Some(*condition), meta, variables, custom_functions)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(Some(*if_code), meta, variables, custom_functions)?
            } else {
                run(Some(*else_code), meta, variables, custom_functions)?
            };

            Ok(value)
        }
        Expr::Property(_, _) => todo!(),
        Expr::Method(expr, method, args) => {
            let expr = expr.eval(meta, variables, custom_functions)?;
            let methodical = match expr {
                Some(expr) => expr,
                None => bail!(Errors::LineError {
                    line,
                    msg: "No methods belong to type `None`.".to_string()
                }),
            }
            .to_methodical(meta, variables, custom_functions)
            .with_context(|| Errors::GeneralError(f!("Error on line {line}")))?;

            let res = methodical
                .0
                .call(&method, args)
                .with_context(|| Errors::LineError {
                    line,
                    msg: f!("Failed to run method `{method}`"),
                })?;

            Ok(res)
        }
        Expr::For(ident, iterable, code) => {
            match iterable.eval(meta, variables, custom_functions)? {
                Some(Expr::String(_)) => {
                    bail!(Errors::LineError {
                    line,
                    msg: "String is not an iterable. Maybe you meant to iterate on its characters? \
                    `s.chars()`".to_string()
                })
                }
                Some(Expr::Array(array)) => {
                    for expr in array {
                        let variable = variable_expr_eq(
                            expr.eval(meta, variables, custom_functions)?,
                            meta.scope,
                        );

                        variables.insert(ident.to_string(), variable);

                        code.clone().eval(meta, variables, custom_functions)?;
                    }

                    variables.remove(&ident);
                }
                _ => bail!("for: Type is not an iterable."),
            }

            Ok(None)
        }
        Expr::ForComplex(initializer, condition, eoi, code) => {
            initializer.eval(meta, variables, custom_functions)?;

            loop {
                let condition_ = condition.clone().eval(meta, variables, custom_functions)?;

                match condition_ {
                    Some(Expr::Bool(b)) => {
                        if !b {
                            break;
                        }
                    }
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Expected boolean".to_string()
                    }),
                }

                code.clone().eval(meta, variables, custom_functions)?;

                eoi.clone().eval(meta, variables, custom_functions)?;
            }

            Ok(None)
        }
        Expr::Reassign(var, operation, expr) => {
            let val = expr.clone().eval(meta, variables, custom_functions)?;

            match variables.get_mut(&var) {
                Some(v) => {
                    let (var_expr, scope) = expr_variable_eq(v);
                    *v = match operation {
                        ReassignCode::Re => variable_expr_eq(val, scope),
                        ReassignCode::Plus => {
                            let res = (W(var_expr) + W(val)).with_context(|| {
                                Errors::GeneralError(f!("Error on line {line}"))
                            })?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Minus => {
                            let res = (W(var_expr) - W(val)).with_context(|| {
                                Errors::GeneralError(f!("Error on line {line}"))
                            })?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Multiply => {
                            let res = (W(var_expr) * W(val)).with_context(|| {
                                Errors::GeneralError(f!("Error on line {line}"))
                            })?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Divide => {
                            let res = (W(var_expr) / W(val)).with_context(|| {
                                Errors::GeneralError(f!("Error on line {line}"))
                            })?;

                            variable_expr_eq(res, scope)
                        }
                    };
                }
                None => bail!(f!("Variable {var} not found")),
            }

            Ok(None)
        }
        Expr::Slice(e, slice) => {
            let slice = slice.eval(meta, variables, custom_functions)?;
            let slice: usize = match slice {
                Some(slice) => match slice {
                    Expr::Int8(n) => n.try_into()?,
                    Expr::Int16(n) => n.try_into()?,
                    Expr::Int32(n) => n.try_into()?,
                    Expr::Int64(n) => n.try_into()?,
                    Expr::Uint8(n) => n.into(),
                    Expr::Uint16(n) => n.into(),
                    Expr::Uint32(n) => n.try_into()?,
                    Expr::Uint64(n) => n.try_into()?,
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot use this type as slicer.".to_string()
                    }),
                },
                None => bail!(Errors::LineError {
                    line,
                    msg: "Cannot use None as slicer.".to_string()
                }),
            };

            Ok(match e.eval(meta, variables, custom_functions)? {
                Some(expr) => match expr {
                    Expr::String(s) => Some(Expr::Char(
                        s.get(slice..slice + 1)
                            .with_context(|| Errors::LineError {
                                line,
                                msg: f!("Could not slice string at index {slice}"),
                            })?
                            .chars()
                            .next()
                            .with_context(|| Errors::LineError {
                                line,
                                msg: f!("Could not slice string at index {slice}"),
                            })?,
                    )),
                    Expr::Array(array) => {
                        let item = array.get(slice).with_context(|| Errors::LineError {
                            line,
                            msg: f!("Could not slice array at index {slice}"),
                        })?;

                        item.to_owned().eval(meta, variables, custom_functions)?
                    }
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot slice this object.".to_string()
                    }),
                },
                None => bail!(Errors::LineError {
                    line,
                    msg: "Cannot slice a None object.".to_string()
                }),
            })
        }
        Expr::Func(name, args, body) => {
            if name.starts_with('$') {
                bail!(Errors::LineError {
                    line,
                    msg: f!("Cannot create inner functions! `{name}`")
                })
            }

            custom_functions.insert(name.to_string(), (args, body));

            Ok(None)
        }
        e => Ok(Some(e)),
    }
}

fn garbage(variables: &mut Variables, scope_: &usize) {
    variables.retain(|_, v| match v {
        Variable::String(_, scope) => scope_ >= scope,
        Variable::Int8(_, scope) => scope_ >= scope,
        Variable::Int16(_, scope) => scope_ >= scope,
        Variable::Int32(_, scope) => scope_ >= scope,
        Variable::Int64(_, scope) => scope_ >= scope,
        Variable::Uint8(_, scope) => scope_ >= scope,
        Variable::Uint16(_, scope) => scope_ >= scope,
        Variable::Uint32(_, scope) => scope_ >= scope,
        Variable::Uint64(_, scope) => scope_ >= scope,
        Variable::Float(_, scope) => scope_ >= scope,
        Variable::None(scope) => scope_ >= scope,
        Variable::Array(_, scope) => scope_ >= scope,
        Variable::Char(_, scope) => scope_ >= scope,
    });
}

fn variable_expr_eq(expr: Option<Expr>, scope: usize) -> Variable {
    let expr = if let Some(expr) = expr {
        expr
    } else {
        return Variable::None(scope);
    };

    match expr {
        Expr::Int8(n) => Variable::Int8(n, scope),
        Expr::Int16(n) => Variable::Int16(n, scope),
        Expr::Int32(n) => Variable::Int32(n, scope),
        Expr::Int64(n) => Variable::Int64(n, scope),
        Expr::Uint8(n) => Variable::Uint8(n, scope),
        Expr::Uint16(n) => Variable::Uint16(n, scope),
        Expr::Uint32(n) => Variable::Uint32(n, scope),
        Expr::Uint64(n) => Variable::Uint64(n, scope),
        Expr::String(s) => Variable::String(s, scope),
        Expr::Array(array) => Variable::Array(array, scope),
        Expr::Char(c) => Variable::Char(c, scope),
        Expr::Float(f) => Variable::Float(f, scope),
        _ => unimplemented!(),
    }
}

fn expr_variable_eq(var: &Variable) -> (Option<Expr>, usize) {
    match var {
        Variable::Int8(n, scope) => (Some(Expr::Int8(*n)), *scope),
        Variable::Int16(n, scope) => (Some(Expr::Int16(*n)), *scope),
        Variable::Int32(n, scope) => (Some(Expr::Int32(*n)), *scope),
        Variable::Int64(n, scope) => (Some(Expr::Int64(*n)), *scope),
        Variable::Uint8(n, scope) => (Some(Expr::Uint8(*n)), *scope),
        Variable::Uint16(n, scope) => (Some(Expr::Uint16(*n)), *scope),
        Variable::Uint32(n, scope) => (Some(Expr::Uint32(*n)), *scope),
        Variable::Uint64(n, scope) => (Some(Expr::Uint64(*n)), *scope),
        Variable::String(s, scope) => (Some(Expr::String(s.to_string())), *scope),
        Variable::Array(array, scope) => (Some(Expr::Array(array.to_vec())), *scope),
        Variable::Char(c, scope) => (Some(Expr::Char(*c)), *scope),
        Variable::Float(f, scope) => (Some(Expr::Float(*f)), *scope),
        _ => unimplemented!(),
    }
}

fn check_type(expr: &Expr, ty: &Type, line: usize) -> Result<()> {
    match (expr, ty) {
        (Expr::Int8(_), Type::Int8) | (Expr::Int8(_), Type::DynInt) => {}
        (Expr::Int16(_), Type::Int16) | (Expr::Int16(_), Type::DynInt) => {}
        (Expr::Int32(_), Type::Int32) | (Expr::Int32(_), Type::DynInt) => {}
        (Expr::Int64(_), Type::Int64) | (Expr::Int64(_), Type::DynInt) => {}
        (Expr::Uint8(_), Type::Uint8) | (Expr::Uint8(_), Type::DynInt) => {}
        (Expr::Uint16(_), Type::Uint16) | (Expr::Uint16(_), Type::DynInt) => {}
        (Expr::Uint32(_), Type::Uint32) | (Expr::Uint32(_), Type::DynInt) => {}
        (Expr::Uint64(_), Type::Uint64) | (Expr::Uint64(_), Type::DynInt) => {}
        (Expr::String(_), Type::String) => {}
        (Expr::Float(_), Type::Float) => {},
        (Expr::Bool(_), Type::Bool) => {}
        (Expr::Char(_), Type::Char) => {}
        (Expr::Array(array), Type::Array(ty)) => {
            if let Some(ty) = &**ty {
                for expr in array {
                    check_type(expr, ty, line)?;
                }
            }
        }
        (_, _) => bail!(Errors::LineError {
            line,
            msg: "Type does not match declaration".to_string()
        }),
    }

    Ok(())
}
