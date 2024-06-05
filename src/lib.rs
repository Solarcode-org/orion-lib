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
//!     run_contents("say(\"Hello, world!\")".to_string())?;
//!     Ok(())
//! }
//! ```

#![deny(missing_docs)]
#![deny(rustdoc::invalid_rust_codeblocks)]

use color_eyre::install;
use lalrpop_util::lalrpop_mod;
use utils::ast::Expr;
use utils::orion::{setup_functions, setup_variables, Metadata, Variables};

use crate::utils::orion::{Function, Variable};
use prelude::*;
use crate::utils::ast::{CompCode, OpCode, ReassignCode};

mod error;
mod prelude;
mod types;
mod utils;

lalrpop_mod!(
    #[allow(missing_docs)]
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
///     run_contents("say(\"Hello, world!\")".to_string())?;
///     Ok(())
/// }
/// ```
pub fn run_contents<S: ToString>(contents: S) -> Result<()> {
    install()?;

    let contents = contents.to_string().leak();

    if contents.is_empty() {
        return Ok(());
    }

    let mut metadata = Metadata {
        functions: setup_functions(),
        ..Default::default()
    };

    let mut variables = setup_variables();

    let ast: Vec<Option<Expr>> = lrparser::StatementsParser::new()
        .parse(contents)
        .with_context(|| Errors::GeneralError("Could not parse file".to_string()))?;

    for (count, expr) in ast.iter().enumerate() {
        metadata.line = count + 1;
        run(expr.to_owned(), &metadata, &mut variables)?;
    }

    Ok(())
}

fn run(ast: Option<Expr>, meta: &Metadata, variables: &mut Variables) -> Result<Option<Expr>> {
    let functions = &meta.functions;
    let line = meta.line;

    if ast.is_none() {
        return Ok(None);
    }

    match ast.unwrap() {
        Expr::FuncCall(func, args) => {
            let func = functions
                .get(func.as_str())
                .with_context(|| Errors::LineError {
                    line,
                    msg: f!("Could not find function `{func}`"),
                })?;

            Ok(match func {
                Function::Void(f) => {
                    f(args, meta, variables).with_context(|| f!("Error on line {}", meta.line))?;
                    return Ok(None);
                }
                Function::String(f) => Some(Expr::String(
                    f(args, meta, variables)
                        .with_context(|| f!("Error on line {}", meta.line))?
                        .to_string(),
                )),
            })
        }
        Expr::Let(name, value) => {
            let value = value.eval(meta, variables)?;

            variables.insert(
                name,
                variable_expr_eq(value, meta.scope)
            );

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
                _ => unimplemented!(),
            };

            Ok(var)
        }
        Expr::Op(a, opcode, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            match opcode {
                OpCode::Add => W(a) + W(b),
                OpCode::Subtract => W(a) - W(b),
                OpCode::Multiply => W(a) * W(b),
                OpCode::Divide => W(a) / W(b),
            }
        }
        Expr::Compare(a, opcode, b) => {
            let a = W(a.eval(meta, variables)?);
            let b = W(b.eval(meta, variables)?);

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
                value = run(line, &meta, variables)?;
            }

            garbage(variables, &(meta.scope - 1));

            Ok(value)
        }
        Expr::If(condition, scope) => {
            let condition = run(Some(*condition), meta, variables)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(Some(*scope), meta, variables)?
            } else {
                None
            };

            Ok(value)
        }
        Expr::IfElse(condition, if_code, else_code) => {
            let condition = run(Some(*condition), meta, variables)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(Some(*if_code), meta, variables)?
            } else {
                run(Some(*else_code), meta, variables)?
            };

            Ok(value)
        }
        Expr::Property(_, _) => todo!(),
        Expr::Method(expr, method, args) => {
            let expr = expr.eval(meta, variables)?;
            let methodical = match expr {
                Some(expr) => expr,
                None => bail!(Errors::LineError {
                    line,
                    msg: "No methods belong to type `None`.".to_string()
                })
            }.to_methodical(meta, variables).with_context(|| Errors::GeneralError(
                f!("Error on line {line}")
            ))?;

            let res = methodical.0.call(&method, args).with_context(|| Errors::LineError {
                line,
                msg: f!("Failed to run method `{method}`")
            })?;

            Ok(res)
        }
        Expr::For(ident, iterable, code) => {
            match iterable.eval(meta, variables)? {
                Some(Expr::String(_)) => bail!(Errors::LineError {
                    line,
                    msg: "String is not an iterable. Maybe you meant to iterate on its characters? \
                    `s.chars()`".to_string()
                }),
                Some(Expr::Array(array)) => {
                    for expr in array {
                        let variable =
                            variable_expr_eq(expr.eval(meta, variables)?, meta.scope);

                        variables.insert(ident.to_string(), variable);

                        code.clone().eval(meta, variables)?;
                    }

                    variables.remove(&ident);
                },
                _ => bail!("for: Type is not an iterable.")
            }

            Ok(None)
        },
        Expr::ForComplex(
            initializer,
            condition,
            eoi,
            code,
        ) => {
            initializer.eval(meta, variables)?;

            loop {
                let condition = condition.clone().eval(meta, variables)?;

                match condition {
                    Some(Expr::Bool(b)) => if !b { break }
                    _ => bail!(Errors::LineError { line, msg: "Expected boolean".to_string() })
                }

                code.clone().eval(meta, variables)?;

                eoi.clone().eval(meta, variables)?;
            }

            Ok(None)
        }
        Expr::Reassign(var, operation, expr) => {
            let val = expr.clone().eval(meta, variables)?;

            match variables.get_mut(&var) {
                Some(v) => {
                    *v = match operation {
                        ReassignCode::Re => variable_expr_eq(val, meta.scope),
                        ReassignCode::Plus => {
                            let (var_expr, scope) = expr_variable_eq(v);
                            let res = (W(var_expr) + W(Some(*expr))).with_context(
                                || Errors::GeneralError(f!("Error on line {line}"))
                            )?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Minus => {
                            let (var_expr, scope) = expr_variable_eq(v);
                            let res = (W(var_expr) - W(Some(*expr))).with_context(
                                || Errors::GeneralError(f!("Error on line {line}"))
                            )?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Multiply => {
                            let (var_expr, scope) = expr_variable_eq(v);
                            let res = (W(var_expr) * W(Some(*expr))).with_context(
                                || Errors::GeneralError(f!("Error on line {line}"))
                            )?;

                            variable_expr_eq(res, scope)
                        }
                        ReassignCode::Divide => {
                            let (var_expr, scope) = expr_variable_eq(v);
                            let res = (W(var_expr) / W(Some(*expr))).with_context(
                                || Errors::GeneralError(f!("Error on line {line}"))
                            )?;

                            variable_expr_eq(res, scope)
                        }
                    };
                },
                None => bail!(f!("Variable {var} not found"))
            }

            Ok(None)
        }
        Expr::Slice(e, slice) => {
            let slice = slice.eval(meta, variables)?;
            let slice: usize = match slice {
                Some(slice) => {
                    match slice {
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
                        })
                    }
                },
                None => bail!(Errors::LineError {
                    line,
                    msg: "Cannot use None as slicer.".to_string()
                })
            };

            Ok(match e.eval(meta, variables)? {
                Some(expr) => {
                    match expr {
                        Expr::String(s) => Some(Expr::Char(s.get(slice..slice+1).with_context(
                            || Errors::LineError {
                                line,
                                msg: f!("Could not slice string at index {slice}")
                            }
                        )?.chars().next().with_context(
                            || Errors::LineError {
                                line,
                                msg: f!("Could not slice string at index {slice}")
                            }
                        )?)),
                        Expr::Array(array) => {
                            let item = array.get(slice).with_context(
                                || Errors::LineError {
                                    line,
                                    msg: f!("Could not slice array at index {slice}")
                                }
                            )?;

                            item.to_owned().eval(meta, variables)?
                        },
                        _ => bail!(Errors::LineError {
                            line,
                            msg: "Cannot slice this object.".to_string()
                        })
                    }
                }
                None => bail!(Errors::LineError {
                    line,
                    msg: "Cannot slice a None object.".to_string()
                })
            })
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
        Variable::_Float32(_, scope) => scope_ >= scope,
        Variable::_Float64(_, scope) => scope_ >= scope,
        Variable::None(scope) => scope_ >= scope,
        Variable::Array(_, scope) => scope_ >= scope,
        Variable::Char(_, scope) => scope_ >= scope,
    });
}

fn variable_expr_eq(expr: Option<Expr>, scope: usize) -> Variable {
    let expr = if let Some(expr) = expr {
        expr
    } else {
        return Variable::None(scope)
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
        _ => unimplemented!(),
    }
}

fn expr_variable_eq(var: &Variable) -> (Option<Expr>, usize)  {
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
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_i8() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Int8(1), &meta, &mut variables)?,
            Some(Expr::Int8(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Int8(1)), Box::new(Expr::Int8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int8(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Int8(1)), Box::new(Expr::Int8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int8(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Int8(1)), Box::new(Expr::Int8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int8(1))
        );

        Ok(())
    }

    #[test]
    fn test_i16() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Int16(1), &meta, &mut variables)?,
            Some(Expr::Int16(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Int16(1)), Box::new(Expr::Int16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int16(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Int16(1)), Box::new(Expr::Int16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int16(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Int16(1)), Box::new(Expr::Int16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int16(1))
        );

        Ok(())
    }

    #[test]
    fn test_i32() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Int32(1), &meta, &mut variables)?,
            Some(Expr::Int32(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Int32(1)), Box::new(Expr::Int32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int32(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Int32(1)), Box::new(Expr::Int32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int32(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Int32(1)), Box::new(Expr::Int32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int32(1))
        );

        Ok(())
    }

    #[test]
    fn test_i64() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Int64(1), &meta, &mut variables)?,
            Some(Expr::Int64(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Int64(1)), Box::new(Expr::Int64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int64(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Int64(1)), Box::new(Expr::Int64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int64(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Int64(1)), Box::new(Expr::Int64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Int64(1))
        );

        Ok(())
    }

    #[test]
    fn test_u8() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Uint8(1), &meta, &mut variables)?,
            Some(Expr::Uint8(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Uint8(1)), Box::new(Expr::Uint8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint8(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Uint8(1)), Box::new(Expr::Uint8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint8(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Uint8(1)), Box::new(Expr::Uint8(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint8(1))
        );

        Ok(())
    }

    #[test]
    fn test_u16() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Uint16(1), &meta, &mut variables)?,
            Some(Expr::Uint16(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Uint16(1)), Box::new(Expr::Uint16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint16(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Uint16(1)), Box::new(Expr::Uint16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint16(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Uint16(1)), Box::new(Expr::Uint16(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint16(1))
        );

        Ok(())
    }

    #[test]
    fn test_u32() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Uint32(1), &meta, &mut variables)?,
            Some(Expr::Uint32(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Uint32(1)), Box::new(Expr::Uint32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint32(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Uint32(1)), Box::new(Expr::Uint32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint32(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Uint32(1)), Box::new(Expr::Uint32(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint32(1))
        );

        Ok(())
    }

    #[test]
    fn test_u64() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata {
            functions,
            ..Default::default()
        };

        let mut variables = setup_variables();

        assert_eq!(
            run(Expr::Uint64(1), &meta, &mut variables)?,
            Some(Expr::Uint64(1))
        );
        assert_eq!(
            run(
                Expr::Subtract(Box::new(Expr::Uint64(1)), Box::new(Expr::Uint64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint64(0))
        );
        assert_eq!(
            run(
                Expr::Multiply(Box::new(Expr::Uint64(1)), Box::new(Expr::Uint64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint64(1))
        );
        assert_eq!(
            run(
                Expr::Divide(Box::new(Expr::Uint64(1)), Box::new(Expr::Uint64(1))),
                &meta,
                &mut variables
            )?,
            Some(Expr::Uint64(1))
        );

        Ok(())
    }
}
