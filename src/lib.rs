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
//!
//! run_contents("say(\"Hello, world!\")".to_string());
//! ```

// #![deny(missing_docs)]
#![deny(rustdoc::invalid_rust_codeblocks)]

use ast::Expr;
use color_eyre::{
    eyre::{bail, ContextCompat, WrapErr},
    install, Result,
};
use lalrpop_util::lalrpop_mod;
use orion::{setup_functions, setup_variables, Metadata, Variables};

use error::OrionErrors::LineError;

pub mod ast;
mod error;
mod orion;
// pub mod parser;

lalrpop_mod!(pub lrparser);

/// Run the contents of an Orion file.
///
/// NOTE: The file's CONTENT must be provided, not its PATH.
///
/// ## Example
///
/// ```rust
/// use orion_lib::run_contents;
///
/// run_contents("say(\"Hello, world!\")".to_string());
/// ```
pub fn run_contents<S: ToString>(contents: S) -> Result<()> {
    install()?;

    let contents = contents.to_string().leak();

    let mut metadata = Metadata {
        functions: setup_functions(),
        line: 0,
    };

    let mut variables = setup_variables();

    for (count, line) in contents.lines().enumerate() {
        metadata.line = count + 1;

        let line = if let Some((line, _)) = line.split_once('#') {
            line
        } else {
            line
        };

        if line.trim().is_empty() || line.trim().starts_with('#') {
            continue;
        }

        let ast = lrparser::ExprParser::new()
            .parse(line)
            .with_context(|| LineError {
                line: metadata.line,
                msg: "Failed to parse line".to_string(),
            })?;

        run(ast, &metadata, &mut variables)?;
    }

    Ok(())
}

fn run(ast: Expr, meta: &Metadata, variables: &mut Variables) -> Result<Option<Expr>> {
    let functions = &meta.functions;
    let line = meta.line;

    match ast {
        Expr::FuncCall(func, args) => {
            let func = functions.get(func.as_str()).with_context(|| LineError {
                line,
                msg: format!("Could not find function `{func}`"),
            })?;

            Ok(match func {
                orion::FunctionType::Void(f) => {
                    f(args, meta, variables)
                        .with_context(|| format!("Error on line {}", meta.line))?;
                    return Ok(None);
                }
                orion::FunctionType::String(f) => Some(Expr::String(
                    f(args, meta, variables)
                        .with_context(|| format!("Error on line {}", meta.line))?
                        .to_string(),
                )),
            })
        }
        Expr::Let(name, value) => {
            let value = run(*value, meta, variables)?;

            variables.insert(
                name,
                match value {
                    Some(expr) => match expr {
                        Expr::Int8(n) => orion::VariableType::Int8(n),
                        Expr::Int16(n) => orion::VariableType::Int16(n),
                        Expr::Int32(n) => orion::VariableType::Int32(n),
                        Expr::Int64(n) => orion::VariableType::Int64(n),
                        Expr::Uint8(n) => orion::VariableType::Uint8(n),
                        Expr::Uint16(n) => orion::VariableType::Uint16(n),
                        Expr::Uint32(n) => orion::VariableType::Uint32(n),
                        Expr::Uint64(n) => orion::VariableType::Uint64(n),
                        Expr::String(s) => orion::VariableType::String(s),
                        Expr::Ident(_) => unimplemented!(),
                        Expr::FuncCall(_, _) => unimplemented!(),
                        Expr::Let(_, _) => unimplemented!(),
                        Expr::Add(_, _) => unimplemented!(),
                        Expr::Subtract(_, _) => unimplemented!(),
                        Expr::Multiply(_, _) => unimplemented!(),
                        Expr::Divide(_, _) => unimplemented!(),
                    },
                    None => orion::VariableType::None,
                },
            );

            Ok(None)
        }
        Expr::Ident(ident) => {
            let var = variables.get(&ident).with_context(|| LineError {
                line,
                msg: format!("Could not find variable `{ident}`"),
            })?;

            let var = match var {
                orion::VariableType::String(s) => Some(Expr::String(s.to_string())),
                orion::VariableType::Int8(n) => Some(Expr::Int8(*n)),
                orion::VariableType::Int16(n) => Some(Expr::Int16(*n)),
                orion::VariableType::Int32(n) => Some(Expr::Int32(*n)),
                orion::VariableType::Int64(n) => Some(Expr::Int64(*n)),
                orion::VariableType::Uint8(n) => Some(Expr::Uint8(*n)),
                orion::VariableType::Uint16(n) => Some(Expr::Uint16(*n)),
                orion::VariableType::Uint32(n) => Some(Expr::Uint32(*n)),
                orion::VariableType::Uint64(n) => Some(Expr::Uint64(*n)),
                orion::VariableType::Float(_) => todo!(),
                orion::VariableType::None => None,
            };

            Ok(var)
        }
        Expr::Add(a, b) => {
            let a = run(*a, meta, variables)?;
            let b = run(*b, meta, variables)?;

            match (a, b) {
                (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a + b))),
                (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a + b))),
                (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a + b))),
                (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a + b))),
                (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a + b))),
                (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a + b))),
                (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a + b))),
                (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a + b))),

                (None, _) | (_, None) => bail!(LineError {
                    line,
                    msg: "add: Cannot use None in arithmetic".to_string(),
                }),

                _ => {
                    bail!(LineError {
                        line,
                        msg: "add: Cannot perform arithmetic between two different types."
                            .to_string(),
                    })
                }
            }
        }
        Expr::Subtract(a, b) => {
            let a = run(*a, meta, variables)?;
            let b = run(*b, meta, variables)?;

            match (a, b) {
                (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a - b))),
                (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a - b))),
                (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a - b))),
                (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a - b))),
                (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a - b))),
                (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a - b))),
                (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a - b))),
                (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a - b))),

                (None, _) | (_, None) => bail!(LineError {
                    line,
                    msg: "subtract: Cannot use None in arithmetic".to_string(),
                }),

                _ => {
                    bail!(LineError {
                        line,
                        msg: "subtract: Cannot perform arithmetic between two different types."
                            .to_string()
                    })
                }
            }
        }
        Expr::Multiply(a, b) => {
            let a = run(*a, meta, variables)?;
            let b = run(*b, meta, variables)?;

            match (a, b) {
                (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a * b))),
                (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a * b))),
                (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a * b))),
                (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a * b))),
                (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a * b))),
                (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a * b))),
                (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a * b))),
                (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a * b))),

                (None, _) | (_, None) => bail!(LineError {
                    line,
                    msg: "multiply: Cannot use None in arithmetic".to_string(),
                }),

                _ => {
                    bail!(LineError {
                        line,
                        msg: "multiply: Cannot perform arithmetic between two different types"
                            .to_string()
                    })
                }
            }
        }
        Expr::Divide(a, b) => {
            let a = run(*a, meta, variables)?;
            let b = run(*b, meta, variables)?;

            match (a, b) {
                (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a / b))),
                (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a / b))),
                (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a / b))),
                (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a / b))),
                (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a / b))),
                (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a / b))),
                (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a / b))),
                (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a / b))),

                (None, _) | (_, None) => bail!(LineError {
                    line,
                    msg: "Cannot use None in arithmetic".to_string(),
                }),

                _ => {
                    bail!(LineError {
                        line,
                        msg: "Cannot perform arithmetic between two different types.".to_string(),
                    })
                }
            }
        }
        e => Ok(Some(e)),
    }
}

//     match ast {
//         E::FuncCall(func, args) => {
//             let func = match functions.get(&func.to_string()) {
//                 Some(f) => f,
//                 None => bail!(LineError {
//                     line,
//                     msg: format!("Could not find function `{func}`")
//                 }),
//             };

//             match func {
//                 FunctionType::Void(f) => {
//                     f(args, functions.to_owned(), vars.to_owned())
//                         .with_context(|| format!("Error on line {line}"))?;
//                     Ok(None)
//                 }
//                 FunctionType::String(f) => {
//                     let result = f(args, functions.to_owned(), vars.to_owned())
//                         .with_context(|| format!("Error on line {line}"))?;

//                     Ok(Some(E::String(result.leak())))
//                 }
//             }
//         }
//         E::Minus(a, b) => Ok(Some(E::Float(
//             get_float(a, line, functions, vars)? - get_float(b, line, functions, vars)?,
//         ))),
//         E::Plus(a, b) => Ok(Some(E::Float(
//             get_float(a, line, functions, vars)? + get_float(b, line, functions, vars)?,
//         ))),
//         E::Multiply(a, b) => Ok(Some(E::Float(
//             get_float(a, line, functions, vars)? * get_float(b, line, functions, vars)?,
//         ))),
//         E::String(s) => Ok(Some(E::String(s))),
//         E::Divide(a, b) => Ok(Some(E::Float(
//             get_float(a, line, functions, vars)? / get_float(b, line, functions, vars)?,
//         ))),
//         E::E_Nothing => Ok(None),
//         E::Let(varname, value) => {
//             let variables = match variables {
//                 VariablesForRun::Immutable(_) => {
//                     bail!("line {line}: Cannot create variable in this context.")
//                 }
//                 VariablesForRun::Mutable(vars) => vars,
//             };

//             let value = run(
//                 value.to_owned(),
//                 line,
//                 functions,
//                 VariablesForRun::Mutable(variables),
//             )?;

//             variables.insert(
//                 varname.to_string(),
//                 match value {
//                     Some(v) => match v {
//                         E::Integer(n) => VariableType::Integer(n),
//                         E::Float(f) => VariableType::Float(f),
//                         E::String(s) => VariableType::String(s.to_string()),
//                         _ => unimplemented!(),
//                     },
//                     None => VariableType::None,
//                 },
//             );

//             Ok(None)
//         }
//         E::Integer(n) => Ok(Some(E::Integer(*n))),
//         E::Float(f) => Ok(Some(E::Float(*f))),
//         E::Ident(ident) => {
//             let var = vars
//                 .get(&ident.to_string())
//                 .with_context(|| format!("Could not find name `{ident}`"))?;

//             match var {
//                 VariableType::Integer(n) => Ok(Some(E::Integer(*n))),
//                 VariableType::Float(f) => Ok(Some(E::Float(*f))),
//                 VariableType::String(s) => Ok(Some(E::String(s.to_owned().leak()))),
//                 VariableType::None => Ok(None),
//             }
//         }
//     }
// }

// fn get_float(ast: &E, line: usize, functions: &Functions, variables: &Variables) -> Result<f64> {
//     let num = run(
//         ast,
//         line,
//         functions,
//         VariablesForRun::Immutable(variables.clone()),
//     )?;

//     Ok(match num {
//         Some(E::Float(n)) => n,
//         Some(E::Integer(n)) => n as f64,
//         _ => bail!("line {line}: Expected a number"),
//     })
// }

// #[cfg(test)]
// mod tests {
//     use self::parser::_parse_train as parse_train;

//     use super::*;

//     #[test]
//     fn test_run_contents() -> Result<()> {
//         run_contents("say(\"Hello!\")".to_string())?;
//         Ok(())
//     }

//     #[test]
//     fn test_trainer() {
//         let bump = Bump::new();

//         let mut parser = make_parser();
//         parser.exstate.set(&bump);

//         let mut tokenizer = orionlexer::from_str(
//             r#"say("Hello", "world!")
// say "gnu"
//         "#
//             .trim(),
//         );

//         let parserpath = "src/parser.rs";

//         parse_train(&mut parser, &mut tokenizer, parserpath);
//     }
// }
