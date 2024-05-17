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

#![deny(missing_docs)]
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

lalrpop_mod!(
#[allow(missing_docs)]
pub lrparser
);

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_i8() -> Result<()> {
        let functions = setup_functions();

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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

        let meta = Metadata { functions, line: 1 };

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
