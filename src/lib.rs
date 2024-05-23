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

use color_eyre::{
    eyre::{bail, ContextCompat, WrapErr},
    install,
};
use lalrpop_util::lalrpop_mod;
use utils::ast::Expr;
use utils::orion::{setup_functions, setup_variables, Metadata, Variables};

use crate::utils::orion::{Function, Variable};
use prelude::*;

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

    let mut metadata = Metadata {
        functions: setup_functions(),
        ..Default::default()
    };

    let mut variables = setup_variables();

    let ast: Vec<Expr> = lrparser::StatementsParser::new()
        .parse(contents)
        .with_context(|| Errors::GeneralError("Could not parse file".to_string()))?;

    for (count, expr) in ast.iter().enumerate() {
        metadata.line = count + 1;
        run(expr.to_owned(), &metadata, &mut variables)?;
    }

    Ok(())
}

fn run(ast: Expr, meta: &Metadata, variables: &mut Variables) -> Result<Option<Expr>> {
    let functions = &meta.functions;
    let line = meta.line;

    match ast {
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
            let value = run(*value, meta, variables)?;

            variables.insert(
                name,
                match value {
                    Some(expr) => match expr {
                        Expr::Int8(n) => Variable::Int8(n, meta.scope),
                        Expr::Int16(n) => Variable::Int16(n, meta.scope),
                        Expr::Int32(n) => Variable::Int32(n, meta.scope),
                        Expr::Int64(n) => Variable::Int64(n, meta.scope),
                        Expr::Uint8(n) => Variable::Uint8(n, meta.scope),
                        Expr::Uint16(n) => Variable::Uint16(n, meta.scope),
                        Expr::Uint32(n) => Variable::Uint32(n, meta.scope),
                        Expr::Uint64(n) => Variable::Uint64(n, meta.scope),
                        Expr::String(s) => Variable::String(s, meta.scope),
                        _ => todo!(),
                    },
                    None => Variable::None(meta.scope),
                },
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
                _ => todo!(),
            };

            Ok(var)
        }
        Expr::Add(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            W(a) + W(b)
        }
        Expr::Subtract(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            W(a) - W(b)
        }
        Expr::Multiply(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            W(a) * W(b)
        }
        Expr::Divide(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            W(a) / W(b)
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
            let condition = run(*condition, meta, variables)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(*scope, meta, variables)?
            } else {
                None
            };

            Ok(value)
        }
        Expr::IfElse(condition, if_code, else_code) => {
            let condition = run(*condition, meta, variables)?;

            let value = if match condition {
                Some(Expr::Bool(b)) => b,
                _ => bail!("Invalid type for conditioning."),
            } {
                run(*if_code, meta, variables)?
            } else {
                run(*else_code, meta, variables)?
            };

            Ok(value)
        }
        Expr::GreaterThan(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i > j,
                    (Expr::Int16(i), Expr::Int16(j)) => i > j,
                    (Expr::Int32(i), Expr::Int32(j)) => i > j,
                    (Expr::Int64(i), Expr::Int64(j)) => i > j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i > j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i > j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i > j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i > j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) > len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
        }
        Expr::LesserThan(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i < j,
                    (Expr::Int16(i), Expr::Int16(j)) => i < j,
                    (Expr::Int32(i), Expr::Int32(j)) => i < j,
                    (Expr::Int64(i), Expr::Int64(j)) => i < j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i < j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i < j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i < j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i < j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) < len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
        }
        Expr::StrictlyEquals(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i == j,
                    (Expr::Int16(i), Expr::Int16(j)) => i == j,
                    (Expr::Int32(i), Expr::Int32(j)) => i == j,
                    (Expr::Int64(i), Expr::Int64(j)) => i == j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i == j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i == j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i == j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i == j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) == len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
        }
        Expr::NotEquals(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i != j,
                    (Expr::Int16(i), Expr::Int16(j)) => i != j,
                    (Expr::Int32(i), Expr::Int32(j)) => i != j,
                    (Expr::Int64(i), Expr::Int64(j)) => i != j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i != j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i != j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i != j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i != j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) != len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
        }
        Expr::GreaterThanOrStrictlyEquals(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i >= j,
                    (Expr::Int16(i), Expr::Int16(j)) => i >= j,
                    (Expr::Int32(i), Expr::Int32(j)) => i >= j,
                    (Expr::Int64(i), Expr::Int64(j)) => i >= j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i >= j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i >= j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i >= j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i >= j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) >= len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
        }
        Expr::LesserThanOrStrictlyEquals(a, b) => {
            let a = a.eval(meta, variables)?;
            let b = b.eval(meta, variables)?;

            Ok(Some(Expr::Bool(match (a, b) {
                (Some(a), Some(b)) => match (a, b) {
                    (Expr::Int8(i), Expr::Int8(j)) => i <= j,
                    (Expr::Int16(i), Expr::Int16(j)) => i <= j,
                    (Expr::Int32(i), Expr::Int32(j)) => i <= j,
                    (Expr::Int64(i), Expr::Int64(j)) => i <= j,
                    (Expr::Uint8(i), Expr::Uint8(j)) => i <= j,
                    (Expr::Uint16(i), Expr::Uint16(j)) => i <= j,
                    (Expr::Uint32(i), Expr::Uint32(j)) => i <= j,
                    (Expr::Uint64(i), Expr::Uint64(j)) => i <= j,
                    (Expr::String(_), Expr::String(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare strings. Maybe you meant to compare their length?\
                        `len(s1) <= len(s2)`"
                            .to_string()
                    }),
                    (Expr::Bool(_), Expr::Bool(_)) => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare booleans. What are you trying to achieve here 🤔?"
                            .to_string()
                    }),
                    _ => bail!(Errors::LineError {
                        line,
                        msg: "Cannot compare between different types.".to_string()
                    }),
                },
                (Some(_), None) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
                (None, None) => true,
                (None, Some(_)) => bail!(Errors::LineError {
                    line,
                    msg: "Cannot compare between different types.".to_string()
                }),
            })))
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
    });
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
