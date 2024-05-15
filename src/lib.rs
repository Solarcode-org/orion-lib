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

use crate::orion::{setup_functions, setup_variables, FunctionType, VariableType};
use bumpalo::Bump;
use color_eyre::{
    eyre::{bail, Context, ContextCompat},
    install, Result,
};
use error::OrionErrors::LineError;
use lrparser::{make_parser, orionlexer};
use orion::{Functions, Variables};
use orion_ast::E;
use parser::parse;

mod error;
mod lrparser;
pub mod orion;
mod orion_ast;
pub mod parser;

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
pub fn run_contents<S: AsRef<str>>(contents: S) -> Result<()> {
    install()?;

    let functions = setup_functions();
    let mut variables = setup_variables();
    let bump = Bump::new();

    let contents = contents.as_ref();

    let mut parser = make_parser();
    parser.exstate.set(&bump);

    parser.set_err_report(true);

    for (count, line) in contents.lines().enumerate() {
        if line.trim().is_empty() || line.trim().starts_with('#') {
            continue;
        }

        let mut tokenizer = orionlexer::from_str(line);
        let ast = parse(count + 1, &mut parser, &mut tokenizer)?;

        run(
            &ast,
            count + 1,
            &functions,
            VariablesForRun::Mutable(&mut variables),
        )?;
    }

    Ok(())
}

enum VariablesForRun<'a> {
    Immutable(Variables),
    Mutable(&'a mut Variables),
}

fn run<'a>(
    ast: &E<'a>,
    line: usize,
    functions: &Functions<'a>,
    variables: VariablesForRun,
) -> Result<Option<E<'a>>> {
    let vars = match variables {
        VariablesForRun::Immutable(ref vars) => vars,
        VariablesForRun::Mutable(ref vars) => vars,
    };

    match ast {
        E::FuncCall(func, args) => {
            let func = match functions.get(&func.to_string()) {
                Some(f) => f,
                None => bail!(LineError {
                    line,
                    msg: format!("Could not find function `{func}`")
                }),
            };

            match func {
                FunctionType::Void(f) => {
                    f(args, functions.to_owned(), vars.to_owned())
                        .with_context(|| format!("Error on line {line}"))?;
                    Ok(None)
                }
                FunctionType::String(f) => {
                    let result = f(args, functions.to_owned(), vars.to_owned())
                        .with_context(|| format!("Error on line {line}"))?;

                    Ok(Some(E::String(result.leak())))
                }
            }
        }
        E::Minus(a, b) => Ok(Some(E::Float(
            get_float(a, line, functions, vars)? - get_float(b, line, functions, vars)?,
        ))),
        E::Plus(a, b) => Ok(Some(E::Float(
            get_float(a, line, functions, vars)? + get_float(b, line, functions, vars)?,
        ))),
        E::Multiply(a, b) => Ok(Some(E::Float(
            get_float(a, line, functions, vars)? * get_float(b, line, functions, vars)?,
        ))),
        E::String(s) => Ok(Some(E::String(s))),
        E::Divide(a, b) => Ok(Some(E::Float(
            get_float(a, line, functions, vars)? / get_float(b, line, functions, vars)?,
        ))),
        E::E_Nothing => Ok(None),
        E::Let(varname, value) => {
            let variables = match variables {
                VariablesForRun::Immutable(_) => {
                    bail!("line {line}: Cannot create variable in this context.")
                }
                VariablesForRun::Mutable(vars) => vars,
            };

            let value = run(
                value.to_owned(),
                line,
                functions,
                VariablesForRun::Mutable(variables),
            )?;

            variables.insert(
                varname.to_string(),
                match value {
                    Some(v) => match v {
                        E::Integer(n) => VariableType::Integer(n),
                        E::Float(f) => VariableType::Float(f),
                        E::String(s) => VariableType::String(s.to_string()),
                        _ => unimplemented!(),
                    },
                    None => VariableType::None,
                },
            );

            Ok(None)
        }
        E::Integer(n) => Ok(Some(E::Integer(*n))),
        E::Float(f) => Ok(Some(E::Float(*f))),
        E::Ident(ident) => {
            let var = vars
                .get(&ident.to_string())
                .with_context(|| format!("Could not find name `{ident}`"))?;

            match var {
                VariableType::Integer(n) => Ok(Some(E::Integer(*n))),
                VariableType::Float(f) => Ok(Some(E::Float(*f))),
                VariableType::String(s) => Ok(Some(E::String(s.to_owned().leak()))),
                VariableType::None => Ok(None),
            }
        }
    }
}

fn get_float(ast: &E, line: usize, functions: &Functions, variables: &Variables) -> Result<f64> {
    let num = run(
        ast,
        line,
        functions,
        VariablesForRun::Immutable(variables.clone()),
    )?;

    Ok(match num {
        Some(E::Float(n)) => n,
        Some(E::Integer(n)) => n as f64,
        _ => bail!("line {line}: Expected a number"),
    })
}

#[cfg(test)]
mod tests {
    use self::parser::_parse_train as parse_train;

    use super::*;

    #[test]
    fn test_run_contents() -> Result<()> {
        run_contents("say(\"Hello!\")".to_string())?;
        Ok(())
    }

    #[test]
    fn test_trainer() {
        let bump = Bump::new();

        let mut parser = make_parser();
        parser.exstate.set(&bump);

        let mut tokenizer = orionlexer::from_str(
            r#"say("Hello", "world!")
say "gnu"
        "#
            .trim(),
        );

        let parserpath = "src/parser.rs";

        parse_train(&mut parser, &mut tokenizer, parserpath);
    }
}
