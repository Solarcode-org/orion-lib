use lalrpop_util::ParseError;

use crate::lrparser;
use crate::prelude::*;
use crate::run;
use crate::utils;
use crate::utils::orion::{setup_functions, setup_variables, CustomFunctions, Metadata, Variables};

use super::ast::Expr;

/// Run an IDLE Process.
pub struct IdleRunner {
    /// Whether to use braces or not.
    pub braces: bool,

    metadata: Metadata,
    variables: Variables,
    custom_functions: CustomFunctions,
}

impl IdleRunner {
    /// Make a new IDLE Runner.
    pub fn new(braces: bool) -> Result<Self> {
        let metadata = Metadata {
            functions: setup_functions(),
            ..Default::default()
        };

        let mut variables = setup_variables();

        let mut custom_functions = CustomFunctions::new();

        let lib = f!("{}\n", include_str!("../lib/std.or"));

        let result = lrparser::StatementsParser::new()
            .parse(false, lib.leak())
            .with_context(|| "Error in standard file.")?;

        for expr in result {
            run(expr, &metadata, &mut variables, &mut custom_functions).with_context(|| {
                "Error in \
        standard file."
            })?;
        }

        Ok(Self {
            braces,
            metadata,
            variables,
            custom_functions,
        })
    }

    /// Run a line.
    ///
    /// Returns true if the line is a single line, else false.
    pub fn run_line(&mut self, line: &str) -> Result<bool> {
        if line.trim_end().ends_with('\\') {
            return Ok(false);
        }

        let result = lrparser::StatementsParser::new().parse(self.braces, line);

        let result = if result.is_err() {
            Err(match result.err().unwrap() {
                ParseError::InvalidToken { location } => {
                    let loc = utils::location::location(location, line);
                    ParseError::InvalidToken { location: loc }
                }

                ParseError::UnrecognizedEof { location, expected } => {
                    let loc = utils::location::location(location, line);
                    ParseError::UnrecognizedEof {
                        location: loc,
                        expected,
                    }
                }

                ParseError::UnrecognizedToken { token, expected } => {
                    let (loc1, token, loc2) = token;
                    let loc1 = utils::location::location(loc1, line);
                    let loc2 = utils::location::location(loc2, line);

                    ParseError::UnrecognizedToken {
                        token: (loc1, token, loc2),
                        expected,
                    }
                }

                ParseError::ExtraToken { token } => {
                    let (loc1, token, loc2) = token;
                    let loc1 = utils::location::location(loc1, line);
                    let loc2 = utils::location::location(loc2, line);

                    ParseError::ExtraToken {
                        token: (loc1, token, loc2),
                    }
                }

                ParseError::User { error } => ParseError::User { error }, // ParseError::User
            })
        } else {
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
            self.metadata.line = count + 1;

            run(
                expr.to_owned(),
                &self.metadata,
                &mut self.variables,
                &mut self.custom_functions,
            )?;
        }

        Ok(true)
    }
}
