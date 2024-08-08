//! The JIT engine for Orion.

use lalrpop_util::ParseError;
use crate::prelude::*;
use crate::{lrparser, lrbraces, utils};

use serde_yaml::{to_string as yamlt, from_str as yamlf};
use ron::{to_string, from_str};
use crate::utils::ast::Expr;

/// Encode a JIT string.
pub fn encode<S: ToString>(contents: S, use_braces: bool, yaml: bool) -> Result<String> {
    let contents = contents.to_string();

    if contents.is_empty() {
        return Ok(String::new());
    }

    let result = if use_braces {
        lrbraces::StatementsParser::new().parse(contents.clone().leak())
    } else {
        lrparser::StatementsParser::new().parse(contents.clone().leak())
    };

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

    if yaml {
        Ok(yamlt(&ast)?)
    } else {
        Ok(to_string(&ast)?)
    }
}

/// Decode a JIT string.
pub fn decode<S: ToString>(contents: S, yaml: bool) -> Result<Vec<Option<Expr>>> {
    let contents = contents.to_string();

    if yaml {
        Ok(yamlf(&contents)?)
    } else {
        Ok(from_str(&contents)?)
    }
}
