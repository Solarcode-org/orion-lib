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

use error::try_error;
use parser::parse;

use crate::parser::ASTNode;

#[macro_use]
extern crate lazy_static;

mod error;
pub mod lexer;
pub mod orion;
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
pub fn run_contents(contents: String) {
    for (count, line) in contents.lines().enumerate() {
        let ast = parse(line.to_string(), count + 1);

        run(
            ast,
            count + 1,
        );
    }
}

fn run(ast: ASTNode, line: usize) -> Option<ASTNode> {
    if let ASTNode::Func(f, args) = ast {
        match f {
            orion::FunctionType::Printic(f) => {
                f(*args);
                None
            },
            orion::FunctionType::Inputic(f) => {
                Some(ASTNode::String(try_error(f(*args), line)))
            }
            orion::FunctionType::Arithmetic(f) => {
                Some(ASTNode::Number(try_error(f(*args), line)))
            }
            orion::FunctionType::Voidic(f) => {
                try_error(f(*args), line);
                None
            }
        }
    } else {
        Some(ast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_contents() {
        run_contents("say(\"Hello!\")".to_string());
    }
}

