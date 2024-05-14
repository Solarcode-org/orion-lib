//! The parser for Orion.
use anyhow::{Context, Result};
use rustlr::{Bumper, ZCParser};

use crate::{
    error::OrionErrors::LineError,
    lrparser::{orionlexer, parse_with, RetTypeEnum},
    orion_ast::E,
};

/// The parser function.
pub fn parse<'a>(
    parser: &mut ZCParser<RetTypeEnum<'a>, Bumper<'a, ()>>,
    tokenizer: &mut orionlexer<'a>,
) -> Result<E<'a>> {
    let result = parse_with(parser, tokenizer)
        .map_err(|_| LineError(parser.linenum, "Parsing Error".to_string()))
        .with_context(|| "Could not parse line, error occured.")?;

    Ok(result)
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::orion::{setup_functions, setup_variables};

//     #[test]
//     fn test_ast_func() {
//         let functions = setup_functions();
//         let variables = setup_variables();
//         let ast = parse(&functions, &variables, "say(\"Hello\")".to_string(), 0);

//         match ast {
//             ASTNode::Func(_f, _args) => {}
//             _ => panic!("Must be `func`."),
//         }
//     }
//     #[test]
//     fn test_ast_string() {
//         let functions = setup_functions();
//         let variables = setup_variables();
//         let ast = parse(&functions, &variables, "\"Hello\"".to_string(), 0);

//         println!("{:?}", ast);

//         match ast {
//             ASTNode::String(s) => {
//                 if s != *"Hello" {
//                     panic!("Must be `Hello`")
//                 }
//             }
//             _ => {
//                 panic!("Must be `string`.")
//             }
//         }
//     }
// }
