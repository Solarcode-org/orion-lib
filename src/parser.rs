//! The parser for Orion.
use color_eyre::{eyre::WrapErr, owo_colors::OwoColorize, Result, Section, SectionExt};
use rustlr::{Bumper, ZCParser};

use crate::{
    error::OrionErrors::LineError,
    lrparser::{orionlexer, parse_train_with, parse_with, RetTypeEnum},
    orion_ast::E,
};

/// The parser function.
pub fn parse<'a>(
    count: usize,
    parser: &mut ZCParser<RetTypeEnum<'a>, Bumper<'a, ()>>,
    tokenizer: &mut orionlexer<'a>,
) -> Result<E<'a>> {
    let result =
        parse_with(parser, tokenizer /* "grammar/orion.grammar" */).map_err(|_| LineError {
            line: count,
            msg: "Parsing Error".to_string(),
        });

    let err = parser.get_err_report().to_string();

    let result = result
        .with_context(|| "Could not parse line, error occured.")
        .with_section(move || err.header("Parser Error".bright_red()))?;

    Ok(result)
}

pub(crate) fn _parse_train<'a, P: AsRef<str>>(
    parser: &mut ZCParser<RetTypeEnum<'a>, Bumper<'a, ()>>,
    tokenizer: &mut orionlexer<'a>,
    parserpath: P,
) {
    parse_train_with(parser, tokenizer, parserpath.as_ref()).unwrap_or_else(|x| x);
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
