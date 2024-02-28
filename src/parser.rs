//! The parser for Orion.

use logos::Logos as _;

use crate::{
    error::error,
    lexer::Tokens,
    orion::{FunctionType, FUNCTIONS},
};

/// Abstract Syntax Tree (AST) for Orion
#[derive(Debug, Clone)]
pub enum ASTNode {
    /// The function node.
    Func(FunctionType, Box<ASTNode>),

    /// The arguments list node.
    Args(Vec<ASTNode>),

    /// The string node.
    String(String)
}

/// The parser function.
pub fn parse(line: String, line_no: usize) -> ASTNode {
    let tokens_lex = Tokens::lexer(&line);
    let mut args_switch = false;

    fn default(_args: ASTNode) {}

    let mut func = FunctionType::Printic(default);
    let mut args = vec![];
    let mut ret = ASTNode::String(String::new());

    for token in tokens_lex {
        let token = match token {
            Ok(t) => t,
            Err(_) => {
                error(
                    "Unexpected token".to_string(),
                    line_no,
                );
            }
        };

        match token {
            Tokens::Ident(ident) => {
                let functions = FUNCTIONS.lock().unwrap();

                if functions.contains_key(&ident) {
                    func = functions.get(&ident).unwrap().clone();
                } else {
                   todo!()
                }
            }
            Tokens::ParenOpen => args_switch = true,
            Tokens::ParenClose => {
                args_switch = false;

                ret = ASTNode::Func(func.clone(), Box::new(ASTNode::Args(args.clone())));
                args.clear();
            },
            Tokens::String(s) => {
                let s = s.replace("\\n", "\n");
                let s = s.replace("\\r", "\r");
                let s = s.replace("\\t", "\t");
                let s = s.replace("\\\"", "\"");

                if args_switch {
                    args.push(ASTNode::String(s));
                } else {
                    ret = ASTNode::String(s);
                }
            }
            _ => todo!()
        }
    }

    ret
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast_func() {
        let ast = parse("say(\"Hello\")".to_string(), 0);

        match ast {
            ASTNode::Func(_f, _args) => {},
            _ => panic!("Must be `func`."),
        }
    }
    #[test]
    fn test_ast_string() {
        let ast = parse("\"Hello\"".to_string(), 0);

        println!("{:?}", ast);

        match ast {
            ASTNode::String(s) => {
                if s != *"Hello" {
                    panic!("Must be `Hello`")
                }
            }
            _ => {
                panic!("Must be `string`.")
            }
        }
    }
}