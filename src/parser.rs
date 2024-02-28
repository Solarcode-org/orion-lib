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
    Func( FunctionType, Box<ASTNode>),
    Args( Vec<ASTNode>),
    String(String)
}

/// The parser function.
pub fn parse(line: String, line_no: usize) -> ASTNode {
    let tokens_lex = Tokens::lexer(&line);
    let mut args_switch = false;

    fn default(_args: Box<ASTNode>) {}

    let mut func = &FunctionType::Printic(default);
    let mut args = vec![];
    let mut ret = ASTNode::String(String::new());

    // while let Some(token) = tokens_lex.next() {
    //     let token = match token {
    //         Ok(t) => t,
    //         Err(_) => {
    //             error(
    //                 format!("Unexpected token >> {} <<", tokens_lex.slice()),
    //                 line_no,
    //             );
    //         }
    //     };
    //
    //     match token {
    //         Tokens::Ident(ident) => {
    //             let functions = FUNCTIONS.lock().unwrap();
    //
    //             if functions.contains_key(&ident) {
    //                 let func = functions.get(&ident).unwrap();
    //
    //                 push(
    //                     &mut expr,
    //                     ASTNode::Func(func.clone()),
    //                     args_switch,
    //                     &mut args,
    //                 );
    //             } else {
    //                 push(&mut expr, ASTNode::Ident(ident), args_switch, &mut args)
    //             }
    //         }
    //         Tokens::ParenOpen => args_switch = true,
    //         Tokens::ParenClose => {
    //             args_switch = false;
    //             push(
    //                 &mut expr,
    //                 ASTNode::ArgExpr(args.clone()),
    //                 args_switch,
    //                 &mut args,
    //             )
    //         }
    //         Tokens::String(s) => {
    //             let s = s.replace("\\n", "\n");
    //             let s = s.replace("\\r", "\r");
    //             let s = s.replace("\\t", "\t");
    //             let s = s.replace("\\\"", "\"");
    //
    //             push(
    //                 &mut expr,
    //                 ASTNode::String(s.to_string()),
    //                 args_switch,
    //                 &mut args,
    //             );
    //         }
    //         _ => {}
    //     }
    // }
    //
    // let ret = AST {
    //     expr: ASTNode::ArgExpr(expr),
    // };
    //
    // // println!("{:?}", ret);

    for token in tokens_lex {
        let token = match token {
            Ok(t) => t,
            Err(_) => {
                let token = tokens_lex.slice();
                error(
                    format!("Unexpected token >> {} <<", token),
                    line_no,
                );
            }
        };

        match token {
            Tokens::Ident(ident) => {
                let functions = FUNCTIONS.lock().unwrap().clone();

                if functions.contains_key(&ident) {
                    let func_ = functions.get(&ident).unwrap();

                    func = func_
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

        match ast {
            ASTNode::String(s) => {
                if s != String::from("Hello") {
                    panic!("Must be `Hello`")
                }
            }
            _ => {
                panic!("Must be `string`.")
            }
        }
    }
}
