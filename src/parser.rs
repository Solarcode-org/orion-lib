//! The parser for Orion.

use logos::Logos as _;

use crate::{
    error::error,
    lexer::Tokens,
    orion::{FunctionType, FUNCTIONS},
};

/// Abstract Syntax Tree (AST) for Orion
#[derive(Debug)]
pub struct AST {
    /// Abstract Syntax Tree Root Expression
    pub expr: ASTNode,
}

/// A node in the Abstract Syntax Tree (AST)
#[derive(Clone, Debug)]
pub enum ASTNode {
    /// String.
    String(String),

    /// Argument Expression.
    ArgExpr(Vec<ASTNode>),

    /// Function.
    Func(FunctionType),

    /// Identifier.
    Ident(String),
}

/// The parser function.
pub fn parse(line: String, line_no: usize) -> AST {
    let mut tokens_lex = Tokens::lexer(&line);
    let mut expr: Vec<ASTNode> = vec![];
    let mut args_switch = false;
    let mut args = vec![];

    while let Some(token) = tokens_lex.next() {
        let token = match token {
            Ok(t) => t,
            Err(_) => {
                error(
                    format!("Unexpected token >> {} <<", tokens_lex.slice()),
                    line_no,
                );
            }
        };

        match token {
            Tokens::Ident(ident) => {
                let functions = FUNCTIONS.lock().unwrap();

                if functions.contains_key(&ident) {
                    let func = functions.get(&ident).unwrap();

                    push(
                        &mut expr,
                        ASTNode::Func(func.clone()),
                        args_switch,
                        &mut args,
                    );
                } else {
                    push(&mut expr, ASTNode::Ident(ident), args_switch, &mut args)
                }
            }
            Tokens::ParenOpen => args_switch = true,
            Tokens::ParenClose => {
                args_switch = false;
                push(
                    &mut expr,
                    ASTNode::ArgExpr(args.clone()),
                    args_switch,
                    &mut args,
                )
            }
            Tokens::String(s) => {
                let s = s.replace("\\n", "\n");
                let s = s.replace("\\r", "\r");
                let s = s.replace("\\t", "\t");
                let s = s.replace("\\\"", "\"");

                push(
                    &mut expr,
                    ASTNode::String(s.to_string()),
                    args_switch,
                    &mut args,
                );
            }
            _ => {}
        }
    }

    let ret = AST {
        expr: ASTNode::ArgExpr(expr),
    };

    // println!("{:?}", ret);

    ret
}

fn push(expr: &mut Vec<ASTNode>, node: ASTNode, args_switch: bool, args: &mut Vec<ASTNode>) {
    if args_switch {
        args.push(node)
    } else {
        expr.push(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast_ident() {
        let ast = parse("Hello".to_string(), 0);

        match ast.expr {
            ASTNode::ArgExpr(expr) => match &expr[0] {
                ASTNode::Ident(id) => {
                    if id != &String::from("Hello") {
                        panic!("Must be `Hello`")
                    }
                }
                _ => {
                    panic!("Must be `ident`.")
                }
            },
            _ => panic!("Must be `arg_expr`."),
        }
    }
    #[test]
    fn test_ast_string() {
        let ast = parse("\"Hello\"".to_string(), 0);

        match ast.expr {
            ASTNode::ArgExpr(expr) => match &expr[0] {
                ASTNode::String(s) => {
                    if s != &String::from("Hello") {
                        panic!("Must be `Hello`")
                    }
                }
                _ => {
                    panic!("Must be `string`.")
                }
            },
            _ => panic!("Must be `arg_expr`."),
        }
    }
}
