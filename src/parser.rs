//! The parser for Orion.

use logos::Logos as _;

use crate::{error::error, lexer::Tokens, orion::{FunctionType, VariableType}, run};
use crate::orion::{read_functions, read_variables, write_variables};

/// Abstract Syntax Tree (AST) for Orion
#[derive(Debug, Clone)]
pub enum ASTNode {
    /// The function node.
    Func(FunctionType, Box<ASTNode>),

    /// The arguments list node.
    Args(Vec<ASTNode>),

    /// The string node.
    String(String),

    /// The number node.
    Number(f64),

    /// The none node.
    None
}

/// The parser function.
pub fn parse(line: String, line_no: usize) -> ASTNode {
    if line.is_empty() || line.starts_with("#") {
        return ASTNode::String(String::new());
    }

    let tokens_lex = Tokens::lexer(&line);
    let mut args_switch = false;

    fn default(_args: ASTNode) {}

    let mut func = FunctionType::Printic(default);
    let mut args = vec![];
    let mut comma = true;
    let mut ret = ASTNode::String(String::new());
    let mut var_on = false;

    for token in tokens_lex {
        let token = match token {
            Ok(t) => t,
            Err(_) => {
                error(
                    "Unexpected token",
                    line_no,
                );
            }
        };

        match token {
            Tokens::Ident(ident) => {
                if var_on {
                    let line = line.strip_prefix("let ").unwrap();
                    let line = line.strip_prefix(&ident).unwrap();
                    let line = line.strip_prefix(' ').unwrap_or(line);
                    let line = match line.strip_prefix("=") {
                        Some(line) => line,
                        None => {
                            error("Equals ('=') expected", line_no);
                        }
                    };
                    let line = line.strip_prefix(' ').unwrap_or(line);

                    let value = run(parse(line.to_string(), line_no), line_no);

                    let mut variables = write_variables();

                    match value {
                        Some(v) => {
                            variables.insert(ident, VariableType::String(v));
                        }
                        None => {
                            variables.insert(ident, VariableType::None);
                        }
                    }

                    return ASTNode::String(String::new());
                }

                let functions = read_functions();
                let variables = read_variables();

                if functions.contains_key(&ident) {
                    func = functions.get(&ident).unwrap().clone();
                }

                if variables.contains_key(&ident) {
                    let var = variables.get(&ident).unwrap().clone();

                    match var {
                        VariableType::String(s) => {
                            if args_switch {
                                push(&mut args, &mut comma, ASTNode::String(s), line_no);
                            } else {
                                ret = ASTNode::String(s);
                            }
                        }
                        VariableType::Number(n) => {
                            if args_switch {
                                push(&mut args, &mut comma, ASTNode::Number(n), line_no);
                            } else {
                                ret = ASTNode::Number(n);
                            }
                        }
                        VariableType::None => {
                            if args_switch {
                                push(&mut args, &mut comma, ASTNode::None, line_no);
                            } else {
                                ret = ASTNode::None;
                            }
                        }
                    }
                }
            }
            Tokens::ParenOpen => args_switch = true,
            Tokens::ParenClose => {
                args_switch = false;

                ret = ASTNode::Func(func.clone(), Box::new(ASTNode::Args(args.clone())));
                args.clear();
            },
            Tokens::Let => {
                var_on = true;
            }
            Tokens::Comma => comma = true,
            Tokens::String(s) => {
                let s = s.replace("\\n", "\n");
                let s = s.replace("\\r", "\r");
                let s = s.replace("\\t", "\t");
                let s = s.replace("\\\"", "\"");

                if args_switch {
                    push(&mut args, &mut comma, ASTNode::String(s), line_no);
                } else {
                    ret = ASTNode::String(s);
                }
            }
            Tokens::Equals => {},
            t => {
                error(format!("{t:?} is not yet implemented"), line_no);
            }
        }
    }

    ret
}

fn push(args: &mut Vec<ASTNode>, comma: &mut bool, arg: ASTNode, line: usize) {
    if *comma {
        *comma = false;
        (*args).push(arg);
    } else {
        error("Comma expected", line);
    }
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