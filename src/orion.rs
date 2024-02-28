//! The Orion definitions.

use std::io::{stdin, Write};
use std::sync::Mutex;
use std::{collections::HashMap, io::stdout};

use crate::parser::ASTNode;

/// Function type.
#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Functions that take in input but have no output.
    Printic(fn(Box<ASTNode>) -> ()),

    /// Functions that take in input and have output.
    Inputic(fn(Box<ASTNode>) -> Result<String, String>),
}

lazy_static! {
    /// Functions for Orion.
    pub static ref FUNCTIONS: Mutex<HashMap<String, FunctionType>> = {
        let mut m = HashMap::new();
        m.insert("say".to_string(), FunctionType::Printic(say));
        m.insert("ask".to_string(), FunctionType::Inputic(ask));
        Mutex::new(m)
    };
}

fn to_string(token: &ASTNode) -> String {
    match &token {
        ASTNode::String(s) => s.to_string(),
        ASTNode::Func(f, args) => format!("{f:?}, {args:?}"),
        _ => "".to_string(),
    }
}

fn get_args(tokens: Box<ASTNode>) -> Vec<ASTNode> {
    match *tokens {
        ASTNode::Args(args) => args.to_vec(),
        arg => vec![arg],
    }
}

fn say(tokens: Box<ASTNode>) {
    let args = get_args(tokens);

    for arg in args {
        print!("{}", to_string(&arg));

        print!(" ")
    }

    println!();
}

fn ask(tokens: Box<ASTNode>) -> Result<String, String> {
    let args = get_args(tokens);
    let args_no = 1;

    if args.len() < args_no {
        return Err("ask: Not enough arguments!".to_string());
    }

    if args.len() > args_no {
        return Err("ask: Too many arguments!".to_string());
    }

    let prompt = to_string(&args[0]);

    print!("{prompt}");

    match stdout().flush() {
        Ok(_) => {}
        Err(e) => {
            return Err(e.to_string());
        }
    };

    let mut inp = String::new();

    match stdin().read_line(&mut inp) {
        Ok(_) => {}
        Err(e) => {
            return Err(e.to_string());
        }
    }

    return Ok(inp);
}
