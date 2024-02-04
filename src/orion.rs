//! The Orion definitions.

use std::io::{stdin, Write};
use std::sync::Mutex;
use std::{collections::HashMap, io::stdout};

use crate::parser::ASTNode;

/// Function type.
#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Functions that take in input but have no output.
    Printic(fn(Vec<ASTNode>) -> ()),

    /// Functions that take in input and have output.
    Inputic(fn(Vec<ASTNode>) -> Result<String, String>),
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
        ASTNode::String(s) => s.to_owned(),
        ASTNode::Func(f) => format!("{f:?}"),
        _ => "".to_string(),
    }
}

fn say(tokens: Vec<ASTNode>) -> () {
    for token in tokens {
        print!("{}", to_string(&token));

        print!(" ")
    }

    println!();
}

fn ask(tokens: Vec<ASTNode>) -> Result<String, String> {
    let args_no = 1;

    if tokens.len() < args_no {
        return Err("ask: Not enough arguments!".to_string());
    }

    if tokens.len() > args_no {
        return Err("ask: Too many arguments!".to_string());
    }

    let token = to_string(&tokens[0]);

    print!("{token}");

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
