//! The Orion definitions.

use std::{collections::HashMap, io::stdout};
use std::io::{stdin, Write};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::parser::ASTNode;

/// Function type.
#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Functions that take in input but have no output.
    Printic(fn(ASTNode) -> ()),

    /// Functions that take in input and have output.
    Inputic(fn(ASTNode) -> Result<String, String>),
}

/// Variable type.
#[derive(Debug, Clone)]
pub enum VariableType {
    /// String variables.
    String(String),
    /// Number variables.
    Number(f64),
    /// None.
    None
}

lazy_static! {
    /// Functions for Orion.
    pub static ref FUNCTIONS: RwLock<HashMap<String, FunctionType>> = {
        let mut m = HashMap::new();
        m.insert("say".to_string(), FunctionType::Printic(say));
        m.insert("ask".to_string(), FunctionType::Inputic(ask));
        RwLock::new(m)
    };
    /// Functions for Orion.
    pub static ref VARIABLES: RwLock<HashMap<String, VariableType>> = {
        let mut m = HashMap::new();
        m.insert("__env_version".to_string(), VariableType::String(env!("CARGO_PKG_VERSION").to_string()));
        RwLock::new(m)
    };
}

pub(crate) fn read_functions() -> RwLockReadGuard<'static, HashMap<String, FunctionType>> {
    return FUNCTIONS.read().unwrap();
}
pub(crate) fn read_variables() -> RwLockReadGuard<'static, HashMap<String, VariableType>> {
    return VARIABLES.read().unwrap();
}
pub(crate) fn write_variables() -> RwLockWriteGuard<'static, HashMap<String, VariableType>> {
    return VARIABLES.write().unwrap();
}

fn to_string(token: &ASTNode) -> String {
    match &token {
        ASTNode::String(s) => s.to_string(),
        ASTNode::Func(f, args) => format!("{f:?}, {args:?}"),
        _ => "".to_string(),
    }
}

fn get_args(tokens: ASTNode) -> Vec<ASTNode> {
    match tokens {
        ASTNode::Args(args) => args.to_vec(),
        arg => vec![arg],
    }
}

fn say(tokens: ASTNode) {
    let args = get_args(tokens);

    for arg in args {
        print!("{}", to_string(&arg));

        print!(" ")
    }

    println!();
}

fn ask(tokens: ASTNode) -> Result<String, String> {
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

    Ok(inp.trim().to_string())
}
