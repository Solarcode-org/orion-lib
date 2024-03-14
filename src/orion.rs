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

    /// Arithmetic functions
    Arithmetic(fn(ASTNode) -> Result<f64, String>)
}

/// The function return type.
pub enum ReturnType {
    /// String.
    String(String),

    /// Number.
    Number(f64),
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
        m.insert("sum".to_string(), FunctionType::Arithmetic(sum));
        m.insert("difference".to_string(), FunctionType::Arithmetic(difference));
        m.insert("product".to_string(), FunctionType::Arithmetic(product));
        m.insert("quotient".to_string(), FunctionType::Arithmetic(quotient));
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
        ASTNode::Number(n) => n.to_string(),
        _ => String::new(),
    }
}

fn to_num(token: &ASTNode) -> Result<f64, String> {
    match &token {
        ASTNode::Number(n) => Ok(*n),
        ty => Err(format!("Type {ty:?} cannot be converted to number"))
    }
}

fn get_args(tokens: ASTNode) -> Vec<ASTNode> {
    match tokens {
        ASTNode::Args(args) => args.to_vec(),
        arg => vec![arg],
    }
}

fn expect_args<T: AsRef<str>>(tokens: ASTNode, args_no: usize, func: T) -> Result<Vec<ASTNode>, String> {
    let args = get_args(tokens);

    if args.len() < args_no {
        return Err(format!("{}: Not enough arguments ({}, expected: {})", func.as_ref(), args.len(), args_no));
    }

    if args.len() > args_no {
        return Err(format!("{}: Too many arguments ({}, expected: {})", func.as_ref(), args.len(), args_no));
    }

    Ok(args)
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
    let args = expect_args(tokens, 1, "ask")?;

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

fn sum(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "sum")?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 + num2)
}

fn difference(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "difference")?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 - num2)
}

fn product(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "product")?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 * num2)
}

fn quotient(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "quotient")?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 / num2)
}