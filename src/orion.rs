//! The Orion definitions.

use std::{collections::HashMap, io::stdout};
use std::io::{stdin, Write};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::process::exit;

use crate::parser::{ASTNode, parse};
use crate::run;

/// Function type.
#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Functions that take in input but have no output and don't return any errors.
    Printic(fn(ASTNode) -> ()),

    /// Functions that take in input and have output.
    Inputic(fn(ASTNode) -> Result<String, String>),

    /// Arithmetic functions
    Arithmetic(fn(ASTNode) -> Result<f64, String>),

    /// Functions that take in input and have no output but may return an error.
    Voidic(fn(ASTNode) -> Result<(), String>),
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
        m.insert("let".to_string(), FunctionType::Voidic(create_var));
        m.insert("sum".to_string(), FunctionType::Arithmetic(sum));
        m.insert("difference".to_string(), FunctionType::Arithmetic(difference));
        m.insert("product".to_string(), FunctionType::Arithmetic(product));
        m.insert("quotient".to_string(), FunctionType::Arithmetic(quotient));
        m.insert("quit".to_string(), FunctionType::Voidic(quit));
        m.insert("error".to_string(), FunctionType::Voidic(error));
        m.insert("str_join".to_string(), FunctionType::Inputic(str_join));
        RwLock::new(m)
    };
    /// Functions for Orion.
    pub static ref VARIABLES: RwLock<HashMap<String, VariableType>> = {
        let mut m = HashMap::new();
        m.insert("__env_version".to_string(), VariableType::String(env!("CARGO_PKG_VERSION")
            .to_string()));
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
        ASTNode::Args(args_) => {
            let mut args = vec![];

            for arg in args_ {
                args.push(match arg {
                    ASTNode::Expr(line, line_no) => {
                        let value = run(parse(line, line_no), line_no);

                        match value {
                            Some(v) => {
                                v
                            }
                            None => {
                                ASTNode::None
                            }
                        }
                    }
                    arg => arg,
                })
            }

            args
        },
        arg => vec![arg],
    }
}

fn expect_args<T: AsRef<str>>(tokens: ASTNode, args_no: usize, func: T)
    -> Result<Vec<ASTNode>, String> {
    let args = get_args(tokens);

    if args.len() < args_no {
        return Err(format!("{}: Not enough arguments ({}, expected: {})", func.as_ref(), args.len(),
                           args_no));
    }

    if args.len() > args_no {
        return Err(format!("{}: Too many arguments ({}, expected: {})", func.as_ref(), args.len(),
                           args_no));
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

fn create_var(tokens: ASTNode) -> Result<(), String> {
    let args = expect_args(tokens, 2, "let")?;

    let var_name = to_string(&args[0]);
    let var_value = &args[1];

    let mut variables = write_variables();

    variables.insert(var_name, match var_value {
        ASTNode::String(s) => VariableType::String(s.to_owned()),
        ASTNode::Number(n) => VariableType::Number(*n),
        ASTNode::None => VariableType::None,
        _ => return Err("Cannot create variable with this type".to_string()),
    });

    Ok(())
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

fn quit(tokens: ASTNode) -> Result<(), String> {
    let args = get_args(tokens);

    if args.len() > 1 {
        return Err(format!("exit: Too many arguments {{{}, expected: max(1)}}", args.len()));
    }

    let code = to_num(args.get(0).unwrap_or(&ASTNode::Number(0_f64)))?;

    let code = if code.fract() > 0_f64 {
        return Err("exit: Expected an integer as exit code".to_string());
    } else {
        code as i32
    };

    exit(code);
}
fn error(tokens: ASTNode) -> Result<(), String> {
    let args = get_args(tokens);

    if args.len() < 1 {
        return Err(format!("exit: Not enough arguments {{{}, expected: min(1) max(2)}}",
                           args.len()));
    }

    if args.len() > 2 {
        return Err(format!("exit: Too many arguments {{{}, expected: min(1) max(2)}}", args.len()));
    }

    let msg = to_string(&args[0]);
    let code = to_num(args.get(1).unwrap_or(&ASTNode::Number(0_f64)))?;

    let code = if code.fract() > 0_f64 {
        return Err("exit: Expected an integer as exit code".to_string());
    } else {
        code as i32
    };

    eprintln!("{msg}");
    exit(code);
}

fn str_join(tokens: ASTNode) -> Result<String, String> {
    let args = get_args(tokens);

    if args.is_empty() {
        return Err("Empty joining list".to_string());
    }

    let mut joined = String::new();

    for arg in args {
        joined.extend(to_string(&arg).chars())
    }

    Ok(joined)
}