//! The Orion definitions.

use std::{collections::HashMap, io::stdout};
use std::io::{stdin, Write};
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
    Voidic(fn(ASTNode, HashMap<String, FunctionType>, &mut HashMap<String, VariableType>)
               -> Result<(), String>),
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

pub(crate) fn setup_functions() -> HashMap<String, FunctionType> {
    let mut functions = HashMap::new();

    functions.insert("say".to_string(), FunctionType::Printic(say));
    functions.insert("ask".to_string(), FunctionType::Inputic(ask));
    functions.insert("let".to_string(), FunctionType::Voidic(create_var));
    functions.insert("sum".to_string(), FunctionType::Arithmetic(sum));
    functions.insert("difference".to_string(), FunctionType::Arithmetic(difference));
    functions.insert("product".to_string(), FunctionType::Arithmetic(product));
    functions.insert("quotient".to_string(), FunctionType::Arithmetic(quotient));
    functions.insert("quit".to_string(), FunctionType::Voidic(quit));
    functions.insert("error".to_string(), FunctionType::Voidic(error));
    functions.insert("str_join".to_string(), FunctionType::Inputic(str_join));

    functions
}

pub(crate) fn setup_variables() -> HashMap<String, VariableType> {
    let mut variables = HashMap::new();

    variables.insert("__env_version".to_string(), VariableType::String(env!("CARGO_PKG_VERSION")
            .to_string()));

    variables
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

fn get_args(tokens: ASTNode, functions: Option<HashMap<String, FunctionType>>,
            variables: &mut Option<HashMap<String, VariableType>>) -> Vec<ASTNode> {
    let functions = functions.unwrap_or_default();

    match tokens {
        ASTNode::Args(args_) => {
            let mut args = vec![];

            for arg in args_ {
                args.push(match arg {
                    ASTNode::Expr(line, line_no) => {
                        let value = run(parse(&functions, line, line_no), line_no,
                                        &functions, &mut variables);

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

fn expect_args<T: AsRef<str>>(tokens: ASTNode, args_no: usize, func: T,
                              functions: Option<HashMap<String, FunctionType>>,
                              variables: &mut Option<HashMap<String, FunctionType>>)
    -> Result<Vec<ASTNode>, String> {
    let args = get_args(tokens, functions, &mut variables);

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
    let args = get_args(tokens, None, None);

    for arg in args {
        print!("{}", to_string(&arg));

        print!(" ")
    }

    println!();
}

fn ask(tokens: ASTNode) -> Result<String, String> {
    let args = expect_args(tokens, 1, "ask", None)?;

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

fn create_var(tokens: ASTNode, functions: HashMap<String, FunctionType>,
              variables: &mut HashMap<String, VariableType>) -> Result<(), String> {
    let args = expect_args(tokens, 2, "let", Some(functions))?;

    let var_name = to_string(&args[0]);
    let var_value = &args[1];

    variables.insert(var_name, match var_value {
        ASTNode::String(s) => VariableType::String(s.to_owned()),
        ASTNode::Number(n) => VariableType::Number(*n),
        ASTNode::None => VariableType::None,
        _ => return Err("Cannot create variable with this type".to_string()),
    });

    Ok(())
}

fn sum(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "sum", None)?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 + num2)
}

fn difference(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "difference", None)?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 - num2)
}

fn product(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "product", None)?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 * num2)
}

fn quotient(tokens: ASTNode) -> Result<f64, String> {
    let args = expect_args(tokens, 2, "quotient", None)?;

    let num1 = to_num(&args[0])?;
    let num2 = to_num(&args[1])?;

    Ok(num1 / num2)
}

fn quit(tokens: ASTNode, _functions: HashMap<String, FunctionType>,
        _variables: HashMap<String, VariableType>) -> Result<(), String> {
    let args = get_args(tokens, None);

    if args.len() > 1 {
        return Err(format!("exit: Too many arguments {{{}, expected: max(1)}}", args.len()));
    }

    let code = to_num(args.first().unwrap_or(&ASTNode::Number(0_f64)))?;

    let code = if code.fract() > 0_f64 {
        return Err("exit: Expected an integer as exit code".to_string());
    } else {
        code as i32
    };

    exit(code);
}
fn error(tokens: ASTNode, _functions: HashMap<String, FunctionType>,
         _variables: HashMap<String, VariableType>) -> Result<(), String> {
    let args = get_args(tokens, None);

    if args.is_empty() {
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
    let args = get_args(tokens, None);

    if args.is_empty() {
        return Err("Empty joining list".to_string());
    }

    let mut joined = String::new();

    for arg in args {
        joined.push_str(&to_string(&arg))
    }

    Ok(joined)
}