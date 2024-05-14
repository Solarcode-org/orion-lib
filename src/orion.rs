//! The Orion definitions.

use std::{
    collections::HashMap,
    io::{stdin, stdout, Write as _},
};

use anyhow::{bail, Result};
use rustlr::LC;

use crate::{orion_ast::E, run};

/// The functions hashmap type alias.
pub type Functions<'a> = HashMap<String, FunctionType>;

/// The variables hashmap type alias.
pub type Variables = HashMap<String, VariableType>;

/// The function arguments type alias.
pub type Args<'a> = &'a [&'a LC<E<'a>>];

/// The function type alias.
pub type Functional<T> = fn(Args, Functions, Variables) -> Result<T>;

/// Function type.
#[derive(Debug, Clone, Copy)]
pub enum FunctionType {
    /// Functions that take in input but have no output and don't return any errors.
    Voidic(Functional<()>),

    /// Functions that take in input and have output.
    Inputic(Functional<String>),
}

/// Variable type.
#[derive(Debug, Clone)]
pub enum VariableType {
    /// String variables.
    String(String),

    /// Integer variables.
    Integer(i64),

    /// Float variables.
    Float(f64),

    /// None.
    None,
}

pub(crate) fn setup_functions<'a>() -> Functions<'a> {
    let mut functions = HashMap::new();

    functions.insert("say".to_string(), FunctionType::Voidic(say));
    functions.insert("ask".to_string(), FunctionType::Inputic(ask));
    // functions.insert("let", FunctionType::Voidic(create_var));
    // functions.insert("sum", FunctionType::Arithmetic(sum));
    // functions.insert("difference", FunctionType::Arithmetic(difference));
    // functions.insert("product", FunctionType::Arithmetic(product));
    // functions.insert("quotient", FunctionType::Arithmetic(quotient));
    // functions.insert("quit", FunctionType::Voidic(quit));
    // functions.insert("error", FunctionType::Voidic(error));
    // functions.insert("str_join", FunctionType::Inputic(str_join));

    functions
}

pub(crate) fn setup_variables() -> Variables {
    let mut variables = HashMap::new();

    variables.insert(
        "__env_version".to_string(),
        VariableType::String(env!("CARGO_PKG_VERSION").to_string()),
    );

    variables
}

// fn to_string(token: &ASTNode) -> String {
//     match &token {
//         ASTNode::String(s) => s.to_string(),
//         ASTNode::Func(f, args) => format!("{f:?}, {args:?}"),
//         ASTNode::Number(n) => n.to_string(),
//         _ => String::new(),
//     }
// }

// fn to_num(token: &ASTNode) -> Result<f64, String> {
//     match &token {
//         ASTNode::Number(n) => Ok(*n),
//         ty => Err(format!("Type {ty:?} cannot be converted to number")),
//     }
// }

fn get_args<'a>(
    args: &'a [&LC<E>],
    functions: Functions,
    variables: Variables,
    get: &mut Vec<Option<E<'a>>>,
) -> Result<()> {
    let mut args_ = vec![];

    for arg in args {
        args_.push(run(
            &arg.0,
            arg.line(),
            &functions,
            crate::VariablesForRun::Immutable(variables.clone()),
        )?)
    }

    *get = args_;

    Ok(())
}

fn expect_args<'a, T: AsRef<str>>(
    args: &'a [&LC<E>],
    args_no: usize,
    func: T,
    functions: Functions,
    variables: Variables,
    get: &mut Vec<Option<E<'a>>>,
) -> Result<()> {
    let mut args_ = vec![];
    get_args(args, functions, variables, &mut args_)?;

    if args_.len() < args_no {
        bail!(
            "{}: Not enough arguments ({}, expected: {})",
            func.as_ref(),
            args.len(),
            args_no
        );
    }

    if args_.len() > args_no {
        bail!(
            "{}: Too many arguments ({}, expected: {})",
            func.as_ref(),
            args.len(),
            args_no
        );
    }

    *get = args_;

    Ok(())
}

fn say<'a>(
    args: &'a [&'a LC<E<'a>>],
    functions: HashMap<String, FunctionType>,
    variables: HashMap<String, VariableType>,
) -> Result<()> {
    let mut args_ = vec![];
    get_args(args, functions, variables, &mut args_)?;

    for arg in args_ {
        print!(
            "{}",
            if let Some(v) = arg {
                match v {
                    E::String(s) => s.to_string(),
                    E::E_Nothing => "None".to_string(),
                    E::Float(f) => f.to_string(),
                    E::Integer(n) => n.to_string(),
                    _ => unimplemented!(),
                }
            } else {
                "None".to_string()
            }
        );

        print!(" ")
    }

    println!();

    Ok(())
}

fn ask<'a>(
    args: &'a [&'a LC<E<'a>>],
    functions: Functions,
    variables: Variables,
) -> Result<String> {
    let mut args_ = vec![];
    expect_args(args, 1, "ask", functions, variables, &mut args_)?;

    let prompt = match &args_[0] {
        Some(prompt) => match prompt {
            E::String(s) => s,
            _ => bail!("ask: Prompt must be a string"),
        },
        None => todo!(),
    };

    print!("{prompt}");

    stdout().flush()?;

    let mut inp = String::new();

    stdin().read_line(&mut inp)?;

    Ok(inp.trim().to_string())
}

// fn quit(
//     tokens: ASTNode,
//     functions: HashMap<String, FunctionType>,
//     variables: &mut HashMap<String, VariableType>,
// ) -> Result<(), String> {
//     let args = get_args(tokens, functions, variables);

//     if args.len() > 1 {
//         return Err(format!(
//             "exit: Too many arguments {{{}, expected: max(1)}}",
//             args.len()
//         ));
//     }

//     let code = to_num(args.first().unwrap_or(&ASTNode::Number(0_f64)))?;

//     let code = if code.fract() > 0_f64 {
//         return Err("exit: Expected an integer as exit code".to_string());
//     } else {
//         code as i32
//     };

//     exit(code);
// }
// fn error(
//     tokens: ASTNode,
//     functions: HashMap<String, FunctionType>,
//     variables: &mut HashMap<String, VariableType>,
// ) -> Result<(), String> {
//     let args = get_args(tokens, functions, variables);

//     if args.is_empty() {
//         return Err(format!(
//             "exit: Not enough arguments {{{}, expected: min(1) max(2)}}",
//             args.len()
//         ));
//     }

//     if args.len() > 2 {
//         return Err(format!(
//             "exit: Too many arguments {{{}, expected: min(1) max(2)}}",
//             args.len()
//         ));
//     }

//     let msg = to_string(&args[0]);
//     let code = to_num(args.get(1).unwrap_or(&ASTNode::Number(0_f64)))?;

//     let code = if code.fract() > 0_f64 {
//         return Err("exit: Expected an integer as exit code".to_string());
//     } else {
//         code as i32
//     };

//     eprintln!("{msg}");
//     exit(code);
// }

// fn str_join(
//     tokens: ASTNode,
//     functions: HashMap<String, FunctionType>,
//     variables: &mut HashMap<String, VariableType>,
// ) -> Result<String, String> {
//     let args = get_args(tokens, functions, variables);

//     if args.is_empty() {
//         return Err("Empty joining list".to_string());
//     }

//     let mut joined = String::new();

//     for arg in args {
//         joined.push_str(&to_string(&arg))
//     }

//     Ok(joined)
// }
