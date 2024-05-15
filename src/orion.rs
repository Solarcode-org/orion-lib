//! The Orion definitions.

use std::{
    collections::HashMap,
    io::{stdin, stdout, Write as _},
    process::exit,
};

use color_eyre::{
    eyre::{bail, Context, ContextCompat},
    Result, Section,
};
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
    /// Return Nothing.
    Void(Functional<()>),

    /// Return String.
    String(Functional<String>),
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

    functions.insert("say".to_string(), FunctionType::Void(say));
    functions.insert("ask".to_string(), FunctionType::String(ask));
    functions.insert("quit".to_string(), FunctionType::Void(quit));

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

    let binding = &args_[0]
        .clone()
        .with_context(|| "ask: Prompt must be a string")?;

    let prompt = match binding {
        E::String(s) => s,
        _ => bail!("ask: Prompt must be a string"),
    };

    print!("{prompt}");

    stdout().flush()?;

    let mut inp = String::new();

    stdin().read_line(&mut inp)?;

    Ok(inp.trim().to_string())
}

fn quit<'a>(args: &'a [&'a LC<E<'a>>], functions: Functions, variables: Variables) -> Result<()> {
    let mut args_ = vec![];
    expect_args(args, 1, "quit", functions, variables, &mut args_)?;

    let code = match &args_[0] {
        Some(code) => match code {
            E::Integer(i) => i,
            _ => bail!("quit: Exit code must be an integer."),
        },
        None => exit(0),
    };

    exit(
        (*code)
            .try_into()
            .with_context(|| "Expected `i32`, got `i64`")
            .with_note(|| format!("The maximum value of `i32` is {}, got {}", i32::MAX, code))?,
    )
}

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
