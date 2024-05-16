//! The Orion definitions.

#![allow(dead_code)]
#![allow(unused_imports)]

use std::{
    borrow::Cow,
    collections::HashMap,
    io::{stdin, stdout, Write as _},
    process::exit,
};

use color_eyre::{
    eyre::{bail, Context, ContextCompat},
    Result, Section,
};

use crate::{ast::Expr, error::OrionErrors::LineError, run};

/// The functions hashmap type alias.
pub type Functions = HashMap<String, FunctionType>;

/// The variables hashmap type alias.
pub type Variables = HashMap<String, VariableType>;

/// The function arguments type alias.
pub type Args<'a> = Vec<Expr>;

/// The function type alias.
pub type Functional<T> = fn(Args, &Metadata, &mut Variables) -> Result<T>;

#[derive(Debug, Clone)]
pub struct Metadata {
    pub functions: Functions,
    pub line: usize,
}

/// Function type.
#[derive(Debug, Clone, Copy)]
pub enum FunctionType {
    /// Return Nothing.
    Void(Functional<()>),

    /// Return String.
    String(Functional<&'static str>),
}

/// Variable type.
#[derive(Debug, Clone)]
pub enum VariableType {
    /// String variables.
    String(String),

    /// Integer variables.
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),

    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),

    /// Float variables.
    Float(f64),

    /// None.
    None,
}

pub(crate) fn setup_functions() -> Functions {
    let mut functions = HashMap::new();

    functions.insert("say".to_string(), FunctionType::Void(say));
    functions.insert("ask".to_string(), FunctionType::String(ask));
    // functions.insert("quit".to_string(), FunctionType::Void(quit));

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

fn get_args(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<Vec<Option<Expr>>> {
    let mut args_ = vec![];

    for arg in args {
        args_.push(run(arg, meta, variables)?);
    }

    Ok(args_)
}

fn expect_args<T: AsRef<str>>(
    args: Args,
    args_no: usize,
    func: T,
    meta: &Metadata,
    variables: &mut Variables,
) -> Result<Vec<Option<Expr>>> {
    let args = get_args(args, meta, variables)?;

    if args.len() < args_no {
        bail!(LineError {
            line: meta.line,
            msg: format!(
                "{}: Not enough arguments ({}, expected: {})",
                func.as_ref(),
                args.len(),
                args_no
            )
        });
    }

    if args.len() > args_no {
        bail!(LineError {
            line: meta.line,
            msg: format!(
                "{}: Too many arguments ({}, expected: {})",
                func.as_ref(),
                args.len(),
                args_no
            )
        });
    }

    Ok(args)
}

fn say(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<()> {
    let args = get_args(args, meta, variables)?;

    for arg in args {
        print!(
            "{}",
            if let Some(v) = arg {
                match v {
                    Expr::String(s) => s.to_string(),
                    Expr::Int8(n) => n.to_string(),
                    Expr::Int16(n) => n.to_string(),
                    Expr::Int32(n) => n.to_string(),
                    Expr::Int64(n) => n.to_string(),
                    Expr::Uint8(n) => n.to_string(),
                    Expr::Uint16(n) => n.to_string(),
                    Expr::Uint32(n) => n.to_string(),
                    Expr::Uint64(n) => n.to_string(),
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

fn ask(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<&'static str> {
    let args = expect_args(args, 1, "ask", meta, variables)?;

    let binding = &args[0]
        .clone()
        .with_context(|| "ask: Prompt must be a string")?;

    let prompt = match binding {
        Expr::String(s) => s,
        _ => bail!("ask: Prompt must be a string"),
    };

    print!("{prompt}");

    stdout().flush()?;

    let mut inp = String::new();

    stdin().read_line(&mut inp)?;

    Ok(inp.trim().to_string().leak())
}

// fn quit<'a>(args: &'a [&'a LC<E<'a>>], functions: Functions, variables: Variables) -> Result<()> {
//     let mut args_ = vec![];
//     expect_args(args, 1, "quit", functions, variables, &mut args_)?;

//     let code = match &args_[0] {
//         Some(code) => match code {
//             E::Integer(i) => i,
//             _ => bail!("quit: Exit code must be an integer."),
//         },
//         None => exit(0),
//     };

//     exit(
//         (*code)
//             .try_into()
//             .with_context(|| "Expected `i32`, got `i64`")
//             .with_note(|| format!("The maximum value of `i32` is {}, got {}", i32::MAX, code))?,
//     )
// }

// // fn str_join(
// //     tokens: ASTNode,
// //     functions: HashMap<String, FunctionType>,
// //     variables: &mut HashMap<String, VariableType>,
// // ) -> Result<String, String> {
// //     let args = get_args(tokens, functions, variables);

// //     if args.is_empty() {
// //         return Err("Empty joining list".to_string());
// //     }

// //     let mut joined = String::new();

// //     for arg in args {
// //         joined.push_str(&to_string(&arg))
// //     }

// //     Ok(joined)
// // }
