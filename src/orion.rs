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
    functions.insert("quit".to_string(), FunctionType::Void(quit));
    functions.insert("join".to_string(), FunctionType::String(join));
    functions.insert("type".to_string(), FunctionType::String(type_));

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

fn to_string(arg: Option<Expr>) -> String {
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
        print!("{}", to_string(arg));

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

fn quit(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<()> {
    let args = expect_args(args, 1, "quit", meta, variables)?;

    let code = match &args[0] {
        Some(code) => match code {
            Expr::Int8(n) => (*n).into(),
            Expr::Int16(n) => (*n).into(),
            Expr::Int32(n) => *n,
            Expr::Int64(_) => bail!("quit: This number is too big to convert into exit code."),
            Expr::Uint8(n) => (*n).into(),
            Expr::Uint16(n) => (*n).into(),
            Expr::Uint32(n) => (*n)
                .try_into()
                .with_context(|| "quit: Cannot convert number into exit code.")?,
            Expr::Uint64(_) => bail!("quit: This number is too big to convert into exit code."),
            Expr::String(_) => bail!("quit: Exit code must be an integer."),
            Expr::Ident(_) => unimplemented!(),
            Expr::FuncCall(_, _) => unimplemented!(),
            Expr::Let(_, _) => unimplemented!(),
            Expr::Add(_, _) => unimplemented!(),
            Expr::Subtract(_, _) => unimplemented!(),
            Expr::Multiply(_, _) => unimplemented!(),
            Expr::Divide(_, _) => unimplemented!(),
            // _ => bail!("quit: Exit code must be an integer."),
        },
        None => exit(0),
    };

    exit(code)
}

fn join(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<&'static str> {
    let args = get_args(args, meta, variables)?;

    if args.is_empty() {
        bail!("join: No arguments provided");
    }

    let mut joined = String::new();

    for arg in args {
        joined.push_str(&to_string(arg))
    }

    Ok(joined.leak())
}

fn type_(args: Args, meta: &Metadata, variables: &mut Variables) -> Result<&'static str> {
    let args = expect_args(args, 1, "type", meta, variables)?;

    let arg = &args[0];

    Ok(match arg {
        Some(e) => match e {
            Expr::Int8(_) => "i8",
            Expr::Int16(_) => "i16",
            Expr::Int32(_) => "i32",
            Expr::Int64(_) => "i64",
            Expr::Uint8(_) => "u8",
            Expr::Uint16(_) => "u16",
            Expr::Uint32(_) => "u32",
            Expr::Uint64(_) => "u64",
            Expr::String(_) => "String",
            Expr::Ident(_) => unimplemented!(),
            Expr::Add(_, _) => unimplemented!(),
            Expr::Subtract(_, _) => unimplemented!(),
            Expr::Multiply(_, _) => unimplemented!(),
            Expr::Divide(_, _) => unimplemented!(),
            Expr::FuncCall(_, _) => unimplemented!(),
            Expr::Let(_, _) => unimplemented!(),
        },
        None => "None",
    })
}
