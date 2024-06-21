//! The Orion definitions.

use crate::prelude::*;

use std::{
    collections::HashMap,
    io::{stdin, stdout, Write as _},
    process::exit,
};

use color_eyre::eyre::{bail, Context};

use crate::{error::Errors::LineError, run};
use crate::utils::ast::Expr;

/// The functions hashmap type alias.
pub type Functions = HashMap<String, Function>;

/// The variables hashmap type alias.
pub type Variables = HashMap<String, Variable>;

/// The function arguments type alias.
pub type Args<'a> = Vec<Expr>;

/// The function type alias.
pub type Functional<T> = fn(Args, &Metadata, &mut Variables, &mut CustomFunctions) -> Result<T>;

/// The custom function hashmap type alias.
pub type CustomFunctions = HashMap<String, (Vec<String>, Box<Expr>)>;

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    pub functions: Functions,
    pub line: usize,
    pub scope: usize,
}

/// Function type.
#[derive(Debug, Clone, Copy)]
pub enum Function {
    /// Return Nothing.
    Void(Functional<()>),

    /// Return String.
    String(Functional<&'static str>),
}

/// Variable type.
#[derive(Debug, Clone)]
pub enum Variable {
    /// String variables.
    String(String, usize),

    /// Integer variables.
    Int8(i8, usize),
    Int16(i16, usize),
    Int32(i32, usize),
    Int64(i64, usize),

    Uint8(u8, usize),
    Uint16(u16, usize),
    Uint32(u32, usize),
    Uint64(u64, usize),

    /// Float variables.
    Float(f32, usize),

    /// None.
    None(usize),

    /// Array.
    Array(Vec<Expr>, usize),

    /// Character.
    Char(char, usize),
}

pub(crate) fn setup_functions() -> Functions {
    let mut functions = HashMap::new();

    functions.insert("$say".to_string(), Function::Void(say));
    functions.insert("$ask".to_string(), Function::String(ask));
    functions.insert("$quit".to_string(), Function::Void(quit));
    functions.insert("$join".to_string(), Function::String(join));
    functions.insert("$type".to_string(), Function::String(type_));

    functions
}

pub(crate) fn setup_variables() -> Variables {
    let mut variables = HashMap::new();

    variables.insert(
        "__env_version".to_string(),
        Variable::String(env!("CARGO_PKG_VERSION").to_string(), 0),
    );

    variables
}

fn to_repr(metadata: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions, arg: Option<Expr>) -> Result<String> {
    Ok(if let Some(v) = arg {
        match v {
            Expr::String(s) => f!("\"{s}\""),
            Expr::Int8(n) => n.to_string(),
            Expr::Int16(n) => n.to_string(),
            Expr::Int32(n) => n.to_string(),
            Expr::Int64(n) => n.to_string(),
            Expr::Uint8(n) => n.to_string(),
            Expr::Uint16(n) => n.to_string(),
            Expr::Uint32(n) => n.to_string(),
            Expr::Uint64(n) => n.to_string(),
            Expr::Array(ref _array) => to_string(metadata, variables, custom_functions, Some(v))?,
            Expr::Char(c) => f!("'{c}'"),
            _ => unimplemented!(),
        }
    } else {
        "None".to_string()
    })
}

fn to_string(
    metadata: &Metadata,
    variables: &mut Variables,
    custom_functions: &mut CustomFunctions,
    arg: Option<Expr>,
) -> Result<String> {
    Ok(if let Some(v) = arg {
        match v {
            Expr::String(s) => s.to_string(),
            Expr::Char(c) => c.to_string(),
            Expr::Int8(n) => n.to_string(),
            Expr::Int16(n) => n.to_string(),
            Expr::Int32(n) => n.to_string(),
            Expr::Int64(n) => n.to_string(),
            Expr::Uint8(n) => n.to_string(),
            Expr::Uint16(n) => n.to_string(),
            Expr::Uint32(n) => n.to_string(),
            Expr::Uint64(n) => n.to_string(),
            Expr::Float(f) => f.to_string(),
            Expr::Array(array) => {
                let mut arr = String::from('[');

                for expr in array {
                    let res = expr.eval(metadata, variables, custom_functions);
                    let s = to_repr(metadata, variables, custom_functions, res?)?;

                    arr.push_str(&s);
                    arr.push_str(", ");
                }

                arr.pop(); arr.pop();

                arr.push(']');

                arr
            },
            Expr::Bool(b) => b.to_string(),
            _ => unimplemented!(),
        }
    } else {
        "None".to_string()
    })
}

fn get_args(args: Args, meta: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions) -> Result<Vec<Option<Expr>>> {
    let mut args_ = vec![];

    for arg in args {
        args_.push(run(Some(arg), meta, variables, custom_functions)?);
    }

    Ok(args_)
}

fn expect_args<T: AsRef<str>>(
    args: Args,
    args_no: usize,
    func: T,
    meta: &Metadata,
    variables: &mut Variables,
    custom_functions: &mut CustomFunctions
) -> Result<Vec<Option<Expr>>> {
    let args = get_args(args, meta, variables, custom_functions)?;

    if args.len() < args_no {
        bail!(LineError {
            line: meta.line,
            msg: f!(
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
            msg: f!(
                "{}: Too many arguments ({}, expected: {})",
                func.as_ref(),
                args.len(),
                args_no
            )
        });
    }

    Ok(args)
}

fn say(args: Args, meta: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions) -> Result<()> {
    let args = get_args(args, meta, variables, custom_functions)?;

    for arg in args {
        print!("{}", to_string(meta, variables, custom_functions, arg)?);

        print!(" ")
    }

    println!();

    Ok(())
}

fn ask(args: Args, meta: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions) -> Result<&'static str> {
    let args = expect_args(args, 1, "ask", meta, variables, custom_functions)?;

    let binding = &args[0];

    let prompt = match binding {
        Some(Expr::String(s)) => s,
        _ => bail!("ask: Prompt must be a string"),
    };

    print!("{prompt}");

    stdout().flush()?;

    let mut inp = String::new();

    stdin().read_line(&mut inp)?;

    Ok(inp.trim().to_string().leak())
}

fn quit(args: Args, meta: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions) -> Result<()> {
    let args = expect_args(args, 1, "quit", meta, variables, custom_functions)?;

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
            Expr::Bool(_) => bail!("quit: Exit code must be an integer."),
            _ => unimplemented!(),
            // _ => bail!("quit: Exit code must be an integer."),
        },
        None => exit(0),
    };

    exit(code)
}

fn join(args: Args, meta: &Metadata, variables: &mut Variables, custom_functions: &mut CustomFunctions) -> Result<&'static str> {
    let args = get_args(args, meta, variables, custom_functions)?;

    if args.is_empty() {
        bail!("join: No arguments provided");
    }

    let mut joined = String::new();

    for arg in args {
        joined.push_str(&to_string(meta, variables, custom_functions, arg)?)
    }

    Ok(joined.leak())
}

fn type_(
    args: Args,
    meta: &Metadata,
    variables: &mut Variables,
    custom_functions: &mut CustomFunctions,
) -> Result<&'static str> {
    let args = expect_args(args, 1, "type", meta, variables, custom_functions)?;

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
            Expr::Bool(_) => "bool",
            Expr::Char(_) => "char",
            Expr::Array(_) => "array",
            _ => unimplemented!(),
        },
        None => "None",
    })
}
