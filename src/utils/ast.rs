//! The Abstract Syntax Tree (AST) for Orion.

use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Sub};
use crate::run;
use crate::utils::orion::{CustomFunctions, Metadata, Variables};
use crate::prelude::*;
use crate::types::arrays::Array;
use crate::types::bool::Bool;
use crate::types::numbers::{Numeric, Uint16, Uint8, Uint32, Uint64};
use crate::types::ObjectHolder;
use crate::types::strings::Str;

/// The Abstract Syntax Tree (AST) for Orion.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// The i8 type. (8-bit signed integer)
    Int8(i8),

    /// The i16 type. (16-bit signed integer)
    Int16(i16),

    /// The i32 type. (32-bit signed integer)
    Int32(i32),

    /// The i64 type. (64-bit signed integer)
    Int64(i64),

    /// The u8 type. (8-bit unsigned integer)
    Uint8(u8),

    /// The u16 type. (16-bit unsigned integer)
    Uint16(u16),

    /// The u32 type. (32-bit unsigned integer)
    Uint32(u32),

    /// The u64 type. (64-bit unsigned integer)
    Uint64(u64),

    /// The String type.
    String(String),

    /// The Boolean type.
    Bool(bool),

    /// Character.
    Char(char),

    /// Used to describe an identifier (variable).
    Ident(String),

    /// Array type.
    Array(Vec<Expr>),

    // /// The addition expression.
    // Add(Box<Expr>, Box<Expr>),
    //
    // /// The subtraction expression.
    // Subtract(Box<Expr>, Box<Expr>),
    //
    // /// The multiplication expression.
    // Multiply(Box<Expr>, Box<Expr>),
    //
    // /// The division expression.
    // Divide(Box<Expr>, Box<Expr>),

    /// Mathematical Operations.
    Op(Box<Expr>, OpCode, Box<Expr>),

    /// Logical Comparison.
    Compare(Box<Expr>, CompCode, Box<Expr>),

    /// The function call expression.
    FuncCall(String, Vec<Expr>),

    /// The variable creation expression.
    Let(String, Box<Expr>),

    /// The **typed** variable creation expression.
    TypeLet(Type, String, Box<Expr>),

    /// Re-assign an existing variable.
    Reassign(String, ReassignCode, Box<Expr>),

    /// An if statement.
    If(Box<Expr>, Box<Expr>),

    /// An if-else statement.
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// A scope.
    Scope(Vec<Option<Expr>>),

    /// Get a property.
    Property(Box<Expr>, String),

    /// Call a function.
    Method(Box<Expr>, String, Vec<Expr>),

    /// For loop.
    For(String, Box<Expr>, Box<Expr>),

    /// Complex For loop.
    ForComplex(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
    
    /// Slice notation.
    Slice(Box<Expr>, Box<Expr>),

    /// Function creation.
    Func(String, Vec<String>, Box<Expr>)
}

impl Expr {
    pub fn eval(
        self,
        meta: &Metadata,
        variables: &mut Variables,
        custom_functions: &mut CustomFunctions,
    ) -> Result<Option<Expr>> {
        run(Some(self), meta, variables, custom_functions)
    }

    pub fn to_methodical(
        &self,
        meta: &Metadata,
        variables: &mut Variables,
        custom_functions: &mut CustomFunctions) -> Result<ObjectHolder>
    {
        Ok(ObjectHolder(match self {
                Expr::Int8(_n) => /*Uint8::new(*_n)*/todo!(),
                Expr::Int16(_n) => /*Uint8::new(*_n)*/todo!(),
                Expr::Int32(_n) => /*Uint8::new(*_n)*/todo!(),
                Expr::Int64(_n) => /*Uint8::new(*_n)*/todo!(),
                Expr::Uint8(n) => Box::new(Uint8::new((*n).into())),
                Expr::Uint16(n) => Box::new(Uint16::new((*n).into())),
                Expr::Uint32(n) => Box::new(Uint32::new((*n).into())),
                Expr::Uint64(n) => Box::new(Uint64::new((*n).try_into().with_context(
                    || "Could not convert number to correct type."
                )?)),
                Expr::String(s) => Box::new(Str::new(s.to_string())),
                Expr::Bool(b) => Box::new(Bool::new(*b)),
                Expr::Array(array) => Box::new(
                    Array::new(array.to_vec(), meta, variables, custom_functions)?
                ),
                _ => unimplemented!()
            }
        ))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpCode {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CompCode {
    Greater,
    Lesser,
    Equals,
    NotEquals,
    GreaterEquals,
    LesserEquals,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ReassignCode {
    Re,
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    /// The i8 type. (8-bit signed integer)
    Int8,

    /// The i16 type. (16-bit signed integer)
    Int16,

    /// The i32 type. (32-bit signed integer)
    Int32,

    /// The i64 type. (64-bit signed integer)
    Int64,

    /// The u8 type. (8-bit unsigned integer)
    Uint8,

    /// The u16 type. (16-bit unsigned integer)
    Uint16,

    /// The u32 type. (32-bit unsigned integer)
    Uint32,

    /// The u64 type. (64-bit unsigned integer)
    Uint64,

    /// The String type.
    String,

    /// The Boolean type.
    Bool,

    /// Character.
    Char,

    /// Array.
    Array(Box<Option<Type>>),

    /// Dynamic Integer.
    DynInt
}

impl Add for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn add(self, rhs: Self) -> Self::Output {
        let (a, b): (i128, i128) = match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => (a.into(), b.into()),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => (a.into(), b.into()),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => (a.into(), b.into()),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => (a.into(), b.into()),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => (a.into(), b.into()),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => (a.into(), b.into()),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => (a.into(), b.into()),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => (a.into(), b.into()),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("add: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("add: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("add: Cannot use None in arithmetic"),

            _ => {
                bail!("add: Cannot perform arithmetic between two different types.")
            }
        };

        let sum = a + b;

        if let Ok(n) = u8::try_from(sum) {
            Ok(Some(Expr::Uint8(n)))
        } else if let Ok(n) = u16::try_from(sum) {
            Ok(Some(Expr::Uint16(n)))
        } else if let Ok(n) = u32::try_from(sum) {
            Ok(Some(Expr::Uint32(n)))
        } else if let Ok(n) = u64::try_from(sum) {
            Ok(Some(Expr::Uint64(n)))
        } else if let Ok(n) = i8::try_from(sum) {
            Ok(Some(Expr::Int8(n)))
        } else if let Ok(n) = i16::try_from(sum) {
            Ok(Some(Expr::Int16(n)))
        } else if let Ok(n) = i32::try_from(sum) {
            Ok(Some(Expr::Int32(n)))
        } else if let Ok(n) = i64::try_from(sum) {
            Ok(Some(Expr::Int64(n)))
        } else {
            bail!("add: Difference is too big")
        }
    }
}

impl Sub for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn sub(self, rhs: Self) -> Self::Output {
        let (a, b): (i128, i128) = match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => (a.into(), b.into()),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => (a.into(), b.into()),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => (a.into(), b.into()),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => (a.into(), b.into()),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => (a.into(), b.into()),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => (a.into(), b.into()),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => (a.into(), b.into()),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => (a.into(), b.into()),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("subtract: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("subtract: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("subtract: Cannot use None in arithmetic"),

            _ => {
                bail!("subtract: Cannot perform arithmetic between two different types.")
            }
        };

        let difference = a - b;

        if let Ok(n) = u8::try_from(difference) {
            Ok(Some(Expr::Uint8(n)))
        } else if let Ok(n) = u16::try_from(difference) {
            Ok(Some(Expr::Uint16(n)))
        } else if let Ok(n) = u32::try_from(difference) {
            Ok(Some(Expr::Uint32(n)))
        } else if let Ok(n) = u64::try_from(difference) {
            Ok(Some(Expr::Uint64(n)))
        } else if let Ok(n) = i8::try_from(difference) {
            Ok(Some(Expr::Int8(n)))
        } else if let Ok(n) = i16::try_from(difference) {
            Ok(Some(Expr::Int16(n)))
        } else if let Ok(n) = i32::try_from(difference) {
            Ok(Some(Expr::Int32(n)))
        } else if let Ok(n) = i64::try_from(difference) {
            Ok(Some(Expr::Int64(n)))
        } else {
            bail!("subtract: Difference is too big")
        }
    }
}

impl Mul for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn mul(self, rhs: Self) -> Self::Output {
        let (a, b): (i128, i128) = match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => (a.into(), b.into()),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => (a.into(), b.into()),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => (a.into(), b.into()),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => (a.into(), b.into()),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => (a.into(), b.into()),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => (a.into(), b.into()),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => (a.into(), b.into()),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => (a.into(), b.into()),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("multiply: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("multiply: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("multiply: Cannot use None in arithmetic"),

            _ => {
                bail!("multiply: Cannot perform arithmetic between two different types.")
            }
        };

        let product = a * b;

        if let Ok(n) = u8::try_from(product) {
            Ok(Some(Expr::Uint8(n)))
        } else if let Ok(n) = u16::try_from(product) {
            Ok(Some(Expr::Uint16(n)))
        } else if let Ok(n) = u32::try_from(product) {
            Ok(Some(Expr::Uint32(n)))
        } else if let Ok(n) = u64::try_from(product) {
            Ok(Some(Expr::Uint64(n)))
        } else if let Ok(n) = i8::try_from(product) {
            Ok(Some(Expr::Int8(n)))
        } else if let Ok(n) = i16::try_from(product) {
            Ok(Some(Expr::Int16(n)))
        } else if let Ok(n) = i32::try_from(product) {
            Ok(Some(Expr::Int32(n)))
        } else if let Ok(n) = i64::try_from(product) {
            Ok(Some(Expr::Int64(n)))
        } else {
            bail!("multiply: Difference is too big")
        }
    }
}

impl Div for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn div(self, rhs: Self) -> Self::Output {
        let (a, b): (i128, i128) = match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => (a.into(), b.into()),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => (a.into(), b.into()),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => (a.into(), b.into()),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => (a.into(), b.into()),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => (a.into(), b.into()),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => (a.into(), b.into()),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => (a.into(), b.into()),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => (a.into(), b.into()),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("divide: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("divide: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("divide: Cannot use None in arithmetic"),

            _ => {
                bail!("divide: Cannot perform arithmetic between two different types.")
            }
        };

        let quotient = a / b;

        if let Ok(n) = u8::try_from(quotient) {
            Ok(Some(Expr::Uint8(n)))
        } else if let Ok(n) = u16::try_from(quotient) {
            Ok(Some(Expr::Uint16(n)))
        } else if let Ok(n) = u32::try_from(quotient) {
            Ok(Some(Expr::Uint32(n)))
        } else if let Ok(n) = u64::try_from(quotient) {
            Ok(Some(Expr::Uint64(n)))
        } else if let Ok(n) = i8::try_from(quotient) {
            Ok(Some(Expr::Int8(n)))
        } else if let Ok(n) = i16::try_from(quotient) {
            Ok(Some(Expr::Int16(n)))
        } else if let Ok(n) = i32::try_from(quotient) {
            Ok(Some(Expr::Int32(n)))
        } else if let Ok(n) = i64::try_from(quotient) {
            Ok(Some(Expr::Int64(n)))
        } else {
            bail!("divide: Difference is too big")
        }
    }
}

impl Eq for W<Option<Expr>> {}

impl PartialEq<Self> for W<Option<Expr>> {
    fn eq(&self, other: &Self) -> bool {
        let a = &self.0;
        let b = &other.0;

        match (a, b) {
            (Some(a), Some(b)) => match (a, b) {
                (Expr::Int8(i), Expr::Int8(j)) => i == j,
                (Expr::Int16(i), Expr::Int16(j)) => i == j,
                (Expr::Int32(i), Expr::Int32(j)) => i == j,
                (Expr::Int64(i), Expr::Int64(j)) => i == j,
                (Expr::Uint8(i), Expr::Uint8(j)) => i == j,
                (Expr::Uint16(i), Expr::Uint16(j)) => i == j,
                (Expr::Uint32(i), Expr::Uint32(j)) => i == j,
                (Expr::Uint64(i), Expr::Uint64(j)) => i == j,
                (Expr::String(a), Expr::String(b)) => a == b,
                (Expr::Bool(a), Expr::Bool(b)) => {
                    eprintln!(
                        "\x1b[93mWarning: Redundant comparison.\x1b[0m"
                    );
                    a == b
                },
                _ => panic!("\x1b[31Cannot compare between different types.\x1b[0m"),
            },
            (Some(_), None) => panic!("\x1b[31Cannot compare between different types.\x1b[0m"),
            (None, None) => {
                eprintln!(
                    "\x1b[93mWarning: Consider using `is_none(some_v)` for `none` checks\x1b[0m"
                );
                true
            },
            (None, Some(_)) => panic!("\x1b[31Cannot compare between different types.\x1b[0m")
        }
    }
}

impl PartialOrd<Self> for W<Option<Expr>> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = &self.0;
        let b = &other.0;

        match (a, b) {
            (Some(a), Some(b)) => {
                let na: Option<i128> = match a {
                    Expr::Int8(i) => Some((*i).into()),
                    Expr::Int16(i) => Some((*i).into()),
                    Expr::Int32(i) => Some((*i).into()),
                    Expr::Int64(i) => Some((*i).into()),
                    Expr::Uint8(i) => Some((*i).into()),
                    Expr::Uint16(i) => Some((*i).into()),
                    Expr::Uint32(i) => Some((*i).into()),
                    Expr::Uint64(i) => Some((*i).into()),
                    _ => None,
                };

                let nb: Option<i128> = match b {
                    Expr::Int8(i) => Some((*i).into()),
                    Expr::Int16(i) => Some((*i).into()),
                    Expr::Int32(i) => Some((*i).into()),
                    Expr::Int64(i) => Some((*i).into()),
                    Expr::Uint8(i) => Some((*i).into()),
                    Expr::Uint16(i) => Some((*i).into()),
                    Expr::Uint32(i) => Some((*i).into()),
                    Expr::Uint64(i) => Some((*i).into()),
                    _ => None,
                };

                if let (Some(a), Some(b)) = (na, nb) {
                    return Some(a.cmp(&b));
                }

                match (a, b) {
                    (Expr::String(_), Expr::String(_)) => {
                        eprintln!("\x1b[31Cannot compare strings.\
                        Maybe you meant to compare their length? \
                        `len(s1) > len(s2)`\x1b[0m");
                        None
                    }

                    (Expr::Bool(_), Expr::Bool(_)) => {
                        eprintln!("\x1b[31mError: Cannot compare booleans. \
                        What are you trying to achieve here 🤔?\x1b[0m");
                        None
                    }
                    _ => {
                        eprintln!("\x1b[31mError: Cannot compare between different types.\x1b[0m");
                        None
                    },
                }
            },
            (Some(_), None) => {
                eprintln!("\x1b[31mError: Cannot compare between different types.\x1b[0m");
                None
            },
            (None, None) => {
                eprintln!(
                    "\x1b[93mError: Cannot compare `None` values\x1b[0m"
                );
                None
            },
            (None, Some(_)) => {
                eprintln!(
                    "\x1b[93mError: Cannot compare `None` values\x1b[0m"
                );
                None
            }
        }
    }
}
