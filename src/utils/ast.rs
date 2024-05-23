//! The Abstract Syntax Tree (AST) for Orion.

use std::ops::{Add, Div, Mul, Sub};
use color_eyre::eyre::bail;
use crate::run;
use crate::utils::orion::{Metadata, Variables};
use crate::prelude::*;

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

    /// Used to describe an identifier (variable).
    Ident(String),

    /// The addition expression.
    Add(Box<Expr>, Box<Expr>),

    /// The subtraction expression.
    Subtract(Box<Expr>, Box<Expr>),

    /// The multiplication expression.
    Multiply(Box<Expr>, Box<Expr>),

    /// The division expression.
    Divide(Box<Expr>, Box<Expr>),

    /// The function call expression.
    FuncCall(String, Vec<Expr>),

    /// The variable creation expression.
    Let(String, Box<Expr>),

    /// An if statement.
    If(Box<Expr>, Box<Expr>),

    /// An if-else statement.
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// A scope.
    Scope(Vec<Expr>),

    /// Greater than.
    GreaterThan(Box<Expr>, Box<Expr>),

    /// Lesser than.
    LesserThan(Box<Expr>, Box<Expr>),

    /// Strictly equals.
    StrictlyEquals(Box<Expr>, Box<Expr>),

    /// Not equals
    NotEquals(Box<Expr>, Box<Expr>),

    /// Greater than or strictly equals.
    GreaterThanOrStrictlyEquals(Box<Expr>, Box<Expr>),

    /// Lesser than or strictly equals.
    LesserThanOrStrictlyEquals(Box<Expr>, Box<Expr>),

    /// Get a property.
    Property(Box<Expr>, String),

    /// Call a function.
    Method(Box<Expr>, String, Vec<Expr>)
}

impl Expr {
    pub fn eval(self, meta: &Metadata, variables: &mut Variables) -> Result<Option<Expr>> {
        run(self, meta, variables)
    }
}

impl Add for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a + b))),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a + b))),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a + b))),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a + b))),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a + b))),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a + b))),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a + b))),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a + b))),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("add: Cannot use \
            strings in operations. Did you mean to concatenate the strings? `join(s1, s2)`"),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("add: Cannot use \
            booleans in operations. Did you mean to use an AND expression? `b1 and b2`"),

            (None, _) | (_, None) => bail!("add: Cannot use None in arithmetic"),

            _ => {
                bail!("add: Cannot perform arithmetic between two different types.")
            }
        }
    }
}

impl Sub for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a - b))),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a - b))),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a - b))),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a - b))),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a - b))),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a - b))),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a - b))),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a - b))),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("subtract: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("subtract: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("subtract: Cannot use None in arithmetic"),

            _ => {
                bail!("subtract: Cannot perform arithmetic between two different types.")
            }
        }
    }
}

impl Mul for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a * b))),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a * b))),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a * b))),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a * b))),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a * b))),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a * b))),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a * b))),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a * b))),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("multiply: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("multiply: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("multiply: Cannot use None in arithmetic"),

            _ => {
                bail!("multiply: Cannot perform arithmetic between two different types.")
            }
        }
    }
}

impl Div for W<Option<Expr>> {
    type Output = Result<Option<Expr>>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (Some(Expr::Int8(a)), Some(Expr::Int8(b))) => Ok(Some(Expr::Int8(a / b))),
            (Some(Expr::Int16(a)), Some(Expr::Int16(b))) => Ok(Some(Expr::Int16(a / b))),
            (Some(Expr::Int32(a)), Some(Expr::Int32(b))) => Ok(Some(Expr::Int32(a / b))),
            (Some(Expr::Int64(a)), Some(Expr::Int64(b))) => Ok(Some(Expr::Int64(a / b))),
            (Some(Expr::Uint8(a)), Some(Expr::Uint8(b))) => Ok(Some(Expr::Uint8(a / b))),
            (Some(Expr::Uint16(a)), Some(Expr::Uint16(b))) => Ok(Some(Expr::Uint16(a / b))),
            (Some(Expr::Uint32(a)), Some(Expr::Uint32(b))) => Ok(Some(Expr::Uint32(a / b))),
            (Some(Expr::Uint64(a)), Some(Expr::Uint64(b))) => Ok(Some(Expr::Uint64(a / b))),
            (Some(Expr::String(_)), Some(Expr::String(_))) => bail!("divide: Cannot use \
            strings in operations."),
            (Some(Expr::Bool(_)), Some(Expr::Bool(_))) => bail!("divide: Cannot use \
            booleans in operations."),

            (None, _) | (_, None) => bail!("divide: Cannot use None in arithmetic"),

            _ => {
                bail!("divide: Cannot perform arithmetic between two different types.")
            }
        }
    }
}
