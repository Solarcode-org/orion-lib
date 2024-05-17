//! The Abstract Syntax Tree (AST) for Orion.

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
}
