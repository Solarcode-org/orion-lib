#[derive(Debug, Clone)]
pub enum Expr {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),

    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),

    String(String),

    Ident(String),

    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),

    FuncCall(String, Vec<Expr>),

    Let(String, Box<Expr>),
}
