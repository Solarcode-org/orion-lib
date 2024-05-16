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

    FuncCall(String, Vec<Expr>),

    Let(String, Box<Expr>),
}
