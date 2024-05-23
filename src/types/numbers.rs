//! The number types.

use crate::utils::ast::Expr;

pub trait Numeric<T> {
    fn new(num: T) -> Self;
    fn to_str(&self) -> Expr;
}

struct Uint8(u8);

impl<T: Into<u8>> Numeric<T> for Uint8 {
    fn new(num: T) -> Self {
        Self(num.into())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}