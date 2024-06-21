//! The number types.

use crate::prelude::*;
use crate::types::Object;

use crate::utils::ast::Expr;

pub trait Numeric: Object {
    fn new(num: i64) -> Self;
    fn to_str(&self) -> Expr;
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Uint8(u8);

#[derive(Debug, Clone, Copy, Default)]
pub struct Uint16(u16);

#[derive(Debug, Clone, Copy, Default)]
pub struct Uint32(u32);

#[derive(Debug, Clone, Copy, Default)]
pub struct Uint64(u64);

#[derive(Debug, Clone, Copy, Default)]
pub struct Int8(i8);

#[derive(Debug, Clone, Copy, Default)]
pub struct Int16(i16);

#[derive(Debug, Clone, Copy, Default)]
pub struct Int32(i32);

#[derive(Debug, Clone, Copy, Default)]
pub struct Int64(i64);

impl Object for Uint8 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Uint8 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type u8")
        }
    }
}
impl Object for Uint16 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Uint16 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type u16")
        }
    }
}
impl Object for Uint32 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }

                Ok(Some(<Uint32 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type u32")
        }
    }
}
impl Object for Uint64 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Uint64 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type u64")
        }
    }
}

impl Object for Int8 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Int8 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type i8")
        }
    }
}
impl Object for Int16 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Int16 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type i16")
        }
    }
}
impl Object for Int32 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }

                Ok(Some(<Int32 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type i32")
        }
    }
}
impl Object for Int64 {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(<Int64 as Numeric>::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type i64")
        }
    }
}

impl Numeric for Uint8 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Uint16 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Uint32 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Uint64 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Int8 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Int16 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Int32 {
    fn new(num: i64) -> Self {
        Self(num.try_into().unwrap())
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Numeric for Int64 {
    fn new(num: i64) -> Self {
        Self(num)
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

pub struct Float(f32);

impl Float {
    pub fn new(f: f32) -> Self {
        Self(f)
    }

    fn to_str(&self) -> Expr {
        Expr::String(self.0.to_string())
    }
}

impl Object for Float {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "to_str" => {
                if !args.is_empty() {
                    bail!("`to_str` accepts no arguments.")
                }
                Ok(Some(Float::to_str(self)))
            },
            f => bail!("Could not find method `{f}` for type f32")
        }
    }
}
