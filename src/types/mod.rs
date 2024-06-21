//! The types.

use crate::utils::ast::Expr;

use crate::prelude::*;

pub(crate) mod numbers;
pub(crate) mod strings;
pub(crate) mod arrays;
pub(crate) mod bool;

pub trait Object {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>>;
}

pub struct ObjectHolder(pub Box<dyn Object>);