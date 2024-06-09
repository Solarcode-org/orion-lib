use crate::types::Object;
use crate::utils::ast::Expr;

use crate::prelude::*;

#[allow(dead_code)]
pub struct Bool(bool);

impl Bool {
    pub fn new(b: bool) -> Self {
        Self(b)
    }
}

impl Object for Bool {
    fn call(&self, _f: &str, _args: Vec<Expr>) -> Result<Option<Expr>> {
        Ok(None)
    }
}