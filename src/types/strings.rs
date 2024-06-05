use crate::types::Object;
use crate::utils::ast::Expr;

use crate::prelude::*;

pub struct Str(String);

impl Str {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    fn chars(&self) -> Expr {
        let mut ch_list = vec![];

        for c in self.0.chars() {
            ch_list.push(Expr::Char(c));
        }

        Expr::Array(ch_list)
    }
}

impl Object for Str {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "chars" => {
                if !args.is_empty() {
                    bail!("`chars` accepts no arguments.")
                }

                Ok(Some(self.chars()))
            }
            f => bail!("Could not find method `{f}` for type String.")
        }
    }
}