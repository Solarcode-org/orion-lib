use crate::types::{Object, ObjectHolder};
use crate::utils::ast::Expr;

use crate::prelude::*;
use crate::utils::orion::{CustomFunctions, Metadata, Variables};

pub struct Array(Vec<ObjectHolder>);

impl Array {
    pub fn new(
        array: Vec<Expr>,
        meta: &Metadata,
        variables: &mut Variables,
        custom_functions: &mut CustomFunctions,
    ) -> Result<Self> {
        let mut self_array = vec![];

        for i in array  {
            self_array.push(match i.eval(meta, variables, custom_functions)? {
                Some(expr) => expr,
                None => bail!("None cannot be used in arrays.")
            }.to_methodical(meta, variables, custom_functions)?);
        }

        Ok(Self(self_array))
    }

    fn len(&self) -> Expr {
        Expr::Uint8(self.0.len() as u8)
    }
}

impl Object for Array {
    fn call(&self, f: &str, args: Vec<Expr>) -> Result<Option<Expr>> {
        match f {
            "len" => {
                if !args.is_empty() {
                    bail!("`len` accepts no arguments.")
                }

                Ok(Some(self.len()))
            }
            f => bail!("Could not find method `{f}` for type Array.")
        }
    }
}