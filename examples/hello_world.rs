use std::fs::read_to_string;

use anyhow::Result;
use orion_lib::run_contents;

fn main() -> Result<()> {
    run_contents(read_to_string("./examples/calc.or")?)?;

    Ok(())
}
