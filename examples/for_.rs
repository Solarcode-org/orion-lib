use std::fs::read_to_string;

use color_eyre::Result;
use orion_lib::run_contents;

fn main() -> Result<()> {
    run_contents(read_to_string("for_.or")?)?;

    Ok(())
}
