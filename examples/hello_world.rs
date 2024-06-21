use std::fs::read_to_string;

use color_eyre::Result;
use orion_lib::{run_contents, encode, decode, run_ast, setup_error_hooks};

fn main() -> Result<()> {
    setup_error_hooks()?;

    run_contents(read_to_string("examples/hello.or")?, false)?;

    //#region JIT
    let j = encode(read_to_string("examples/hello.or")?, false, false)?;

    println!("{j}");

    let ast = decode(&j, false)?;

    println!("{ast:?}");
    //#endregion

    run_ast(ast)?;

    Ok(())
}
