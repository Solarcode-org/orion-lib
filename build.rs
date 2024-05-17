use std::{
    fs::{read_to_string, write},
    path::Path,
    process::Command,
};

use color_eyre::{
    eyre::{eyre, WrapErr},
    install, Result,
};

fn main() -> Result<()> {
    install()?;

    // Cache Script: Start

    let path = Path::new("grammar.cache");
    let grammar = read_to_string("src/lrparser.lalrpop")
        .with_context(|| "Could not find grammar `src/lrparser.lalrpop`")?;

    if path.exists() {
        let contents = read_to_string(path)?;

        if contents == grammar {
            // return Ok(());
        }
    }

    // Cache Script: End

    Command::new("cd").arg("src").output()?;

    lalrpop::process_root()
        .map_err(|e| eyre!(e.to_string()))
        .with_context(|| "Could not process all the files in the current directory.")?;

    Command::new("cd").arg("..").output()?;

    // Write cache.
    write("grammar.cache", grammar)?;

    Ok(())
}
