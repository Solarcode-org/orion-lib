use std::{
    fs::{read_to_string, write},
    path::Path,
};

use color_eyre::{
    eyre::{eyre, WrapErr},
    install, Result,
};
use rustlr::generate;

fn main() -> Result<()> {
    install()?;

    // Cache Script: Start

    let path = Path::new("grammar.cache");
    let grammar = read_to_string("grammar/orion.grammar")?;

    if path.exists() {
        let contents = read_to_string(path)?;

        if contents == grammar {
            return Ok(());
        }
    }

    // Cache Script: End

    let report = generate("workaround grammar/orion.grammar -o src/lrparser.rs -trace 0 -lr1")
        .map_err(|e| eyre!(e))
        .with_context(|| "Could not generate parser: `parser.rs`")?;

    eprintln!("{report}");

    // Write cache.
    write("grammar.cache", grammar)?;

    Ok(())
}
