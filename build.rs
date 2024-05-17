use std::process::Command;

use color_eyre::{
    eyre::{eyre, WrapErr},
    install, Result,
};

fn main() -> Result<()> {
    install()?;

    Command::new("sh").arg("-c").arg("cd").arg("src").output()?;

    lalrpop::process_root()
        .map_err(|e| eyre!(e.to_string()))
        .with_context(|| "Could not process all the files in the current directory.")?;

    Command::new("sh").arg("-c").arg("cd").arg("..").output()?;

    Ok(())
}
