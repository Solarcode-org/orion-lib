//! The error module.

use thiserror::Error;

/// The errors.
#[derive(Error, Debug)]
pub enum OrionErrors {
    /// Error on a line `line` with message `msg`.
    #[error("line {line}: {msg}")]
    LineError { line: usize, msg: String },
}
