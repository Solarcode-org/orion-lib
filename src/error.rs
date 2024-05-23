//! The error module.

use thiserror::Error;

/// The errors.
#[derive(Error, Debug)]
pub enum Errors {
    /// Error on a line `line` with message `msg`.
    #[error("line {line}: {msg}")]
    LineError { line: usize, msg: String },

    /// A general error.
    #[error("{0}")]
    GeneralError(String),
}
