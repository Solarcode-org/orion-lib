//! The error module.

use thiserror::Error;

#[derive(Error, Debug)]
pub enum OrionErrors {
    #[error("line {line}: {msg}")]
    LineError { line: usize, msg: String },
}
