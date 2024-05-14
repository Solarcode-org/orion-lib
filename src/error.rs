//! The error module.

use thiserror::Error;

#[derive(Error, Debug)]
pub enum OrionErrors {
    #[error("line {0}: {1}")]
    LineError(usize, String),
}
