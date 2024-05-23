//! Prelude.

pub use crate::error::Errors;

pub type Result<T> = color_eyre::Result<T>;

pub use std::format as f;

pub struct W<T>(pub T);