//! The error module.

use std::process::exit;

/// Simple error on a line.
pub fn error<T: ToString>(msg: T, line: usize) -> ! {
    eprintln!("Error on line {}: {}!", line, msg.to_string());
    exit(1);
}

/// Tries result.
pub fn try_error<T>(res: Result<T, String>, line: usize) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error on line {}: {}!", line, e);
            exit(1);
        }
    }
}

/// Tries result. (io)
pub fn _io_try_error<T>(res: std::io::Result<T>, line: usize) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error on line {}: {}!", line, e);
            exit(1);
        }
    }
}
