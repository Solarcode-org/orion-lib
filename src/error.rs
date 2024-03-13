//! The error module.

/// Simple error on a line.
pub fn error<T: ToString>(msg: T, line: usize) -> ! {
    panic!("Error on line {}: {}!", line, msg.to_string());
}

/// Tries result.
pub fn try_error<T>(res: Result<T, String>, line: usize) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            panic!("Error on line {}: {}!", line, e);
        }
    }
}

/// Tries result. (io)
pub fn _io_try_error<T>(res: std::io::Result<T>, line: usize) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            panic!("Error on line {}: {}!", line, e);
        }
    }
}
