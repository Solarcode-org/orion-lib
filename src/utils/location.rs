use std::fmt::{Debug, Display, Formatter};

pub(crate) fn location<S: AsRef<str>>(location: usize, contents: S) -> Position {
    let mut line = 1;
    let mut loc = 1;

    for (count, tok) in contents.as_ref().chars().enumerate() {
        if (count + 1) == location {
            break;
        }

        if tok == '\n' {
            line += 1;
            loc = 1;
            continue;
        }

        loc += 1;
    }

    Position {
        line,
        index: loc,
    }
}

pub(crate) struct Position {
    line: usize,
    index: usize,
}

impl Debug for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "line: {}, index: {}", self.line, self.index
        )
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "line: {}, index: {}", self.line, self.index
        )
    }
}