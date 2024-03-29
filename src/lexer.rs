//! The lexer for Orion.

use logos::Logos;

/// The tokens for Orion.
#[derive(Debug, Logos, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"#\s*.*")]
pub enum Tokens {
    /// Boolean.
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    /// Curly brace open.
    #[token("{")]
    BraceOpen,

    /// Curly brace close.
    #[token("}")]
    BraceClose,

    /// Bracket open.
    #[token("[")]
    BracketOpen,

    /// Bracket close.
    #[token("]")]
    BracketClose,

    /// Parentheses open.
    #[token("(")]
    ParenOpen,

    /// Parentheses close.
    #[token(")")]
    ParenClose,

    /// Colon.
    #[token(":")]
    Colon,

    /// Comma.
    #[token(",")]
    Comma,

    /// None value.
    #[token("null")]
    None,

    /// Number (integer and float).
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?",
        |lex| lex.slice().parse::<f64>().unwrap())]
    Number(f64),

    /// String.
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#,
        |lex| lex.slice().replace('\"', "").to_owned())]
    String(String),

    /// Identifier a.k.a functions, variables, names, etc.
    #[regex("([A-Za-z_]+)", |lex| lex.slice().to_owned())]
    Ident(String),

    /// Equals.
    #[token("=")]
    Equals,

    /// Let Keyword
    #[token("let")]
    Let,

    /// Plus
    #[token("+")]
    Plus,

    /// Minus
    #[token("-")]
    Minus,

    /// Multiply
    #[token("*")]
    Multiply,

    /// Divide
    #[token("/")]
    Divide,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(line: &str, token: Tokens) {
        let mut lexer = Tokens::lexer(line);
        assert_eq!(
            lexer
                .next()
                .expect("No token found!")
                .expect("Token failed to be parsed!"),
            token
        );
    }

    #[test]
    fn check_bool() {
        check("true", Tokens::Bool(true));
        check("false", Tokens::Bool(false));
    }

    #[test]
    fn check_braces() {
        check("{", Tokens::BraceOpen);
        check("}", Tokens::BraceClose);
    }

    #[test]
    fn check_brackets() {
        check("[", Tokens::BracketOpen);
        check("]", Tokens::BracketClose);
    }

    #[test]
    fn check_parens() {
        check("(", Tokens::ParenOpen);
        check(")", Tokens::ParenClose);
    }

    #[test]
    fn check_comma() {
        check(",", Tokens::Comma);
    }

    #[test]
    fn check_colon() {
        check(":", Tokens::Colon);
    }

    #[test]
    fn check_none() {
        check("null", Tokens::None);
    }

    #[test]
    fn check_string() {
        check("\"Hello\"", Tokens::String("Hello".to_owned()));
    }

    #[test]
    fn check_number() {
        check("12.4", Tokens::Number(12.4));
    }

    #[test]
    fn check_ident() {
        check("foo", Tokens::Ident("foo".to_owned()));
    }

    #[test]
    fn check_equals() {
        check("=", Tokens::Equals);
    }

    #[test]
    fn check_let() {
        check("let", Tokens::Let);
    }

    #[test]
    fn check_plus() {
        check("+", Tokens::Plus);
    }
    #[test]
    fn check_minus() {
        check("-", Tokens::Minus);
    }
    #[test]
    fn check_multiply() {
        check("*", Tokens::Multiply);
    }
    #[test]
    fn check_divide() {
        check("/", Tokens::Divide);
    }
}
