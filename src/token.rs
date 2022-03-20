#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub start: usize,
    pub end: usize,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Ident,
    Integer,

    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    OpenAngleBrace,
    CloseAngleBrace,

    Equals,
    Comma,
    Colon,
    Semicolon,
    Ampersand,

    Var,
    Fn,
    Return,
    If,
    Else,
    While,
    True,
    False,

    OpenBrace,
    CloseBrace,
    OpenCurlyBrace,
    CloseCurlyBrace,
}