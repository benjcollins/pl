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
    Dot,

    Var,
    Func,
    Return,
    If,
    Else,
    While,
    True,
    False,
    Struct,

    OpenBrace,
    CloseBrace,
    OpenCurlyBrace,
    CloseCurlyBrace,
}

impl Token {
    pub fn as_str<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start..self.end]
    }
}