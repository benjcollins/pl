#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub offset: usize,
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

    Let,
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

    Invalid,
    End,
}

fn next_while(source: &str, f: impl Fn(char) -> bool) -> usize {
    let mut len = 0;
    loop {
        let ch = match source[len..].chars().next() {
            Some(ch) if f(ch) => ch,
            _ => break,
        };
        len += ch.len_utf8();
    }
    len
}

impl Token {
    pub fn new(offset: usize, kind: TokenKind) -> Token {
        Token { offset, kind }
    }
    pub fn len(&self, source: &str) -> usize {
        match self.kind {
            TokenKind::Ident => next_while(&source[self.offset..], |ch| ch.is_alphanumeric()),
            TokenKind::Integer => next_while(&source[self.offset..], |ch| ch.is_numeric()),
            TokenKind::Invalid => next_while(&source[self.offset..], |ch| !ch.is_whitespace()),
            TokenKind::OpenBrace => "(".len(),
            TokenKind::CloseBrace => ")".len(),
            TokenKind::OpenCurlyBrace => "{".len(),
            TokenKind::CloseCurlyBrace => "}".len(),
            TokenKind::Comma => ",".len(),
            TokenKind::Colon => ":".len(),
            TokenKind::Semicolon => ";".len(),
            TokenKind::Plus => "+".len(),
            TokenKind::Minus => "-".len(),
            TokenKind::Asterisk => "*".len(),
            TokenKind::ForwardSlash => "/".len(),
            TokenKind::Equals => "=".len(),
            TokenKind::OpenAngleBrace => "<".len(),
            TokenKind::CloseAngleBrace => ">".len(),
            TokenKind::Let => "let".len(),
            TokenKind::Fn => "fn".len(),
            TokenKind::True => "true".len(),
            TokenKind::False => "false".len(),
            TokenKind::If => "if".len(),
            TokenKind::Else => "else".len(),
            TokenKind::Return => "return".len(),
            TokenKind::While => "while".len(),
            TokenKind::End => 0,
        }
    }
    pub fn as_str<'src>(&self, source: &'src str) -> &'src str {
        &source[self.offset..self.offset + self.len(source)]
    }
}
