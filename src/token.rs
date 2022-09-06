use strum::EnumIter;

use crate::lexer::{Lexer, Position};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub offset: usize,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Ident,
    Integer,
    Keyword(Keyword),
    Symbol(Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub enum Keyword {
    Func,
    Struct,

    Var,
    If,
    Else,
    While,
    Return,

    True,
    False,

    I32,
    I16,
    I8,
    U32,
    U16,
    U8,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub enum Symbol {
    Arrow,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    OpenBrace,
    CloseBrace,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenAngleBrace,
    CloseAngleBrace,
    Semicolon,
    Colon,
    Dot,
    Ampersand,
    Comma,
    Equals,
}

impl Keyword {
    pub fn str(&self) -> &str {
        match self {
            Keyword::Func => "func",
            Keyword::Var => "var",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::I32 => "i32",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Return => "return",
            Keyword::Struct => "struct",
            Keyword::I16 => "i16",
            Keyword::I8 => "i8",
            Keyword::U32 => "u32",
            Keyword::U16 => "u16",
            Keyword::U8 => "u8",
            Keyword::Bool => "bool",
        }
    }
}

impl Symbol {
    pub fn str(&self) -> &str {
        match self {
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Asterisk => "*",
            Symbol::ForwardSlash => "/",
            Symbol::OpenBrace => "(",
            Symbol::CloseBrace => ")",
            Symbol::OpenCurlyBrace => "{",
            Symbol::CloseCurlyBrace => "}",
            Symbol::Semicolon => ";",
            Symbol::Colon => ":",
            Symbol::Arrow => "->",
            Symbol::Dot => ".",
            Symbol::Ampersand => "&",
            Symbol::Comma => ",",
            Symbol::OpenAngleBrace => "<",
            Symbol::CloseAngleBrace => ">",
            Symbol::Equals => "=",
        }
    }
}

impl Token {
    pub fn pos(&self, source: &str) -> Position {
        let mut line = 1;
        let mut column = 1;
        for (offset, ch) in source.char_indices() {
            if offset == self.offset {
                return Position { line, column };
            }
            if ch == '\n' {
                column = 0;
                line += 1;
            }
            column += 1;
        }
        unreachable!()
    }
    pub fn len<'a>(&self, source: &str) -> usize {
        let mut lexer = Lexer {
            offset: self.offset,
            source,
        };
        lexer.next_token();
        lexer.offset - self.offset
    }
    pub fn str<'a>(&self, source: &'a str) -> &'a str {
        &source[self.offset..self.offset + self.len(source)]
    }
}
