use strum::EnumIter;

use crate::lexer::{Lexer, Position};

#[derive(Debug, Clone, Copy)]
pub struct Token<'s> {
    pub source: &'s str,
    pub offset: usize,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy)]
pub struct RawToken {
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

impl<'s> Token<'s> {
    pub fn pos(&self) -> Position {
        let mut line = 1;
        let mut column = 1;
        for (offset, ch) in self.source.char_indices() {
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
    pub fn len(&self) -> usize {
        let mut lexer = Lexer {
            offset: self.offset,
            source: self.source,
        };
        lexer.next_token();
        lexer.offset - self.offset
    }
    pub fn str(&self) -> &'s str {
        &self.source[self.offset..self.offset + self.len()]
    }
}
