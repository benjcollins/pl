use strum::IntoEnumIterator;

use crate::{
    token::{Keyword, RawToken, Symbol, TokenKind},
    tokens::Tokens,
};

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    pub offset: usize,
    pub source: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

pub fn lex<'s>(source: &'s str) -> Tokens<'s> {
    let mut lexer = Lexer { offset: 0, source };
    let mut raw_tokens = vec![];
    while let Some(token) = lexer.next_token() {
        raw_tokens.push(token);
    }
    Tokens { raw_tokens, source }
}

impl<'a> Lexer<'a> {
    fn eat_str(&mut self, str: &str) -> bool {
        if self.source[self.offset..].starts_with(str) {
            self.offset += str.len();
            true
        } else {
            false
        }
    }
    fn eat_if(&mut self, f: impl Fn(char) -> bool) -> bool {
        let ch = self.source[self.offset..].chars().next();
        if ch.map_or(false, f) {
            self.offset += ch.map_or(0, |ch| ch.len_utf8());
            true
        } else {
            false
        }
    }
    fn eat_while(&mut self, f: impl Fn(char) -> bool + Copy) {
        loop {
            if !self.eat_if(f) {
                break;
            }
        }
    }
    pub fn next_token(&mut self) -> Option<RawToken> {
        loop {
            if self.offset >= self.source.len() {
                return None;
            }

            let offset = self.offset;

            if self.eat_str("//") {
                self.eat_while(|ch| ch != '\n');
                continue;
            }
            if self.eat_if(|ch| ch.is_whitespace()) {
                continue;
            }
            if self.eat_if(|ch| ch.is_numeric()) {
                self.eat_while(|ch| ch.is_numeric());
                return Some(RawToken {
                    kind: TokenKind::Integer,
                    offset,
                });
            }
            if self.eat_if(|ch| ch.is_alphabetic() || ch == '_') {
                self.eat_while(|ch| ch.is_alphanumeric() || ch == '_');
                for keyword in Keyword::iter() {
                    if keyword.str() == &self.source[offset..self.offset] {
                        return Some(RawToken {
                            kind: TokenKind::Keyword(keyword),
                            offset,
                        });
                    }
                }
                return Some(RawToken {
                    kind: TokenKind::Ident,
                    offset,
                });
            }
            for symbol in Symbol::iter() {
                if self.eat_str(symbol.str()) {
                    return Some(RawToken {
                        kind: TokenKind::Symbol(symbol),
                        offset,
                    });
                }
            }
            panic!("{}", &self.source[self.offset..])
        }
    }
}
