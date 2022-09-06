use crate::token::{RawToken, Token};

pub struct Tokens<'s> {
    pub raw_tokens: Vec<RawToken>,
    pub source: &'s str,
}

impl<'s> Tokens<'s> {
    pub fn iter(&self) -> TokenIter<'_, 's> {
        TokenIter {
            raw_tokens: &self.raw_tokens,
            source: self.source,
            index: 0,
        }
    }
}

pub struct TokenIter<'t, 's> {
    raw_tokens: &'t Vec<RawToken>,
    source: &'s str,
    index: usize,
}

impl<'t, 's> Iterator for TokenIter<'t, 's> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.raw_tokens.len() {
            let raw_token = self.raw_tokens[self.index];
            self.index += 1;
            Some(Token {
                kind: raw_token.kind,
                offset: raw_token.offset,
                source: self.source,
            })
        } else {
            None
        }
    }
}
