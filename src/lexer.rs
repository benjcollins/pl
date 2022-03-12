use crate::token::{Token, TokenKind};

pub fn lex<'a>(src: &'a str) -> Vec<Token> {
    let mut lexer = Lexer {
        offset: 0,
        tokens: vec![],
        src,
    };
    loop {
        let ch = match lexer.peek_char() {
            Some(ch) => ch,
            None => break,
        };
        match ch {
            ch if ch.is_whitespace() => lexer.next_while(|ch| ch.is_whitespace()),
            ch if ch.is_numeric() => {
                let offset = lexer.offset;
                lexer.next_while(|ch| ch.is_numeric());
                lexer.push_token(offset, TokenKind::Integer);
            }
            ch if ch.is_alphabetic() => {
                let offset = lexer.offset;
                lexer.next_while(|ch| ch.is_alphanumeric());
                let content = &lexer.src[offset..lexer.offset];
                let kind = match content {
                    "let" => TokenKind::Let,
                    "fn" => TokenKind::Fn,
                    "return" => TokenKind::Return,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    _ => TokenKind::Ident,
                };
                lexer.push_token(offset, kind);
            }
            '(' => lexer.single_char_token(TokenKind::OpenBrace),
            ')' => lexer.single_char_token(TokenKind::CloseBrace),
            '{' => lexer.single_char_token(TokenKind::OpenCurlyBrace),
            '}' => lexer.single_char_token(TokenKind::CloseCurlyBrace),
            '<' => lexer.single_char_token(TokenKind::OpenAngleBrace),
            '>' => lexer.single_char_token(TokenKind::CloseAngleBrace),
            '+' => lexer.single_char_token(TokenKind::Plus),
            '-' => lexer.single_char_token(TokenKind::Minus),
            '*' => lexer.single_char_token(TokenKind::Asterisk),
            '/' => lexer.single_char_token(TokenKind::ForwardSlash),
            '=' => lexer.single_char_token(TokenKind::Equals),
            ',' => lexer.single_char_token(TokenKind::Comma),
            ':' => lexer.single_char_token(TokenKind::Colon),
            ';' => lexer.single_char_token(TokenKind::Semicolon),
            '&' => lexer.single_char_token(TokenKind::Ampersand),
            _ => panic!(),
        }
    }
    lexer.tokens
}

struct Lexer<'a> {
    src: &'a str,
    offset: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn peek_char(&self) -> Option<char> {
        self.src[self.offset..].chars().next()
    }
    fn next(&mut self) {
        match self.peek_char() {
            Some(ch) => self.offset += ch.len_utf8(),
            None => panic!(),
        }
    }
    fn next_if(&mut self, cond: impl Fn(char) -> bool) -> bool {
        match self.peek_char() {
            Some(ch) => {
                if cond(ch) {
                    self.offset += ch.len_utf8();
                    true
                } else {
                    false
                }
            }
            None => false
        }
    }
    fn next_while(&mut self, cond: impl Fn(char) -> bool) {
        while self.next_if(&cond) {}
    }
    fn push_token(&mut self, start: usize, kind: TokenKind) {
        self.tokens.push(Token {
            start,
            end: self.offset,
            kind,
        })
    }
    fn single_char_token(&mut self, kind: TokenKind) {
        let offset = self.offset;
        self.next();
        self.push_token(offset, kind);
    }
}