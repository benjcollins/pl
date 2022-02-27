use crate::token::{Token, TokenKind};

pub struct Lexer<'src> {
    src: &'src str,
    offset: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            offset: 0,
            src: source,
        }
    }
    fn token_here(&self, kind: TokenKind) -> Token {
        Token::new(self.offset, kind)
    }
    pub fn next(&mut self) -> Token {
        let token = loop {
            let ch = match self.src[self.offset..].chars().next() {
                Some(ch) => ch,
                None => return self.token_here(TokenKind::End),
            };
            match ch {
                ch if ch.is_alphabetic() => {
                    let ident_token = self.token_here(TokenKind::Ident);
                    break match ident_token.as_str(self.src) {
                        "let" => self.token_here(TokenKind::Let),
                        "fn" => self.token_here(TokenKind::Fn),
                        "return" => self.token_here(TokenKind::Return),
                        _ => ident_token,
                    };
                }
                ch if ch.is_numeric() => break self.token_here(TokenKind::Integer),
                ch if ch.is_whitespace() => self.offset += ch.len_utf8(),
                '(' => break self.token_here(TokenKind::OpenBrace),
                ')' => break self.token_here(TokenKind::CloseBrace),
                '{' => break self.token_here(TokenKind::OpenCurlyBrace),
                '}' => break self.token_here(TokenKind::CloseCurlyBrace),
                '+' => break self.token_here(TokenKind::Plus),
                '-' => break self.token_here(TokenKind::Minus),
                '*' => break self.token_here(TokenKind::Asterisk),
                '/' => break self.token_here(TokenKind::ForwardSlash),
                '=' => break self.token_here(TokenKind::Equals),
                ',' => break self.token_here(TokenKind::Comma),
                ':' => break self.token_here(TokenKind::Colon),
                _ => break self.token_here(TokenKind::Invalid),
            };
        };
        self.offset += token.len(self.src);
        token
    }
    pub fn src(&self) -> &str {
        self.src
    }
}

#[test]
fn test_lexer() {
    let source = "hello   7235   #hudw stop";
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next().as_str(source), "hello");
    assert_eq!(lexer.next().as_str(source), "7235");
    assert_eq!(lexer.next().as_str(source), "#hudw");
    assert_eq!(lexer.next().as_str(source), "stop");
    assert_eq!(lexer.next().kind, TokenKind::End);
}
