use std::{fs, process::Command};

use parser::Parser;

use crate::{lexer::Lexer, token::TokenKind};

mod token;
mod lexer;
mod ast;
mod parser;
mod compiler;
mod mir;
mod mips;

fn _print_tokens(src: &str) {
    let mut lexer = Lexer::new(src);
    let mut token = lexer.next();
    while token.kind != TokenKind::End {
        println!("{:?} : '{}'", token.kind, token.as_str(src));
        token = lexer.next();
    }
}

fn main() {
    let src = include_str!("../example.txt");
    let mut parser = Parser::new(src);
    let fun_ast = parser.parse_fn();
    let fun_mir = compiler::compile_fun(&fun_ast, src);
    // println!("{:#?}", fun_ast);

    let mut pre = include_str!("../pre.a").to_string();
    let asm = mips::compile_fun(&fun_mir);
    pre.push_str(&asm);
    fs::write("output.a", pre).unwrap();
    Command::new("spim").arg("-f").arg("output.a").status().unwrap();
}
