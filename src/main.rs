use std::fs;

use infer::Context;
use symbol::SymbolTableBuilder;
use parser::Parser;

use crate::{lexer::Lexer, token::TokenKind};

mod token;
mod lexer;
mod ast;
mod parser;
mod symbol;
mod ty;
mod infer;
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
    let fun = parser.parse_fn();
    let symbols = SymbolTableBuilder::resolve(&fun, src);
    let (symbol_tys, return_ty) = Context::infer(&fun, &symbols, src);
    let fun = mir::Compiler::compile_fun(src, &symbols, &symbol_tys, &return_ty, &fun);
    let mut pre = include_str!("../pre.a").to_string();
    let asm = mips::compiler::Compiler::compile_fun(&fun);
    pre.push_str(&asm);
    fs::write("output.a", pre).unwrap();
}
