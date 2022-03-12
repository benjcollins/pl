use std::{fs, process::Command};

mod token;
mod lexer;
mod ast;
mod parser;
mod compiler;
mod mir;
mod mips;

fn main() {
    let src = include_str!("../example.txt");
    let tokens = lexer::lex(src);
    let fun_ast = parser::parse(&tokens, src).unwrap();
    let fun_mir = compiler::compile_fun(&fun_ast, src);

    let mut pre = include_str!("../pre.a").to_string();
    let asm = mips::compile_fun(&fun_mir);
    pre.push_str(&asm);
    fs::write("output.a", pre).unwrap();
    Command::new("spim").arg("-f").arg("output.a").status().unwrap();
}
