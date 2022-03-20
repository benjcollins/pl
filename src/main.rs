use std::{fs::File, process::Command};

mod token;
mod lexer;
mod ast;
mod parser;
mod ty;
mod infer;
mod compiler;
mod mir;
mod qbe;

fn main() {
    let src = include_str!("../example.txt");
    let tokens = lexer::lex(src);
    let fun_ast = parser::parse(&tokens).unwrap();
    let fun_mir = compiler::compile_fun(&fun_ast, src);

    println!("{}", fun_mir);

    let file = File::create("output.ssa").unwrap();
    qbe::compile_fun(&fun_mir, file).unwrap();
    Command::new("qbe/obj/qbe").args(["output.ssa", "-o", "output.S"]).status().unwrap();
    Command::new("gcc").args(["-o", "output", "main.c", "output.S"]).status().unwrap();
    Command::new("./output").status().unwrap();
}
