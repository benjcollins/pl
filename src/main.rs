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
    let fn_asts = parser::parse(&tokens, src).unwrap();
    let mut fn_mirs = vec![];
    for fn_ast in &fn_asts {
        if let Some(fn_mir) = compiler::compile_fun(fn_ast, &fn_asts) {
            println!("{}", fn_mir);
            fn_mirs.push(fn_mir);
        }
    }

    let file = File::create("output.ssa").unwrap();
    for fn_mir in &fn_mirs {
        qbe::compile_fun(fn_mir, &file).unwrap();
    }
    Command::new("qbe/obj/qbe").args(["output.ssa", "-o", "output.S"]).status().unwrap();
    Command::new("gcc").args(["-o", "output", "main.c", "output.S"]).status().unwrap();
    Command::new("./output").status().unwrap();
}
