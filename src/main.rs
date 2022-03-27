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
    let program = parser::parse(&tokens, src).unwrap();
    let mut func_mirs = vec![];
    for (name, func_ast) in &program.funcs {
        if let Some(func_mir) = compiler::compile_fun(name, func_ast, &program) {
            // println!("{}", func_mir);
            func_mirs.push(func_mir);
        }
    }

    let file = File::create("output.ssa").unwrap();
    for (name, structure) in &program.structs {
        qbe::compile_struct(name, structure, &program, &file).unwrap();
    }
    for func_mir in &func_mirs {
        qbe::compile_fun(func_mir, &file).unwrap();
    }
    Command::new("qbe/obj/qbe").args(["output.ssa", "-o", "output.S"]).status().unwrap();
    Command::new("gcc").args(["-o", "output", "main.c", "output.S"]).status().unwrap();
    Command::new("./output").status().unwrap();
}
