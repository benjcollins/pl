use std::{fs::File, process::Command};

mod token;
mod lexer;
mod ast;
mod parser;
mod compile_ast;
mod typed_ast;
mod qbe;
mod symbols;
mod infer;
mod ty;
mod ir;
mod compile_typed_ast;

fn main() {
    let src = include_str!("../example.txt");
    let tokens = lexer::lex(src);
    let (program, symbols) = match parser::parse(&tokens, src) {
        Ok(r) => r,
        Err(e) => {
            println!("{}", e);
            return
        }
    };

    // println!("{:#?}", program);

    let mut func_mirs = vec![];
    for (name, func_ast) in &program.funcs {
        if let Some(func_mir) = compile_ast::compile_func(*name, func_ast, &program) {
            // println!("{}", func_mir);
            func_mirs.push(func_mir);
        }
    }

    let file = File::create("output.ssa").unwrap();
    for (name, structure) in &program.structs {
        qbe::compile_struct(*name, structure, &file, &symbols).unwrap();
    }
    for func_mir in &func_mirs {
        let func_lir = compile_typed_ast::lower_func(func_mir);
        qbe::compile_fun(&func_lir, &file, &symbols, &program).unwrap();
    }
    Command::new("qbe/obj/qbe").args(["output.ssa", "-o", "output.S"]).status().unwrap();
    Command::new("gcc").args(["-o", "output", "main.c", "output.S"]).status().unwrap();
    Command::new("./output").status().unwrap();
}
