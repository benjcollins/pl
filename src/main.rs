use std::{fs::File, process::Command};

mod ast;
mod compile_ast;
mod compile_typed_ast;
mod infer;
mod ir;
mod lexer;
mod parser;
mod qbe;
mod region;
mod symbols;
mod token;
mod tokens;
mod ty;
mod typed_ast;

fn main() {
    let source = include_str!("../example.txt");
    let tokens = lexer::lex(source);
    let (program, symbols, parse_errors) = parser::parse(&tokens);
    if parse_errors.len() > 0 {
        for err in parse_errors {
            println!("{}", err)
        }
        return;
    }

    // println!("{:#?}", program);

    let mut func_mirs = vec![];
    for func_ast in program.func_iter() {
        if let Some(func_mir) = compile_ast::compile_func(func_ast, &program) {
            // println!("{}", func_mir);
            func_mirs.push(func_mir);
        }
    }

    let file = File::create("output.ssa").unwrap();
    for struct_decl in program.struct_iter() {
        qbe::compile_struct(struct_decl, &file, &symbols).unwrap();
    }
    for func_mir in &func_mirs {
        let func_lir = compile_typed_ast::lower_func(func_mir);
        qbe::compile_func(&func_lir, &file, &symbols, &program).unwrap();
    }
    Command::new("qbe/obj/qbe")
        .args(["output.ssa", "-o", "output.S"])
        .status()
        .unwrap();
    Command::new("gcc")
        .args(["-o", "output", "main.c", "output.S"])
        .status()
        .unwrap();
    Command::new("./output").status().unwrap();
}
