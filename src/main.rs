mod token;
mod parse;
mod error;
mod bc;
mod compile;
mod ast;
use token::*;
use parse::*;
use bc::*;
use std::fs::File;
use std::io::*;
use std::env;
use std::process::ExitCode;
use compile::*;

struct Args {
    bc: bool,
    file: String,

}
fn argparse(args: &Vec<String>) -> Option<Args> {
    let mut arg = Args {
        bc: true,
        file: "".to_string(),
    };

    let mut i = 1;
    //technically dead code now but here for future use.
    while i < args.len() {
        if args[i] == "-e" {
            arg.bc = false;
        } else {
            break;
        }
        i+=1;
    }
    if i == args.len() {
        println!("You need to specify a file to parse");
        return None;
    }

    arg.file=args[i].clone();

    return Some(arg);
}

fn main() -> ExitCode {
    let res = compiler_main();

    ExitCode::from(res as u8)
}

fn compiler_main() -> usize {
    let args = env::args().collect();

    let arg = argparse(&args);
    if arg.is_none() {
        println!("Argparse failed");
        return 1;
    }

    let arg = arg.unwrap();


    let mut file = File::open(arg.file).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    error::initialize_errors(&source);
    let tkns = tokenize(source);
    println!("TKNS: {:?}",tkns);
    
    let ast = parse(tkns);

    if !ast.is_some() {
        println!("Parsing failed");
        return 1;
   }
    let pp = ast.unwrap();
    let prg = pp.get_program();
    println!("AST: {:?}",*prg);

    let mut bc_comp = compile::CompilePass::new();
    //let mut vm = VM::new();
    //vm.compile(ast.as_ref());
    bc_comp.visit_program(&prg);
    bc_comp.display_bc();

    let mut vm = VM::new(bc_comp);
    let res = vm.interpret();
    println!("{:?}",res);
    return (res == InterpretResult::OK) as usize;
}
