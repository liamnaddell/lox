mod token;
mod parse;
mod error;
mod bc;
mod eval;

use token::*;
use parse::*;
use bc::*;
use std::fs::File;
use std::io::*;
use std::env;
use std::process::ExitCode;
use crate::eval::*;

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
    let ast = ast.unwrap();
    println!("AST: {:?}",ast);

    if arg.bc {
        let mut vm = VM::new();
        vm.compile(ast.as_ref());
        vm.display_bc();

        let res = vm.interpret();
        println!("{:?}",res);
        return (res == InterpretResult::OK) as usize;
    } else {
        eval(ast);
    }

    return 0;

}
