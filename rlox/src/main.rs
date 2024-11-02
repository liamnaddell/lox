mod token;
mod parse;
mod error;
mod bc;

use token::*;
use parse::*;
use bc::*;
use std::fs::File;
use std::io::*;
use std::env;
use std::process::ExitCode;

struct Args {
    bc: bool,
    file: String,

}
fn argparse(args: &Vec<String>) -> Option<Args> {
    let mut arg = Args {
        bc: false,
        file: "".to_string(),
    };

    let mut i = 1;
    while i < args.len() {
        if args[i] == "-b" {
            arg.bc = true;
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

    if arg.bc {
        let mut hunk = Chunk::new();

        //((1+2-1)*2)/2=2
        hunk.add_const(1.0);
        hunk.add_const(2.0);
        hunk.add_add();
        hunk.add_const(1.0);
        hunk.add_sub();
        hunk.add_const(2.0);
        hunk.add_mul();
        hunk.add_const(2.0);
        hunk.add_div();
        hunk.add_print();
        hunk.add_return();

        println!("{}",hunk);

        let res = hunk.interpret();
        println!("{:?}",res);
        return (res == InterpretResult::OK) as usize;
    }

    let mut file = File::open(arg.file).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    error::initialize_errors(&source);
    let tkns = tokenize(source);
    println!("TKNS: {:?}",tkns);
    
    let ast = parse(tkns);
    println!("AST: {:?}",ast);

    if !ast.is_some() {
        println!("Parsing failed");
        return 1;
   }
    let ast = ast.unwrap();

    let evaled = eval(ast);
    println!("EVAL: {:?}",evaled);

    return 0;

}
