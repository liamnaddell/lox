mod token;
mod parse;

use token::*;
use parse::*;
use std::fs::File;
use std::io::*;
use std::env;


fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("You SUCK at specifying file input");
    }

    let mut file = File::open(args[1].to_string()).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    let tkns = tokenize(source);
    println!("TKNS: {:?}",tkns);
    
    let ast = parse(tkns);
    println!("AST: {:?}",ast);
}
