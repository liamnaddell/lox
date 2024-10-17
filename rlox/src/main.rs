mod token;
mod parse;

use token::*;
use parse::*;
use std::fs::File;
use std::io::prelude::*;
use std::env;


fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("You SUCK at specifying file input");
    }

    let file = File::open(args[1].to_string());
    let mut source = String::new();
    
    match file {
        Ok(mut f) => {
            let mut source = String::new();
            f.read_to_string(&mut source).unwrap();
            let tkns = tokenize(source);
            println!("TKNS: {:?}",tkns);
        },
        Err(e) => panic!("File does not exist??: {e:?}"),
    }

    
}
