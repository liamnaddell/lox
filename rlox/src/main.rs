mod token;
mod parse;

use token::*;
use parse::*;


fn main() {
    let source = String::new();
    let tkns = tokenize(&source);
    println!("TKNS: {:?}",tkns);
    let ast = parse(tkns);
    println!("AST: {:?}",ast);
}
