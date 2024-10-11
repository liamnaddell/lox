mod token;
mod parse;

use token::*;
use parse::*;


fn main() {
    let source = String::new();
    let tkns = tokenize(&source);
    let ast = parse(tkns);
    println!("{:?}",ast);
}
