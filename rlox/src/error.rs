use std::result::Result as RResult;

pub type Result<T> = RResult<T,CompileError>;

#[derive(Debug)]
pub struct CompileError {
    locus: usize,
    msg: String,
}

impl CompileError {
    pub fn new(locus: usize, msg: String) -> CompileError {
        return CompileError {locus,msg};
    }
    pub fn from_str(locus: usize, msg: &str) -> CompileError {
        return CompileError {locus,msg:msg.to_string()};
    }
    pub fn emit(&self) {
        //TODO: Add pretty colors 可愛!!
        println!("{}",self.msg);
    }
}
