use std::result::Result as RResult;
use std::sync::OnceLock;

pub type Result<T> = RResult<T,CompileError>;

#[derive(Debug)]
pub struct CompileError {
    locus: usize,
    msg: String,
}

static SOURCE: OnceLock<String> = OnceLock::new();

impl CompileError {
    pub fn new(locus: usize, msg: String) -> CompileError {
        return CompileError {locus,msg};
    }
    pub fn from_str(locus: usize, msg: &str) -> CompileError {
        return CompileError::new(locus,msg.to_string());
    }
    pub fn emit(&self) {
        //TODO: FIX GARBAGE CODE
        println!("\x1b[31;1mERROR:\x1b[0m {}",self.msg);
        let data: &[u8] = SOURCE.get().unwrap().as_ref();
        let pad = "  | ";
        let mut begin = self.locus;
        let mut end = begin;
        let mut locus = self.locus;
        while begin != 0 {
            if data[begin] == b'\n' {
                break;
            }
            begin-=1;
        }
        if data[begin] == b'\n' {
            begin+=1;
        }

        while end != data.len() {
            if data[end] == b'\n' {
                break;
            }
            end+=1;
        }

        locus -=begin;


        println!("{}",pad);
        print!("{}",pad);
        let mut i = begin;
        while i < end {
            print!("{}",data[i] as char);
            i+=1;
        }

        println!("");
        print!("{}",pad);

        i=0;
        while i < locus {
            print!(" ");
            i+=1;
        }

        println!("^");
    }
}

pub fn new_err(locus:usize, msg: &str) -> CompileError {
    return CompileError::from_str(locus,msg);
}


pub fn initialize_errors(s:&str) {
    let _ = SOURCE.set(s.to_string());
}
