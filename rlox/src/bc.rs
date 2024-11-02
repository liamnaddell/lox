use std::fmt;

#[repr(u8)]
#[derive(Clone,Copy,Ord,PartialOrd,PartialEq,Eq)]
#[allow(non_camel_case_types)]
enum Opcode {
    OP_RETURN = 0,
    OP_CONSTANT,
    OP_PRINT,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,

    //MAKE SURE THIS IS THE LAST ONE!!!!!
    /* NEVER ADD CODE HERE */
    OP_NONE, /* DONT THINK ABOUT IT!!! */
    /* NEVER ADD CODE HERE */
/*STOP*/}

impl Opcode {
    fn from_u8(mo: u8) -> Opcode {
        if mo < (Opcode::OP_NONE as u8) {
            // this is safe because mo >=0 and  mo < OP_NONE, and mo has u8 size.
            return unsafe { std::mem::transmute(mo) }
        } else {
            return Opcode::OP_NONE;
        }
    }
}

type Value = f64;

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    stack: Vec<Value>,
}

#[derive(Debug,PartialEq)]
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

type BinOp = fn(Value,Value) -> Value;
impl Chunk {
    pub fn new() -> Chunk {
        return Chunk { code:vec!(),constants: vec!(), stack: vec!()};
    }
    pub fn stack_len(&self) -> usize {
        return self.stack.len();
    }
    pub fn stack_empty(&self) -> bool {
        return self.stack_len() == 0;
    }
    pub fn push_stack(&mut self,v:Value) {
        return self.stack.push(v);
    }
    pub fn pop_stack(&mut self) -> Value {
        return self.stack.pop().expect("ICE");
    }
    pub fn add_const(&mut self,v:Value) {
        self.constants.push(v);
        let index = self.constants.len()-1;
        self.code.push(Opcode::OP_CONSTANT as u8);
        self.code.push(index as u8);
    }
    pub fn add_print(&mut self) {
        self.code.push(Opcode::OP_PRINT as u8);
    }
    pub fn add_add(&mut self) {
        self.code.push(Opcode::OP_ADD as u8);
    }
    pub fn add_sub(&mut self) {
        self.code.push(Opcode::OP_SUBTRACT as u8);
    }
    pub fn add_mul(&mut self) {
        self.code.push(Opcode::OP_MULTIPLY as u8);
    }
    pub fn add_div(&mut self) {
        self.code.push(Opcode::OP_DIVIDE as u8);
    }
    pub fn add_return(&mut self) {
        self.code.push(Opcode::OP_RETURN as u8);
    }
    fn get_fn(&self,opc:Opcode) -> BinOp {
        use Opcode::*;
        return match opc {
            OP_ADD => |x,y| {x + y},
            OP_SUBTRACT => |x,y| {x - y},
            OP_MULTIPLY => |x,y| {x * y},
            OP_DIVIDE => |x,y| {x / y},
            _ => { panic!("get fn failed compiler sucks")}
        };
    }
    pub fn interpret(&mut self) -> InterpretResult {
        use InterpretResult::*;
        use Opcode::*;
        let mut i = 0;
        loop {
            let opc = self.code[i];
            let op = Opcode::from_u8(opc);
            match op {
                OP_RETURN => { 
                    return OK;
                }
                OP_CONSTANT => { 
                    if i + 1 >= self.code.len() {
                        return CompileError;
                    }
                    i+=1;
                    let const_index = self.code[i] as usize;

                    if const_index >= self.constants.len() {
                        return CompileError;
                    }

                    let v = self.constants[const_index];
                    self.push_stack(v);
                }
                OP_ADD | OP_SUBTRACT | OP_MULTIPLY | OP_DIVIDE => {
                    let op_fn = self.get_fn(op);
                    if self.stack_len() < 1 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    self.push_stack(op_fn(v2,v1));

                }
                OP_PRINT => {
                    if self.stack_empty() {
                        return CompileError;
                    }
                    let v = self.pop_stack();
                    println!("{}",v);
                }
                OP_NONE => {
                    return CompileError;
                }

            }
            i+=1;
        }
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"==BEGIN==\n")?;
        //TODO: Fix this
        let mut i = 0;
        while i < self.code.len() {
            use Opcode::*;
            let opc = self.code[i];
            write!(f,"{:b}\t",opc)?;
            match Opcode::from_u8(opc) {
                OP_RETURN => { 
                    write!(f,"OP_RETURN")?;
                }
                OP_CONSTANT => { 
                    write!(f,"OP_CONSTANT\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let const_index = self.code[i] as usize;

                    if const_index >= self.constants.len() {
                        write!(f," WTFINDEX")?;
                    }

                    let v = self.constants[const_index];
                    write!(f,"{}",v)?;
                }
                OP_ADD => {
                    write!(f,"OP_ADD")?;
                }
                OP_SUBTRACT => {
                    write!(f,"OP_SUBTRACT")?;
                }
                OP_MULTIPLY => {
                    write!(f,"OP_MULTIPLY")?;
                }
                OP_DIVIDE => {
                    write!(f,"OP_DIVIDE")?;
                }
                OP_PRINT => {
                    write!(f,"OP_PRINT")?;
                }
                OP_NONE => {
                    write!(f,"Invalid opcode!!!")?;
                }
            }
            i+=1;
            write!(f,"\n")?;
        }
        write!(f,"==END==\n")?;
        Ok(())
    }
}

