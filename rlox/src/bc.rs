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
    OP_NEGATE,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    //TODO: Equality + comparison (  OP_EQUAL, OP_GREATER, OP_LESS,)

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

#[derive(Clone,Copy,PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Num(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => {
                write!(f,"{}",b)?;
            }
            Value::Nil => {
                write!(f,"Nil")?;
            }
            Value::Num(fnum) => {
                write!(f,"{}",fnum)?;
            }
        }
        write!(f,"\n")
    }
}

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

fn is_falsey(v: Value) -> bool {
    if let Value::Bool(b) = v {
        return b == false;
    }

    if v == Value::Nil {
        return true;
    }

    return false;
}

type FBinOp = fn(f64,f64) -> f64;
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
    pub fn add_const(&mut self,v:f64) {
        self.constants.push(Value::Num(v));
        let index = self.constants.len()-1;
        self.code.push(Opcode::OP_CONSTANT as u8);
        self.code.push(index as u8);
    }
    pub fn add_nil(&mut self) {
        self.code.push(Opcode::OP_NIL as u8);
    }
    pub fn add_true(&mut self) {
        self.code.push(Opcode::OP_TRUE as u8);
    }
    pub fn add_false(&mut self) {
        self.code.push(Opcode::OP_FALSE as u8);
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
    pub fn add_negate(&mut self) {
        self.code.push(Opcode::OP_NEGATE as u8);
    }
    pub fn add_not(&mut self) {
        self.code.push(Opcode::OP_NOT as u8);
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
    fn get_fn(&self,opc:Opcode) -> FBinOp {
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
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    match (v1,v2) {
                        (Value::Num(f1),Value::Num(f2)) => {
                            self.push_stack(Value::Num(op_fn(f2,f1)));
                        }
                        _ => {
                            return RuntimeError;
                        }
                    }
                }
                OP_NIL => {
                    self.push_stack(Value::Nil);
                }
                OP_TRUE => {
                    self.push_stack(Value::Bool(true));
                }
                OP_FALSE => {
                    self.push_stack(Value::Bool(false));
                }
                OP_NEGATE => {
                    if self.stack_len() < 1 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();

                    if let Value::Num(a) = v1 {
                        self.push_stack(Value::Num(-1.0*a));
                    } else {
                        return RuntimeError;
                    }
                }
                OP_NOT => {
                    if self.stack_len() < 1 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();

                    self.push_stack(Value::Bool(is_falsey(v1)));
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
                OP_NIL => {
                    write!(f,"OP_NIL")?;
                }
                OP_TRUE => {
                    write!(f,"OP_TRUE")?;
                }
                OP_FALSE => {
                    write!(f,"OP_FALSE")?;
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
                OP_NEGATE => {
                    write!(f,"OP_NEGATE")?;
                }
                OP_NOT => {
                    write!(f,"OP_NOT")?;
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

