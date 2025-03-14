use std::fmt;

#[repr(u8)]
#[derive(Clone,Copy,Ord,PartialOrd,PartialEq,Eq,Debug)]
#[allow(non_camel_case_types)]
pub enum Opcode {
    OP_RETURN = 0,
    OP_CONSTANT,
    //discard top value on stack
    OP_POP,
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
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_AND,
    OP_OR,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    //create a function on the stack
    #[allow(dead_code)]
    OP_FUNC,
    //create a function on the stack with arguments after
    OP_CALL,
    OP_SET_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_LOCAL,
    OP_GET_LOCAL,

    //MAKE SURE THIS IS THE LAST ONE!!!!!
    /* NEVER ADD CODE HERE */
    OP_NONE, /* DONT THINK ABOUT IT!!! */
    /* NEVER ADD CODE HERE */
/*STOP*/}

impl Opcode {
    pub fn from_u8(mo: u8) -> Opcode {
        if mo < (Opcode::OP_NONE as u8) {
            // this is safe because mo >=0 and  mo < OP_NONE, and mo has u8 size.
            return unsafe { std::mem::transmute(mo) }
        } else {
            return Opcode::OP_NONE;
        }
    }
}

#[derive(Clone,PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    Nil,
    Num(f64),
    String(String),
    //index into vm.funcs
    Func(usize),
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
            Value::String(fstr) => {
                write!(f,"{}",fstr)?;
            }
            Value::Func(i) => {
                write!(f,"<func: {}>",i)?;
            }
        }
        write!(f,"\n")
    }
}

#[derive(Debug,PartialEq)]
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

fn is_falsey(v: &Value) -> bool {
    if let Value::Bool(b) = v {
        return *b == false;
    }

    if *v == Value::Nil {
        return true;
    }

    return false;
}

pub struct Function {
    pub chunk: usize,
    pub arity: usize,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"arity:{}>\n",self.arity)
    }
}

#[derive(Clone,Copy)]
struct Frame {
    //stored instruction pointer in previous frame (return address)
    sip: usize,
    //index into vm.funcs, i.e. which function 
    func_index: usize,
    //size of the stack before the function call.
    sp: usize,
}

pub struct VM {
    //Means of storing Function's and their bytecode.
    //Array indexes are "function id's".
    pub funcs: Vec<Function>,
    pub chunks: Vec<Chunk>,
    pub stack: Vec<Value>,
    frames: Vec<Frame>,
    pub globals: Vec<Value>,
}

use crate::compile::CompilePass;
impl VM {
    pub fn new(cp: CompilePass) -> VM {
        return VM {funcs: cp.funcs,
            chunks:cp.cnks,
            frames:vec!(),
            stack:vec!(),
            globals: cp.globals,
        };
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

    fn current_frame(&self) -> Frame {
        assert!(self.frames.len() > 0);
        let frame = self.frames[self.frames.len() - 1];
        return frame;
    }

    pub fn current_func(&self) -> &Function {
        let findex = self.current_frame().func_index;
        assert!(findex < self.funcs.len());
        return &self.funcs[findex];
    }

    pub fn current_chunk(&self) -> &Chunk {
        return &self.chunks[self.current_func().chunk];
    }

    pub fn current_code(&self) -> &Vec<u8> {
        return &self.current_chunk().code;
    }

    pub fn current_constants(&self) -> &Vec<Value> {
        return &self.current_chunk().constants;
    }


    pub fn interpret(&mut self) -> InterpretResult {
        use InterpretResult::*;
        use Opcode::*;
        let mut i = 0;
        //start at the main function
        self.frames.push(Frame { sip: 0, func_index:0, sp: 0 });
        let mut reset_ip = false;
        let mut skip_increment = false;
        loop {
            if i >= self.current_code().len() {
                panic!("missing return");
            }
            let opc = self.current_code()[i];
            let op = Opcode::from_u8(opc);
            //TODO: Clean up all of this spammy garbage and get rid of CompileError
            match op {
                OP_RETURN => { 
                    let frame = self.frames.pop().unwrap();

                    if self.frames.len() == 0 {
                        //how we return from main function but have full stack
                        //smells like compiler bug
                        assert!(self.stack_empty());
                        return OK;
                    }

                    //somehow we deleted local variables before deleting them at return??
                    assert!(self.stack_len() >= frame.sp);
                    while self.stack_len() != frame.sp { self.pop_stack(); };

                    i=frame.sip+1;
                    //TODO(return): Add a proper return value here...
                    self.push_stack(Value::Nil);
                    continue;
                }
                OP_POP => {
                    let _ = self.pop_stack();
                }
                OP_CONSTANT => { 
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i+=1;
                    let const_index = self.current_code()[i] as usize;

                    if const_index >= self.current_constants().len() {
                        return CompileError;
                    }

                    let v = self.current_constants()[const_index].clone();
                    self.push_stack(v);
                }

                OP_SET_GLOBAL => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
               
                    let glob_var_index = self.current_code()[i] as usize;
                    assert!(glob_var_index <= self.globals.len());
    
                    let v = self.pop_stack();
                    //no need to create var
                    if glob_var_index < self.globals.len() {
                        self.globals[glob_var_index] = v;
                    } else {
                        self.globals.push(v);
                        assert!(self.globals.len() - 1 == glob_var_index);
                    }
                }
                OP_SET_LOCAL => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
                    let loc_var_no = self.current_code()[i] as usize;
                    assert!(loc_var_no < self.stack.len());
                    //setting a new local in the current scope
                    if loc_var_no == self.stack.len ()-1 {
                        //do nothing.
                    } else {
                        assert!(self.stack.len() != 0);
                        let v = self.pop_stack();
                        self.stack[loc_var_no] = v;
                    }
    
                }
                OP_GET_LOCAL => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
                    let loc_var_no = self.current_code()[i] as usize;
                    assert!(loc_var_no < self.stack.len());
                    let v = self.stack[loc_var_no].clone();
                    self.stack.push(v);
                }
                OP_GET_GLOBAL => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
               
                    let glob_var_index = self.current_code()[i] as usize;
                    assert!(glob_var_index <= self.globals.len());

                    let v = self.globals[glob_var_index].clone();
                    self.push_stack(v);
                }

                OP_FUNC => { 
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    //TODO: Fix validation
                    i+=1;
                    let func_index = self.current_code()[i] as usize;

                    assert!(func_index < self.funcs.len());

                    self.push_stack(Value::Func(func_index));
                }
                OP_CALL => { 
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    //TODO: Fix validation
                    i+=1;
                    let findex = self.current_code()[i] as usize;
                    if findex >= self.funcs.len() {
                        return CompileError;
                    }
                    let func = &self.funcs[findex];

                    let arity = func.arity;
                    //TODO: BRAIN ROT?
                    if self.stack_len() < arity {
                        return CompileError;
                    }
                    //create a new frame, return when complete
                    self.frames.push(Frame { sip:i,func_index:findex, sp: self.stack_len()});
                    reset_ip = true;

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
                        (Value::String(s1), Value::String(s2)) => {
                            self.push_stack(Value::String(s2 + &s1));
                        }
                        _ => {
                            return RuntimeError;
                        }
                    }
                }
                OP_EQUAL => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    self.push_stack(Value::Bool(v1 == v2));
                }
                OP_GREATER | OP_LESS => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    match (v1,v2) {
                        (Value::Num(f1),Value::Num(f2)) => {
                            if op == OP_GREATER {
                                self.push_stack(Value::Bool(f2 > f1));
                            } else if op == OP_LESS {
                                self.push_stack(Value::Bool(f2 < f1));
                            } else {
                                panic!("whut");
                            }
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

                    self.push_stack(Value::Bool(is_falsey(&v1)));
                }
                OP_AND | OP_OR => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v2 = self.pop_stack();
                    let v1 = self.pop_stack();
                    match (v2,v1) {
                        (Value::Bool(a),Value::Bool(b)) => {
                            if op == OP_AND {
                                self.push_stack(Value::Bool(a && b));
                            } else if op == OP_OR {
                                self.push_stack(Value::Bool(a || b));
                            } else {
                                panic!("whut");
                            }
                        }
                        _ => {return RuntimeError;}
                    }
                }
                OP_JUMP_IF_FALSE => {
                    if i + 2 >= self.current_code().len() {
                        return CompileError;
                    }
                    i+=1;
                    let jump_offset = self.current_code()[i] as usize;

                    let cond = &self.stack[self.stack_len() - 1];
                    if is_falsey(cond) {
                        i+= jump_offset;
                        self.pop_stack();
                        skip_increment = true;
                    }
                }
                OP_JUMP => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i+=1;
                    let jump_offset = self.current_code()[i] as usize;
                    i += jump_offset;
                    skip_increment = true;
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
            if reset_ip {
                i=0;
                reset_ip=false;
            } else if !skip_increment {
                i+=1;
            }
            skip_increment = false;
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}


type FBinOp = fn(f64,f64) -> f64;
impl Chunk {
    pub fn new() -> Chunk {
        return Chunk { code:vec!(),constants: vec!()};
    }
    //TODO: Arguments
    pub fn add_call(&mut self,findex:usize) {
        self.code.push(Opcode::OP_CALL as u8);
        self.code.push(findex as u8);
    }
    pub fn add_pop(&mut self) {
        self.code.push(Opcode::OP_POP as u8);
    }
    pub fn add_const_num(&mut self,v:f64) {
        self.constants.push(Value::Num(v));
        let index = self.constants.len()-1;
        self.code.push(Opcode::OP_CONSTANT as u8);
        self.code.push(index as u8);
    }
    pub fn add_const_str(&mut self, v: &String) {
        self.constants.push(Value::String(v.clone()));
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
    pub fn add_or(&mut self) {
        self.code.push(Opcode::OP_OR as u8);
    }
    pub fn add_and(&mut self) {
        self.code.push(Opcode::OP_AND as u8);
    }
    pub fn add_div(&mut self) {
        self.code.push(Opcode::OP_DIVIDE as u8);
    }
    pub fn add_return(&mut self) {
        self.code.push(Opcode::OP_RETURN as u8);
    }
    pub fn add_equal(&mut self) {
        self.code.push(Opcode::OP_EQUAL as u8);
    }
    pub fn add_greater(&mut self) {
        self.code.push(Opcode::OP_GREATER as u8);
    }
    pub fn add_less(&mut self) {
        self.code.push(Opcode::OP_LESS as u8);
    }
    pub fn add_get_global(&mut self, index: usize) {
        self.code.push(Opcode::OP_GET_GLOBAL as u8);
        self.code.push(index as u8);
    }
    pub fn add_set_global(&mut self, vindex:usize) {
        self.code.push(Opcode::OP_SET_GLOBAL as u8);
        self.code.push(vindex as u8);
    }
    pub fn add_set_local(&mut self, ofs:u8) {
        self.code.push(Opcode::OP_SET_LOCAL as u8);
        self.code.push(ofs);
    }
    pub fn add_get_local(&mut self, ofs: u8) {
        self.code.push(Opcode::OP_GET_LOCAL as u8);
        self.code.push(ofs);
    }
    pub fn add_jump_if(&mut self, ofs: u8) -> usize{
        self.code.push(Opcode::OP_JUMP_IF_FALSE as u8);
        self.code.push(ofs);
        return self.code.len() -1;
    }
    pub fn add_jump_else(&mut self, ofs: u8) -> usize {
        self.code.push(Opcode::OP_JUMP as u8);
        self.code.push(ofs);
        return self.code.len() -1;
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"==BEGIN==\n")?;
        let mut i = 0;
        while i < self.code.len() {
            use Opcode::*;
            let opc = self.code[i];
            write!(f,"{:b}\t",opc)?;
            let op = Opcode::from_u8(opc);
            match op {
                OP_RETURN => { 
                    write!(f,"OP_RETURN")?;
                }
                OP_POP => {
                    write!(f,"OP_POP\t")?;
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

                    let v = self.constants[const_index].clone();
                    write!(f,"{}",v)?;
                }

                OP_SET_GLOBAL => {
                    write!(f,"OP_SET_GLOBAL\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }

                    i+=1;
                    let global_index = self.code[i] as usize;
                    write!(f, "{}", global_index)?; 
                }
                
                OP_GET_GLOBAL => {
                    write!(f,"OP_GET_GLOBAL\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let global_index = self.code[i] as usize;

                    write!(f,"{}",global_index)?; 
                } 
                OP_GET_LOCAL => {
                    write!(f,"OP_GET_LOCAL\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let index = self.code[i] as usize;

                    write!(f,"{}",index)?; 
                } 
                OP_SET_LOCAL => {
                    write!(f,"OP_SET_LOCAL\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let index = self.code[i] as usize;

                    write!(f,"{}",index)?; 
                } 
                OP_FUNC => { 
                    write!(f,"OP_FUNC\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let func_index = self.code[i] as usize;

                    write!(f,"{}",func_index)?;
                }
                OP_CALL => { 
                    write!(f,"OP_CALL\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let func_index = self.code[i] as usize;
                    write!(f,"{}",func_index)?;
                }

                OP_JUMP_IF_FALSE => {
                    write!(f,"OP_JUMP_IF_FALSE\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let index = self.code[i] as usize;

                    write!(f,"{}",index)?;  
                }
                OP_JUMP => {
                    write!(f,"OP_JUMP\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let index = self.code[i] as usize;

                    write!(f,"{}",index)?; 
                } 

                OP_NIL | OP_TRUE | OP_EQUAL | OP_FALSE | OP_ADD | OP_SUBTRACT 
                | OP_MULTIPLY | OP_DIVIDE | OP_PRINT | OP_NEGATE | OP_NOT 
                | OP_GREATER | OP_LESS | OP_AND | OP_OR => {
                    write!(f,"{:?}",op)?;
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

