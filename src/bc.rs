use std::fmt;
use crate::Program;
use std::collections::HashMap;

#[repr(u8)]
#[derive(Clone,Copy,Ord,PartialOrd,PartialEq,Eq,Debug)]
#[allow(non_camel_case_types)]
enum Opcode {
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
    fn from_u8(mo: u8) -> Opcode {
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
    //index into vm.funcs
    pub chunk: Chunk,
    //arity of the function.
    pub arity: usize,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"arity:{}> function:\n{}",self.arity,self.chunk)
    }
}

#[derive(Clone,Copy)]
struct Frame {
    //stored instruction pointer in previous frame (return address)
    sip: usize,
    //index into vm.funcs, i.e. which function 
    func_index: usize,
}

#[derive(Clone,Copy)]
struct Decl {
    //friendly reminder: no decl may have id 0
    decl_id: u64,
    scope: u8,
}
struct CTStack {
    decls: [Decl;u8::MAX as usize],
    //lexical scope depth
    current_depth: u8,
    tip: u8,
}

impl CTStack {
    fn new() -> CTStack {
        return CTStack {decls: [Decl {decl_id:0,scope:0};255], tip: 0, current_depth: 0};
    }
    fn push_frame(&mut self) {
        self.current_depth += 1;
    }
    fn pop_frame(&mut self) {
        assert!(self.current_depth != 0);
        for i in (0..(self.tip as usize)).rev() {
            assert!(self.decls[i].decl_id != 0);
            if self.decls[i].scope != self.current_depth {
                break;
            }
            self.decls[i].decl_id = 0;
            assert!(self.tip != 0);
            self.tip -= 1;
        }
        self.current_depth -= 1;
    }
    fn add_decl(&mut self, d:u64) -> u8 {
        assert!(self.decls[self.tip as usize].decl_id == 0);
        self.decls[self.tip as usize].decl_id = d;
        self.decls[self.tip as usize].scope = self.current_depth;
        let ret = self.tip;
        self.tip += 1;
        return ret;
    }
    fn get_stack_offset_of_decl(&self, d:u64) -> Option<(u8,Decl)> {
        let end = self.tip as usize;
        for i in (0..end).rev() {
            if self.decls[i].decl_id == d {
                return Some((i as u8,self.decls[i]));
            }
        }
        return None;
    }
}

pub struct VM {
    //Means of storing Function's and their bytecode.
    //Array indexes are "function id's".
    pub funcs: Vec<Function>,
    pub stack: Vec<Value>,
    frames: Vec<Frame>,
    //used for allocating ct_name_to_id slots
    next_id: u64,
    //A bijection of variable/function names and their associated id's
    ct_name_to_id: HashMap<String,u64>,
    //A mapping from variable id to index in the `globals` array
    ct_global_id_to_index: HashMap<u64,usize>,
    pub globals: Vec<Value>,
    ct_stack: CTStack,
}

impl VM {
    pub fn ct_push_scope(&mut self) {
        self.ct_stack.push_frame();
    }
    pub fn ct_pop_scope(&mut self) {
        self.ct_stack.pop_frame();
    }
    pub fn ct_add_decl(&mut self, d:u64) -> u8 {
        return self.ct_stack.add_decl(d);
    }
    fn ct_get_stack_offset_of_decl(&self, d:u64) -> Option<(u8,Decl)> {
        return self.ct_stack.get_stack_offset_of_decl(d);
    }
    pub fn ct_get_id_of_var(&mut self, name: &str) -> u64 {
        let v = self.ct_name_to_id.get(name);
        if let Some(id) = v {
            return *id;
        }

        let id = self.next_id;
        self.next_id+=1;

        self.ct_name_to_id.insert(name.to_string(),id);
        return id;
    }

    pub fn ct_add_function(&mut self, name: String, func: Function) {
        self.funcs.push(func);
        let id = self.ct_get_id_of_var(&name);
        let ind = self.funcs.len() - 1;
        self.ct_global_id_to_index.insert(id, ind);
    }

    pub fn ct_get_function_index(&mut self, name: &str) -> Option<usize> {
        //TODO: THIS FUNCTION STINKS!!!!!!!
        let id = self.ct_get_id_of_var(name);
        let index = self.ct_get_global_index(id);
        return index;
    }

    //emit a variable assignment, either to a global or local
    pub fn emit_assign_var(&mut self, ch: &mut Chunk, var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        let Some((ofs,decl)) = self.ct_get_stack_offset_of_decl(id) else {
            assert!(false,"TODO: Error handling");
            return;
        };
        if decl.scope == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            ch.add_set_global(index);
            //exprs must return something.
            ch.add_get_global(index);
            return;
        }
        ch.add_set_local(ofs);
        ch.add_get_local(ofs);
    }


    pub fn ct_get_global_index(&self, id:u64) -> Option<usize> {
        let v = self.ct_global_id_to_index.get(&id);
        return v.copied();
    }

    pub fn emit_create_var(&mut self, ch: &mut Chunk, var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        //NOTE: Technically not useful for global variables...
        let ofs = self.ct_add_decl(id);
        if self.ct_stack.current_depth == 0 {
            //global var
            let index = self.globals.len();
            let old_value = self.ct_global_id_to_index.insert(id,index);
            //TODO: Error handling.
            assert!(old_value == None);
            ch.add_set_global(index);
        } else {
            //local var
            ch.add_set_local(ofs);
        }
    }

    pub fn emit_get_var(&mut self, ch: &mut Chunk, var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        let (ofs,decl) = self.ct_get_stack_offset_of_decl(id).expect("ICE: reference to id which does not exist");
        if decl.scope == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            ch.add_get_global(index);
            return;
        }
        ch.add_get_local(ofs);
    }

    pub fn patch_jump(&mut self, ch: &mut Chunk, pos: usize) {
        //jump to the END of the current function.
        let offset = ch.code.len() - pos;

        let offset = match u8::try_from(offset) {
            Ok(offset) => offset,
            Err(_) => {
                panic!("jump too big"); 
            }
        };

        let opc = ch.code[pos -1];
        let op = Opcode::from_u8(opc);
        match op {
            Opcode::OP_JUMP_IF_FALSE => ch.code[pos] = offset,
            Opcode::OP_JUMP => ch.code[pos] = offset,
            _ => panic!("cant do jump at this opcode"),
        }
    }

    pub fn new() -> VM {
        return VM {funcs: vec!(),
            frames:vec!(),
            stack:vec!(),
            ct_name_to_id:HashMap::new(),
            ct_global_id_to_index: HashMap::new(),
            globals:vec!(),
            ct_stack: CTStack::new(),
            next_id:1
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
    pub fn display_bc(&self) {
        for i in 0..self.funcs.len() {
            let func = &self.funcs[i];
            println!("<func #{} {}",i,func);
        }
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

    pub fn compile(&mut self, ast: &Program) {
        let mut cnk = Chunk::new();
        ast.emit_bc(&mut cnk,self);
        self.funcs.push(Function { chunk:cnk,arity:0});
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
        return &self.current_func().chunk;
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
        self.frames.push(Frame { sip: 0, func_index:self.funcs.len()-1 });
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
                    i=frame.sip;
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
                    i += arity;
                    //create a new frame, return when complete
                    self.frames.push(Frame { sip:i,func_index:findex});
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
    code: Vec<u8>,
    constants: Vec<Value>,
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
                    //TODO: Fix this when we have to deal with arguments...
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

