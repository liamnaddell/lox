use std::fmt;
use crate::obj::*;


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
    //create a closure on the stack
    OP_CLOSURE,
    //create a function on the stack with arguments after
    OP_CALL,
    OP_SET_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_LOCAL,
    OP_GET_LOCAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_WHILE,
    OP_LOOP,

    //MAKE SURE THIS IS THE LAST ONE!!!!!
    /* NEVER ADD CODE HERE */
    OP_NONE, /* DONT THINK ABOUT IT!!! */
    /* NEVER ADD CODE HERE */
    /* See Opcode::from_u8 as for why */
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

/** A unique address used to find an upvalue inside struct VM */
#[derive(Clone,Copy,Debug)]
struct UpvalueAddress {
    frame: usize,
    upvalue: usize,
}
impl UpvalueAddress {
    pub fn new(frame: usize, upvalue: usize) -> Self {
        return UpvalueAddress { frame: frame, upvalue: upvalue }
    }
}

/*
#[derive(Clone,PartialEq, Debug)]
pub enum ValueOld {
    Bool(bool),
    Nil,
    Num(f64),
    String(String),
    Closure(Closure),
}

impl fmt::Display for ValueOld {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueOld::Bool(b) => {
                write!(f,"{}",b)?;
            }
            ValueOld::Nil => {
                write!(f,"Nil")?;
            }
            ValueOld::Num(fnum) => {
                write!(f,"{}",fnum)?;
            }
            ValueOld::String(fstr) => {
                write!(f,"{}",fstr)?;
            }
            ValueOld::Closure(i) => {
                write!(f,"<closure: {}>",i)?;
            }
        }
        write!(f,"")
    }
}
*/

#[derive(Debug,PartialEq)]
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

fn is_falsey(v: &Value) -> bool {
    if v.is_bool() {
        let b = v.get_bool();
        return b == false;
    }

    if v.is_nil() {
        return true;
    }

    return false;
}


pub struct Function {
    pub chunk: usize,
    pub arity: usize,
    /** This is added by the compile.rs code to list all the
     * open upvalues a function utilizes
     */
    pub upvalues_template: Vec<Upvalue>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"arity:{}>\n",self.arity)
    }
}

#[derive(Clone)]
struct Frame {
    //stored instruction pointer in previous frame (return address)
    sip: usize,
    // The function (closure) we are currently running.
    closure: Value,
    //size of the stack before the function call.
    sp: usize,
}

impl Frame {
    pub fn get_closure(&self) -> &mut Closure {
        return self.closure.get_closure();
    }
    pub fn move_closure(&mut self) -> Closure {
        //note, after this called, the current closure is destroyed and now invalid, becuase the
        //Value inside will be nil.
        return self.closure.move_closure();
    }
}

pub struct VM {
    //Means of storing Function's and their bytecode.
    //Array indexes are "function id's".
    pub funcs: Vec<Function>,
    pub chunks: Vec<Chunk>,
    pub stack: Vec<Value>,
    frames: Vec<Frame>,
    pub globals: Vec<Value>,
    //object storage :0
    vals: Vec<InnerValue>,
}

use crate::compile::CompilePass;
impl VM {
    //TODO: This code is StOoPiD.
    //Allocate these manually
    pub fn new_value(&mut self) -> Value {
        let iv = InnerValue::new();
        let v2 = Value::new(&iv);
        self.vals.push(iv);
        return v2;
    }
    pub fn new_value_from_iv(&mut self, iv: InnerValue) -> Value {
        let v2 = Value::new(&iv);
        self.vals.push(iv);
        return v2;
    }
    pub fn new_str(&mut self,a:String) -> Value {
        let v = self.new_value();
        v.set_str(a);
        return v;
    }
    pub fn new_num(&mut self,a:f64) -> Value {
        let v = self.new_value();
        v.set_num(a);
        return v;
    }
    pub fn new_nil(&mut self) -> Value {
        let v = self.new_value();
        return v;
    }
    pub fn new_bool(&mut self,a:bool) -> Value {
        let v = self.new_value();
        v.set_bool(a);
        return v;
    }
    pub fn new_closure(&mut self,a:Closure) -> Value {
        let v = self.new_value();
        v.set_closure(a);
        return v;
    }
    pub fn val_from_const(&mut self,a:&ConstValue) -> Value {
        let mut v = self.new_value();
        v.copy_from_const(a);
        return v;
    }
    pub fn new(cp: CompilePass) -> VM {
        return VM {funcs: cp.funcs,
            chunks:cp.cnks,
            frames:vec!(),
            stack:vec!(),
            globals: cp.globals,
            vals: vec!(),
        };
    }
    pub fn collect_garbage(&mut self) {
        //step 1: Snow White     (mark everything as white/freeable)
        for v in self.vals.iter_mut() {
            v.mark = ValueMark::White;
        }
        //step 2: Plant a Tree   (rooting objects)
        //step 3: Climb the Tree (go thru the worklist to find what's still needed)
        //step 4: Grimhilde      (free anything marked white)

    }
    /** 
     * This function takes an upvalue in the current frame of execution and 
     * closes it by traversing up the stack.
     * NOTE: The upvalue is behind a RC<>, meaning ALL upvalue references point
     * to the same underlying object.
     */
    pub fn close_upvalue(&mut self, upvalue_index: usize) {
        //start at the lowest frame.
        let mut i = self.frames.len() - 1;
        //this is the offset into the current closure's upvalue array we are interested in.
        let mut slot = upvalue_index as usize;

        //This while loop traverses up the stack to resolve the upvalue.
        while i > 0 {
            let cu = &mut self.frames[i].get_closure().upvalues[slot];
            //this is the offset into the next frame's upvalue array.
            slot = cu.slot as usize;
            if cu.is_local() {
                //The upvalue is a local variable in our parent frame.
                let val = &self.stack[self.frames[i].sp + slot];
                //so we grab it.
                let cu = &mut self.frames[i].get_closure().upvalues[slot];
                //this closes the upvalue.
                cu.set(val.clone());
                //The bottom section would copy again. Tis' harmless yet a waste.
                return;
            }
            if cu.is_closed() {
                //in this case, the upvalue was already closed.
                //Proceed to the bottom section where we copy the 
                //closed value
                break;
            }
            i -= 1;
        }
        assert!(i > 0);
        let cu = &self.frames[i].get_closure().upvalues[slot];
        let cv = Some(cu.closed_value.as_ref().unwrap().clone());
        let lf = self.frames.len() - 1;
        let u = &mut self.frames[lf].get_closure().upvalues[upvalue_index];
        u.closed_value = cv;
    }
    pub fn create_closed_closure(&mut self,findex: usize) -> Closure {
        let cl = Closure::new(self,findex);
        let len = cl.upvalues.len();
        //close_upvalue resolves upvalues in the "current frame".
        //We need to temporarily push the closure onto the framestack 
        //in order to resolve upvalues.
        //Makes the code easier to write.
        //Efficiency note: This is pretty inefficient and bad :(
        let c = self.new_closure(cl);
        self.frames.push(Frame { closure:c,sip:0,sp:0});
        for upvalue in 0..len {
            self.close_upvalue(upvalue);
        }
        let mut f = self.frames.pop().unwrap();
        return f.move_closure();
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
        return self.stack.pop().expect("attempt to pop off the stack but there's nothing on the stack");
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

    fn current_frame(&self) -> &Frame {
        assert!(self.frames.len() > 0);
        let frame = &self.frames[self.frames.len() - 1];
        return frame;
    }

    pub fn current_func(&self) -> &Function {
        let findex = self.current_closure().func;
        assert!(findex < self.funcs.len());
        return &self.funcs[findex];
    }

    pub fn current_closure(&self) -> &mut Closure {
        return self.current_frame().get_closure();
    }

    pub fn current_chunk(&self) -> &Chunk {
        return &self.chunks[self.current_func().chunk];
    }

    pub fn current_code(&self) -> &Vec<u8> {
        return &self.current_chunk().code;
    }

    pub fn current_constants(&self) -> &Vec<ConstValue> {
        return &self.current_chunk().constants;
    }

    pub fn get_upvalue(&self, ua: UpvalueAddress) -> &Upvalue {
        return &self.frames[ua.frame].get_closure().upvalues[ua.upvalue];
    }

    pub fn get_upvalue_mut(&mut self, ua: UpvalueAddress) -> &mut Upvalue {
        return &mut self.frames[ua.frame].get_closure().upvalues[ua.upvalue];
    }

    /**
     * Trivial upvalues are local upvalues or previously resolved.
     * See get_or_set_upvalue for explanation of the parameters.
     */
    fn resolve_trivial_upvalue (&mut self,ua: UpvalueAddress,v:Option<Value>) -> Option<Value> {
        let uv = self.get_upvalue(ua);
        if uv.is_closed() {
            if v.is_some() {
                //set operation
                let uv = self.get_upvalue_mut(ua);
                uv.set(v.unwrap());
            } else {
                //get operation
                return Some(uv.get());
            }
        } else if uv.is_local() {
            //if uv is local then the upvalue can be resolved
            //in our parent frame. This means it's a local
            //variable there. This code grabs that local.
            let frameno = ua.frame - 1;
            let stack_base = self.frames[frameno].sp;
            let stack_ofs = stack_base + uv.slot as usize;
            if let Some(va) = v {
                self.stack[stack_ofs] = va;
            } else {
                return Some(self.stack[stack_ofs].clone());
            }
        }
        None
    }
    /** 
     * Resolve uv, if v is set, set upvalue to contain v
     * if v is not set, return a copy of the value inside the upvalue
     */
    pub fn get_or_set_upvalue(&mut self, ua:UpvalueAddress, v: Option<Value>) -> Option<Value> {
        let uv = self.get_upvalue_mut(ua);
        if uv.is_closed() {
            if let Some(set_val) = v {
                //set operation
                uv.set(set_val);
                return None;
            } else {
                //get operatoin
                return Some(uv.get());
            }
        }
        //the upvalue is open, we need to traverse up the framestack now.
        let uv = self.get_upvalue(ua);
        //global frame, outer function, inner function
        assert!(self.frames.len() >= 3);
        let innermost_frame = self.frames.len() - 1;
        let mut cur_frame_no = innermost_frame;
        let mut slot = uv.slot;
        //don't traverse into the global frame. U cant
        //resolve upvalues to globals!
        while cur_frame_no > 0 {
            let cur_frame = &self.frames[cur_frame_no];

            //Get which function this frame belongs to
            let cur_clos = cur_frame.get_closure();

            //Get the upvalue from the function.
            let u = &cur_clos.upvalues[slot as usize];
            if u.is_local() || u.is_closed() {
                //cant resolve in global space
                assert!(cur_frame_no != 1);
                return self.resolve_trivial_upvalue(UpvalueAddress::new(cur_frame_no,slot as usize),v);
            }
            //slot is the upvalue index in the current frame. We will go to the next frame where
            //the upvalue slot is different.
            slot = u.slot;
            cur_frame_no -= 1;
        }
        //somehow we failed resolution on an upvalue which passed name resolution.
        //Compiler bug!
        unreachable!();
    }


    pub fn interpret(&mut self) -> InterpretResult {
        use InterpretResult::*;
        use Opcode::*;
        let mut i = 0;
        //start at the main function
        let cl = Closure::new(self,0);
        let v = self.new_closure(cl);
        self.frames.push(Frame { sip: 0, closure:v, sp: 0 });
        let mut reset_ip = false;
        let mut skip_increment = false;
        loop {
            if i >= self.current_code().len() {
                panic!("missing return");
            }
            let opc = self.current_code()[i];
            let op = Opcode::from_u8(opc);
            println!("About to execute {:?}",op);
            //TODO: Clean up all of this spammy garbage and get rid of CompileError
            match op {
                OP_RETURN => { 
                    let v = self.pop_stack();
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
                    self.push_stack(v);
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

                    let new_value = self.current_constants()[const_index].clone();
                    let v = self.new_value_from_iv(new_value.iv);
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
                OP_GET_UPVALUE => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
               
                    let upval_index = self.current_code()[i] as usize;
                    let framedex = self.frames.len() - 1;
                    let val_ref = self.get_or_set_upvalue(UpvalueAddress::new(framedex,upval_index),None).unwrap();
                    self.stack.push(val_ref);
                }
                OP_SET_UPVALUE => {
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    i += 1;
                    let v = self.pop_stack();

                    let upval_index = self.current_code()[i] as usize;
                    let framedex = self.frames.len() - 1;

                    let _ = self.get_or_set_upvalue(UpvalueAddress::new(framedex,upval_index),Some(v));
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

                OP_CLOSURE => { 
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    //TODO: Fix validation
                    i+=1;
                    let func_index = self.current_code()[i] as usize;

                    assert!(func_index < self.funcs.len());
                    let cl = self.create_closed_closure(func_index);
                    let nb = self.new_closure(cl);
                    self.push_stack(nb);

                }
                OP_CALL => { 
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                    }
                    let v = self.pop_stack();
                    if !v.is_closure() {
                        return RuntimeError;
                    }
                    let c = v.get_closure();
                    let findex = c.func;
                    let func = &self.funcs[findex];

                    let arity = func.arity;
                    //TODO: BRAIN ROT?
                    if self.stack_len() < arity {
                        return CompileError;
                    }
                    //create a new frame, return when complete
                    //Are we supposed to clone the closure here?
                    self.frames.push(Frame { sip:i,closure: v, sp: self.stack_len()});
                    reset_ip = true;

                }
                OP_ADD | OP_SUBTRACT | OP_MULTIPLY | OP_DIVIDE => {
                    let op_fn = self.get_fn(op);
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    if v1.is_num() && v2.is_num() {
                        let f1 = v1.get_num();
                        let f2 = v2.get_num();
                        let res = op_fn(f2,f1);
                        let nb = self.new_num(res);
                        self.push_stack(nb);
                    }
                    if v1.is_str() && v2.is_str() {
                        let s1 = v1.get_str();
                        let s2 = v2.get_str();
                        let nb = self.new_str(s2.to_owned() + &s1);
                        self.push_stack(nb);
                    }
                    return RuntimeError;
                }
                OP_EQUAL => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    let nb = self.new_bool(v1 == v2);
                    self.push_stack(nb);
                }
                OP_GREATER | OP_LESS => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let v2 = self.pop_stack();

                    if v1.is_num() && v2.is_num() {
                        let f1 = v1.get_num();
                        let f2 = v2.get_num();
                        if op == OP_GREATER {
                            let nb = self.new_bool(f2 > f1);
                            self.push_stack(nb);
                        } else if op == OP_LESS {
                            let nb = self.new_bool(f2 < f1);
                            self.push_stack(nb);
                        } else {
                            panic!("whut");
                        }
                    }
                    return RuntimeError;
                }
                OP_NIL => {
                    let nb = self.new_nil();
                    self.push_stack(nb);
                }
                OP_TRUE => {
                    let nb = self.new_bool(true);
                    self.push_stack(nb);
                }
                OP_FALSE => {
                    let nb = self.new_bool(false);
                    self.push_stack(nb);
                }
                OP_NEGATE => {
                    if self.stack_len() < 1 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();

                    if v1.is_num() {
                        let a = v1.get_num();
                        let nb = self.new_num(-1.0*a);
                        self.push_stack(nb);
                    } else {
                        return RuntimeError;
                    }
                }
                OP_NOT => {
                    if self.stack_len() < 1 {
                        return CompileError;
                    }
                    let v1 = self.pop_stack();
                    let nb = self.new_bool(is_falsey(&v1));
                    self.push_stack(nb);
                }
                OP_AND | OP_OR => {
                    if self.stack_len() < 2 {
                        return CompileError;
                    }
                    let v2 = self.pop_stack();
                    let v1 = self.pop_stack();
                    if v1.is_bool() && v2.is_bool() {
                        let a = v1.get_bool();
                        let b = v2.get_bool();
                        if op == OP_AND {
                            let nb = self.new_bool(a && b);
                            self.push_stack(nb);
                        } else if op == OP_OR {
                            let nb =self.new_bool(a || b);
                            self.push_stack(nb);
                        } else {
                            panic!("whut");
                        }
                        return RuntimeError;
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
                OP_WHILE =>{
                    if i + 1 >= self.current_code().len() {
                        return CompileError;
                   }
                    i+=1;
                    let jump_offset: usize = self.current_code()[i] as usize;
                    let cond = self.pop_stack();
                    if is_falsey(&cond) {
                        i+= jump_offset;
                        skip_increment = true;
                   }
                }
                OP_LOOP =>{
                    if i + 1 >= self.current_code().len() {
                       return CompileError;
                   }
                    let jump_offset = self.current_code()[i+1] as usize;
                    // We have < OP_CODE , VALUE >, the jump_offset will land at 
                    // VALUE, which is not an op code, the + 1 makes sure that we
                    // land on OP_CODE as we intend
                    i -= jump_offset + 1;
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
    pub constants: Vec<ConstValue>,
}


type FBinOp = fn(f64,f64) -> f64;
impl Chunk {
    pub fn new() -> Chunk {
        return Chunk { code:vec!(),constants: vec!()};
    }
    pub fn add_call(&mut self) {
        self.code.push(Opcode::OP_CALL as u8);
    }
    pub fn add_closure(&mut self,findex:usize) {
        self.code.push(Opcode::OP_CLOSURE as u8);
        self.code.push(findex as u8);
    }
    pub fn add_pop(&mut self) {
        self.code.push(Opcode::OP_POP as u8);
    }
    pub fn add_const_num(&mut self,v:f64) {
        let mut cv = ConstValue::new();
        cv.set_num(v);
        self.constants.push(cv);
        let index = self.constants.len()-1;
        self.code.push(Opcode::OP_CONSTANT as u8);
        self.code.push(index as u8);
    }
    pub fn add_const_str(&mut self, v: &String) {
        let mut cv = ConstValue::new();
        cv.set_str(v.clone());
        self.constants.push(cv);
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
    pub fn add_get_upvalue(&mut self, ofs: u8) {
        self.code.push(Opcode::OP_GET_UPVALUE as u8);
        self.code.push(ofs);
    }
    pub fn add_set_upvalue(&mut self, ofs: u8) {
        self.code.push(Opcode::OP_SET_UPVALUE as u8);
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
    pub fn add_while(&mut self, ofs: u8) -> usize {
        self.code.push(Opcode::OP_WHILE as u8);
        self.code.push(ofs);
        return self.code.len() -1;
    }
    pub fn add_loop(&mut self, pos: u8) {
        self.code.push(Opcode::OP_LOOP as u8);
        self.code.push(pos);
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

                    let v = &self.constants[const_index];
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
                OP_GET_UPVALUE => {
                    write!(f,"OP_GET_UPVALUE\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let global_index = self.code[i] as usize;

                    write!(f,"{}",global_index)?; 
                } 
                OP_SET_UPVALUE => {
                    write!(f,"OP_SET_UPVALUE\t")?;
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
                OP_CLOSURE => { 
                    write!(f,"OP_CLOSURE\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let func_index = self.code[i] as usize;

                    write!(f,"{}",func_index)?;
                }
                OP_CALL => { 
                    write!(f,"OP_CALL\t")?;
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
                OP_WHILE => {
                    write!(f,"OP_WHILE\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let global_index = self.code[i] as usize;
                    write!(f,"{}",global_index)?; 
                }
                OP_LOOP => {
                    write!(f,"OP_LOOP\t")?;
                    if i + 1 >= self.code.len() {
                        write!(f," WTFEOF")?;
                    }
                    i+=1;
                    let global_index = self.code[i] as usize;                     
                    write!(f,"{}",global_index)?; 
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

