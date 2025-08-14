use std::fmt;
//TODO: This must be removed
use std::mem::ManuallyDrop;
use std::alloc;
use crate::bc::VM;

#[repr(C)]
#[derive(PartialEq,Clone,Copy,Debug)]
enum ValueTag {
    Str,
    Num,
    Bool,
    Nil,
    Closure,
}


#[derive(Clone)]
pub struct Upvalue {
    /** 
     * Is the upvalue in our direct parent function? 
     * Or do we need to go to other frames to get to
     * her 
     */
    is_local: bool,
    /**
     * If our upvalue is in our direct parent function, this is the offset
     * If our upvalue is in her enclosing function, this is the index into our
     * parent's `upvalues` array 
     */
    pub slot: u32,
    //TODO: Make this not pub
    pub closed_value: Option<Value>,
}

/** This is how Upvalues are stored inside closures.
 * When executing OP_CLOSURE, all the Upvalues
 * will be closed. Before that point, we need to traverse
 * up the function stack to access an upvalue
 */

impl Upvalue {
    pub fn is_closed(&self) -> bool {
        self.closed_value.is_some()
    }
    pub fn is_local(&self) -> bool {
        self.is_local
    }
    pub fn set(&mut self, v: Value) {
        self.closed_value = Some(v)
    }
    pub fn get(&self) -> Value {
        let Some(ref v) = self.closed_value else {
            unreachable!();
        };

        return *v;
    }
    pub fn new(is_local: bool, slot: u32) -> Upvalue {
        return Upvalue { is_local:is_local,slot:slot,closed_value:None};
    }
}


#[derive(Default)]
pub struct Closure {
    pub func: usize,
    //TODO: Not implemented by us >:)
    pub upvalues: Vec<Upvalue>,
}

#[repr(C)]
union ValueData {
    s: std::mem::ManuallyDrop<String>,
    n: f64,
    b: bool,
    null: u8,
    closure: *mut Closure,
}

#[repr(C)]
pub struct InnerValue {
    tag: ValueTag,
    vd: ValueData,
    pub mark: ValueMark,
}

impl Clone for InnerValue {
    fn clone(&self) -> Self {
        let mut iv2 = InnerValue::new();
        iv2.copy_from(self);
        return iv2;
    }
}

//TODO:
//2. Add a string table.
//3. bc clones closures when putting them into a function. Stop it! Remove the clone implementation
//   for innervalue, and replace it with an explicit make_copy api that does the following:
//   1. trivial types are copied
//4. Remove the dependency from ConstValue on InnerValue, and have the constvalue store the data
//   inline so it can't be impacted from GC. (Only relevant to `str`).
impl InnerValue {
    pub fn new() -> Self {
        return Self { tag: ValueTag::Nil
            , vd: ValueData { null: 0 }
            , mark: ValueMark::White 
        };
    }
    pub fn clear(&mut self) {
        //step 1: de-allocate inner data.
        match self.tag {
            ValueTag::Str => {
                unsafe {
                    let a: &mut ManuallyDrop<String> = &mut self.vd.s;
                    ManuallyDrop::drop(a);
                }
            },
            ValueTag::Closure => {
                unsafe {
                    let clp = self.vd.closure as *mut u8;
                    let layout = alloc::Layout::new::<Closure>();
                    alloc::dealloc(clp,layout);
                }
            },
            //stored inline. Don't do anything!
            ValueTag::Num | ValueTag::Bool | ValueTag::Nil  => { },
        }
        //step 2: reset to null
        self.tag = ValueTag::Nil;
    }
    pub fn copy_from(&mut self, _a: &InnerValue) {
        unimplemented!();
    }
    pub fn set_str(&mut self, a: String) {
        self.clear();
        self.tag = ValueTag::Str;
        self.vd.s = ManuallyDrop::new(a);
    }
    pub fn move_closure(&self) -> Closure {
        //this won't be pretty
        assert!(self.tag == ValueTag::Closure);
        unsafe {
            let cl_pointer: *mut Closure = self.vd.closure;
            let cl_mutref: &mut Closure = &mut *cl_pointer;
            //this converts a &mut Closure -> Closure by STEALING it's memory
            let new_cl: Closure = std::mem::take(cl_mutref);
            return new_cl;
        }


    }
    pub fn get_str(&self) -> &str {
        assert!(self.tag == ValueTag::Str);
        unsafe {
            return &self.vd.s;
        }
    }
    pub fn is_str(&self) -> bool {
        return self.tag == ValueTag::Str;
    }

    pub fn set_bool(&mut self, a: bool) {
        self.clear();
        self.tag = ValueTag::Bool;
        self.vd.b = a;
    }
    pub fn get_bool(&self) -> bool {
        assert!(self.tag == ValueTag::Bool);
        unsafe {
            return self.vd.b;
        }
    }
    pub fn is_bool(&self) -> bool {
        return self.tag == ValueTag::Bool;
    }
    
    pub fn set_num(&mut self, a: f64) {
        self.clear();
        self.tag = ValueTag::Bool;
        self.vd.n = a;
    }
    pub fn get_num(&self) -> f64 {
        assert!(self.tag == ValueTag::Num);
        unsafe {
            return self.vd.n;
        }
    }
    pub fn is_num(&self) -> bool {
        return self.tag == ValueTag::Num;
    }

    pub fn set_nil(&mut self) {
        self.clear();
    }
    pub fn is_nil(&self) -> bool {
        return self.tag == ValueTag::Nil;
    }

    pub fn set_closure(&mut self, a: Closure) {
        self.clear();
        //yea something cooked is gonna happen in here
        unsafe {
            //They really make this painful don't they :)
            let layout = alloc::Layout::new::<Closure>();
            let ptr = alloc::alloc(layout);
            if ptr.is_null() {
                panic!("alloc fails");
            }
            //coding rust is my passion
            let cl_ptr = ptr as *mut Closure;
            let cla = &mut *(cl_ptr);
            cla.func = a.func;
            cla.upvalues = a.upvalues;
            self.tag = ValueTag::Closure;
            self.vd.closure = cl_ptr;
        }
    }
    pub fn get_closure(&self) -> &mut Closure {
        assert!(self.tag == ValueTag::Closure);
        unsafe {
            return &mut *self.vd.closure;
        }
    }
    pub fn is_closure(&self) -> bool {
        return self.tag == ValueTag::Closure;
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum ValueMark {
    Black,
    White,
    Grey,
}

///A non-garbage-collected value
#[repr(C)]
#[derive(Clone)]
pub struct ConstValue {
    pub iv: InnerValue,
}
impl ConstValue {
    pub fn new() -> ConstValue {
        return ConstValue { iv: InnerValue::new() };
    }
    pub fn set_str(&mut self,a:String) {
            (self.iv).set_str(a);
    }
    pub fn get_str(&self) -> &str {
            return (self.iv).get_str();
    }

    pub fn is_str(&self) -> bool {
            return (self.iv).is_str();
    }

    pub fn set_bool(&mut self,a:bool) {
            (self.iv).set_bool(a);
    }
    pub fn get_bool(&self) -> bool {
            return (self.iv).get_bool();
    }

    pub fn is_bool(&self) -> bool {
            return (self.iv).is_bool();
    }

    pub fn set_num(&mut self,a:f64) {
            (self.iv).set_num(a);
    }
    pub fn get_num(&self) -> f64 {
            return (self.iv).get_num();
    }

    pub fn is_num(&self) -> bool {
            return (self.iv).is_num();
    }

    pub fn is_nil(&self) -> bool {
            return (self.iv).is_nil();
    }
}

/// A garbage-collected Value.
/// When this value is no longer reachable, it is fit for deallocation.
#[repr(C)]
#[derive(Clone,Copy)]
pub struct Value {
    iv: *mut InnerValue,
}
impl Value {
    pub fn new(iv: &InnerValue) -> Value {
        return Value { iv: iv as *const InnerValue as *mut InnerValue };
    }
    pub fn copy_from_const(&mut self,a:&ConstValue) {
        unsafe {
            (&mut *self.iv).copy_from(&a.iv);
        }
    }
    pub fn set_str(&self,a:String) {
        unsafe {
            (&mut*self.iv).set_str(a);
        }
    }
    pub fn move_closure(&self) -> Closure {
        unsafe {
            return (&*self.iv).move_closure();
        }
    }
    pub fn get_str(&self) -> &str {
        unsafe {
            return (&*self.iv).get_str();
        }
    }

    pub fn is_str(&self) -> bool {
        unsafe {
            return (&*self.iv).is_str();
        }
    }

    pub fn set_bool(&self,a:bool) {
        unsafe {
            (&mut*self.iv).set_bool(a);
        }
    }
    pub fn get_bool(&self) -> bool {
        unsafe {
            return (&*self.iv).get_bool();
        }
    }

    pub fn is_bool(&self) -> bool {
        unsafe {
            return (&*self.iv).is_bool();
        }
    }

    pub fn set_num(&self,a:f64) {
        unsafe {
            (&mut*self.iv).set_num(a);
        }
    }
    pub fn get_num(&self) -> f64 {
        unsafe {
            return (&*self.iv).get_num();
        }
    }

    pub fn is_num(&self) -> bool {
        unsafe {
            return (&*self.iv).is_num();
        }
    }


    pub fn set_closure(&self,a:Closure) {
        unsafe {
            (&mut*self.iv).set_closure(a);
        }
    }
    pub fn get_closure(&self) -> &mut Closure {
        unsafe {
            return (&*self.iv).get_closure();
        }
    }

    pub fn is_closure(&self) -> bool {
        unsafe {
            return (&*self.iv).is_closure();
        }
    }

    pub fn is_nil(&self) -> bool {
        unsafe {
            return (&*self.iv).is_nil();
        }
    }
}
impl fmt::Display for Value {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            return (&*self.iv).fmt(f);
        }
    }
}
impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return self.iv.fmt(f);
    }
}
impl fmt::Debug for InnerValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f,"{}",self);
    }
}

impl fmt::Display for InnerValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f,"");
        /*
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
        */
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        //bcs hywhy not
        unsafe {
            return self.iv == other.iv || (&*self.iv) == (&*other.iv)
        }
    }
}

impl PartialEq for InnerValue {
    fn eq(&self, other: &Self) -> bool {
        //bcs hywhy not
        if self.tag != other.tag {
            return false;
        }
        let tag = self.tag;
        unsafe {
            match tag {
                ValueTag::Str => {
                    return self.vd.s == other.vd.s;
                },
                ValueTag::Num => {
                    return self.vd.n == other.vd.n;
                },
                ValueTag::Bool => {
                    return self.vd.b == other.vd.b;
                },
                ValueTag::Nil => {
                    return true;
                },
                ValueTag::Closure => {
                    //TODO: Idk what i'm supposed to be have done hwere
                    unreachable!();
                },
            }
        }
    }
}

impl fmt::Display for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"(is_local: {}, slot: {}, closed_value: {})",self.is_local,self.slot,self.closed_value.is_some())
    }
}
impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "func: {}, upvalues: [", self.func,)?;
        for uv in self.upvalues.iter() {
            write!(f,"{},",uv)?;
        }
        write!(f,"\n")

    }
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f,"{}",self);

    }
}

impl Closure {
    pub fn new(vm: &VM, findex: usize) -> Self {
        return Closure { func: findex, upvalues: vm.funcs[findex].upvalues_template.clone() };
    }
}

/** We need to impl PartialEq because Closures are values */
impl PartialEq for Closure {
    fn eq(&self, oth: &Self) -> bool {
        return self.func == oth.func;
    }
}

