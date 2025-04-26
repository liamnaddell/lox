use crate::ast::*;
use std::collections::HashMap;
use std::sync::OnceLock;
use crate::bc;

struct VariablePassResults {
    vp: VariablePass,
}

static VPR: OnceLock<VariablePassResults> = OnceLock::new();

pub fn get_vpass() -> &'static VariablePass {
    //for single threaded access this is safe
    let Some(vpr) = VPR.get() else {
        unreachable!();
    };
    return &vpr.vp;

}

pub fn run_vpass(p: &Program) {
    let mut vp = VariablePass::new();
    vp.visit_program(p);
    let res = VPR.set(VariablePassResults { vp:vp});
    assert!(res.is_ok());
}


#[derive(Clone,Copy)]
pub struct VariableDefinition {
    /// This is the location of the variable on the stack.
    /// For global variables, this is the "index" of the global.
    /// For functions, this is the "index" of the function
    pub stack_location: u32,
    // The Node that does the definition (Must be a `var a = x` definition)
    def_node: NodeId,
    ///Was this declared in global scope or not?
    global: bool,
    is_func: bool,
}

impl VariableDefinition {
    pub fn is_global(&self) -> bool {
        return self.global;
    }
    pub fn is_local(&self) -> bool {
        return !self.global;
    }
    pub fn is_func(&self) -> bool {
        return self.is_func;
    }
}


#[derive(PartialEq,Clone,Copy)]
pub enum UseKind {
    Local,
    Global,
    /* is this a local upvalue or not */
    Upvalue(bool),
}

#[derive(Clone,Copy)]
pub struct VariableUse {
    /// How is this variable used
    pub use_kind: UseKind,
    /// Note, to get the VariableDefinition stack_location, key this into VariablePass.defs
    pub what_used: NodeId,
    /// If this is an upvalue, this is the slot.
    pub upvalue_slot: usize,
}

impl VariableUse {
    pub fn is_global(&self) -> bool { self.use_kind == UseKind::Global }
    pub fn is_local(&self) -> bool { self.use_kind == UseKind::Local }
    pub fn is_upvalue(&self) -> bool { 
        if let UseKind::Upvalue(_) = self.use_kind {
            return true;
        }
        return false;
    }
}

struct NRStackFrame {
    defs: HashMap<String,NodeId>,
}

impl NRStackFrame {
    fn new() -> Self {
        return NRStackFrame { defs: HashMap::new() };
    }

    /// Check if the variable is defined in the current scope
    fn find_name(&self, s: &str) -> Option<NodeId> {
        return self.defs.get(s).copied();
    }

    /// Add the variable name (and it's definition node) to the current 
    /// Returns the index of that declaration
    fn add_name(&mut self, s: &str,nodeid: NodeId) -> Option<u32> {
        // We already have this name declared in the current scope!
        if self.find_name(s).is_some() {
            return None;
        }
        let ret = Some(self.defs.len() as u32);
        self.defs.insert(s.to_string(),nodeid);
        return ret;
    }
}

struct NRStack {
    frames: Vec<NRStackFrame>,
}

impl NRStack {
    fn in_global_context(&self) -> bool {
        return self.frames.len() == 1;
    }
    /// Find a variable in the NRStack, returning the definition nodeid, 
    /// and the use kind
    fn find(&self, name: &str) -> Option<(NodeId, UseKind)> {
        //Tracks what frame we are on
        let mut i = self.frames.len() - 1;
        let local_frame = i;
        while i >= 0 {
            if let Some(def_nodeid) = self.frames[i].find_name(name) {
                let use_kind = if i == 0 {
                    UseKind::Global
                } else if i == local_frame {
                    UseKind::Local
                } else {
                    //The frame the upvalue is in is in our parent function.
                    //Needed for later.
                    let is_local = local_frame - i == 1;
                    UseKind::Upvalue(is_local)
                };
                return Some((def_nodeid,use_kind));
            }
            i -= 1;
        }
        return None;
    }

    ///push a new name onto the stack. Errors if trying to push the same
    ///name in the same context multiple times.
    fn push(&mut self, name: &str, nodeid: NodeId) -> u32 {
        let fno = self.frames.len() - 1;
        let relevant_frame = &mut self.frames[fno];

        let maybe_ofs = relevant_frame.add_name(name,nodeid);

        let Some(ofs) = maybe_ofs else {
            panic!("Error handling add");
        };
        return ofs;
    }

    fn push_context(&mut self) {
        self.frames.push(NRStackFrame::new());
    }

    fn pop_context(&mut self) {
        assert!(self.frames.len() != 0);
        self.frames.pop();
    }

    fn new() -> Self {
        let mut nr = NRStack { frames: vec!() };
        //add context for the global scope
        nr.push_context();
        return nr;
    }

}

/** 
 * This is how upvalues are tracked at compile time.
 */
struct Upvalue {
    what_used: NodeId,
    /** Is the upvalue a local variable in our parent function? */
    is_local: bool,
}

/**
 * Each function has a list of upvalues it (or it's descendent functions) use
 */
struct UpvalueList {
    upvalues: Vec<Upvalue>,
}

impl UpvalueList {
    pub fn new() -> Self {
        return UpvalueList { upvalues: vec!()};
    }

    pub fn add_upvalue(&mut self, what_used: NodeId,is_local: bool) -> usize {
        let idx = self.upvalues.len();
        self.upvalues.push(Upvalue { what_used, is_local });
        return idx;
    }

    pub fn create_or_get_upvalue(&mut self, what_used: NodeId,is_local:bool) -> usize {
        let mut i = 0;
        for uv in self.upvalues.iter() {
            if uv.what_used == what_used {
                return i;
            }
            i+=1;
        }
        return self.add_upvalue(what_used,is_local);
    }
}


pub struct VariablePass {
    /// A structure for tracking what variable names are currently defined.
    /// Functions like a stack, since variables have stack semantics.
    names_in_use: NRStack,
    /// Mapping of NodeId -> VariableDefinition
    defs: HashMap<NodeId,VariableDefinition>,
    /// Mapping of NodeId -> VariableUse
    /// To figure out "What does this Identifier refer to", do the following:
    /// 1. Lookup the id in refs to get the VariableUse
    /// 2. Get the appropriate declaration id from `VariableUse.use_loc`
    /// 3. Look that up in `VariablePass.defs`
    refs: HashMap<NodeId,VariableUse>,
    /// When we add a variable definition, we increment this.
    /// This is used to predict where local variables will land on the
    /// stack when they are defined
    current_stack_loc: u32,
    /// Are we currently adding local variable definitions? Or global variable
    /// definitions.
    is_local: bool,
    /// The number of global variables, used to give indexes to globals
    global_ctr: u32,
    /// The number of functions, used to give indexes to functions
    func_ctr: u32,
    /// A struct that tracks which upvalues are utilized by a given function.
    upvalues: HashMap<NodeId,UpvalueList>,
    /// This is used while traversing the AST to track which function we are at, and which
    /// functions we had to traverse through to get here. 
    /// The reason we need this is that if the innermost function references an upvalue,
    /// we need to copy that upvalue up to our parent function, so they know to store that
    /// upvalue if the function is returned by value.
    func_stack: Vec<NodeId>,

}


impl VariablePass {
    pub fn current_fn(&self) -> NodeId {
        return self.func_stack[self.func_stack.len() - 1];
    }
    pub fn new() -> Self {
        return VariablePass {
            names_in_use: NRStack::new(),
            refs: HashMap::new(),
            defs: HashMap::new(),
            current_stack_loc: 0,
            is_local: false,
            global_ctr: 0,
            // the 0th function is the "main" implicit function
            func_ctr: 1,
            func_stack: vec!(),
            upvalues: HashMap::new(),
        }
    }
    //TODO: Bad practice for vpass to depend on bc ? 
    pub fn get_upvalue_template(&self,nodeid: NodeId) -> Vec<bc::Upvalue> {
        let uvs = self.get_upvalues(nodeid);
        let mut tvs = vec!();
        let mut i = 0;
        for uv in uvs.upvalues.iter() {
            tvs.push ( bc::Upvalue::new(uv.is_local, i));
            i+=1;
        }
        return tvs;
    }
    fn resolve_name(&mut self, name: &str, nodeid_to_resolve: NodeId) {
        // This means we tried to resolve the same nodeid twice!
        assert!(!self.refs.contains_key(&nodeid_to_resolve));

        // Check that the name can be resolved
        let Some((def_nodeid,use_kind)) = self.names_in_use.find(&name) else {
            panic!("Name resolution on `{}` failed!", name);
        };

        let mut upvalue_slot = 0;
        if let UseKind::Upvalue(is_local) = use_kind {
            let function_to_resolve_in = self.current_fn();
            upvalue_slot = self.upvalues.get_mut(&function_to_resolve_in).unwrap().create_or_get_upvalue(def_nodeid,is_local);
        }


        //Create-and-insert a use for this variable.
        let vu = VariableUse { use_kind: use_kind
            //, use_loc: nodeid_to_resolve
            , what_used: def_nodeid
            , upvalue_slot
        };
        self.refs.insert(nodeid_to_resolve,vu);
    }
    pub fn get_def(&self, ni: NodeId) -> VariableDefinition {
        //Your program should crash if calling get_def on something that isn't a variable
        let vd = self.defs.get(&ni).unwrap();
        return *vd
    }
    pub fn get_usage(&self, ni: NodeId) -> (VariableUse,VariableDefinition) {
        //Your program should crash if calling get_usage on something that isn't a variable
        //Your program should crash if we forgot to name resolve required nodes as well.
        let vu = self.refs.get(&ni).unwrap();
        let vd = self.defs.get(&vu.what_used).unwrap();
        return (*vu,*vd)
    }
    /// Add a new local to the current scope
    fn add_local(&mut self, s:&str,nodeid: NodeId) {
        let stack_location = self.names_in_use.push(s,nodeid);
        let vd = VariableDefinition {
            stack_location: stack_location,
            def_node: nodeid,
            global: false,
            is_func: false,
        };
        self.defs.insert(nodeid,vd);
    }
    /// Add a new global to the current scope
    /// Must ONLY be called at global scope.
    fn add_global(&mut self, s:&str, nodeid: NodeId) {
        let stack_location = self.global_ctr;
        self.global_ctr += 1;

        let vd = VariableDefinition {
            stack_location: stack_location,
            def_node: nodeid,
            global: true,
            is_func: false,
        };
        self.names_in_use.push(s,nodeid);
        self.defs.insert(nodeid,vd);
    }
    fn get_upvalues(&self,nodeid: NodeId) -> &UpvalueList {
        return self.upvalues.get(&nodeid).unwrap();
    }
    /// Add a new function to the current scope
    fn add_func(&mut self, s: &str, nodeid: NodeId) {
        let stack_location = self.func_ctr;
        self.func_ctr += 1;

        let global = self.names_in_use.in_global_context();

        self.upvalues.insert(nodeid,UpvalueList::new());
        let vd = VariableDefinition {
            stack_location: stack_location,
            def_node: nodeid,
            global: global,
            is_func: true
        };
        self.names_in_use.push(s,nodeid);
        self.defs.insert(nodeid,vd);
    }
}

impl AstCooker for VariablePass {
    fn visit_function(&mut self, f: &FnDecl) {
        self.is_local = true;
        // We allow for recursion!
        self.add_func(&f.name,f.nodeid);
        self.names_in_use.push_context();
        for arg in f.args.iter() {
            //This needs to refer to a proper AST item, not the function
            self.add_local(&arg.arg_name,arg.nodeid);
        }
        self.func_stack.push(f.nodeid);
        self.recurse_function(f);
        self.func_stack.pop();
        self.names_in_use.pop_context();
        self.is_local = false;
    }

    fn visit_call(&mut self, c: &Call) {
        self.resolve_name(&c.fn_name,c.nodeid);
        self.recurse_call(c);
    }

    fn visit_var(&mut self, v: &VarDecl) {
        if self.is_local {
            self.add_local(&v.name,v.nodeid);
        } else {
            self.add_global(&v.name,v.nodeid);
        }
        self.recurse_var(v);
        
    }

    fn visit_block(&mut self, b: &Block) {
        //Add a new lexical scope for name resolution
        self.names_in_use.push_context();
        self.recurse_block(b);
        self.names_in_use.pop_context();
    }

    fn visit_assignment(&mut self, a: &Assignment) {
        //TODO: var a = a;
        self.resolve_name(&a.var_name,a.nodeid);
        self.recurse_assignment(a);
    }

    fn visit_literal(&mut self, l: &Literal) {
        if let LitKind::Identifier(id) = &l.kind {
            self.resolve_name(&id,l.nodeid);
        }
    }
}
