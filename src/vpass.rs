use crate::ast::*;
use std::collections::HashMap;

struct VariablePassResults {
    vp: Option<VariablePass>,
}

static mut VPR: VariablePassResults = VariablePassResults {
    vp: None,
};

pub fn get_vpass() -> &'static VariablePass {
    //for single threaded access this is safe
    unsafe {
    #[allow(static_mut_refs)]
    if let Some(v) = &VPR.vp {
        return v;
    }
    }
    unreachable!();
}

pub fn run_vpass(p: &Program) {
    let mut vp = VariablePass::new();
    vp.visit_program(p);
    //Assigning to allocated memory is safe
    unsafe {
        VPR.vp = Some(vp);
    }
}


#[derive(Clone,Copy)]
pub struct VariableDefinition {
    /// This is the location of the variable on the stack.
    /// For global variables, this is the "index" of the global.
    pub stack_location: u32,
    // The Node that does the definition (Must be a `var a = x` definition)
    //def_node: NodeId,
}


#[derive(PartialEq,Clone,Copy)]
pub enum UseKind {
    Local,
    Global,
    Upvalue
}

#[derive(Clone,Copy)]
pub struct VariableUse {
    /// How is this variable used
    pub use_kind: UseKind,
    // Where is the variable used
    //pub use_loc: NodeId,
    /// Note, to get the VariableDefinition stack_location, key this into VariablePass.defs
    pub what_used: NodeId,
}

impl VariableUse {
    pub fn is_global(&self) -> bool { self.use_kind == UseKind::Global }
    pub fn is_local(&self) -> bool { self.use_kind == UseKind::Local }
    pub fn is_upvalue(&self) -> bool { self.use_kind == UseKind::Upvalue }
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
    /// scope Returns `false` if the variable was already present
    fn add_name(&mut self, s: &str,nodeid: NodeId) -> bool {
        // We already have this name declared in the current scope!
        if self.find_name(s).is_some() {
            return false;
        }
        self.defs.insert(s.to_string(),nodeid);
        return true;
    }
}

struct NRStack {
    frames: Vec<NRStackFrame>,
}

impl NRStack {
    /// Find a variable in the NRStack, returning the definition nodeid, 
    /// and the use kind
    fn find(&self, name: &str) -> Option<(NodeId, UseKind)> {
        //Tracks what frame we are on
        let mut i = 0;
        // When iterating in reverse order, this is the "first" frame or "global variable" frame
        let global_frame = self.frames.len() - 1;
        // Reverse-iterate through the frames looking for a definition
        while i < global_frame {
            if let Some(def_nodeid) = self.frames[i].find_name(name) {
                let use_kind = if i == global_frame {
                    UseKind::Global
                } else if i == 0 {
                    UseKind::Local
                } else {
                    UseKind::Upvalue
                };
                return Some((def_nodeid,use_kind));
            }
            i += 1;
        }
        return None;
    }
    ///push a new name onto the stack. Errors if trying to push the same
    ///name in the same context multiple times.
    fn push(&mut self, name: &str, nodeid: NodeId) {
        let fno = self.frames.len() - 1;
        let relevant_frame = &mut self.frames[fno];

        let good = relevant_frame.add_name(name,nodeid);

        if !good {
            panic!("Error handling add");
        }
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
    is_local: bool
}


impl VariablePass {
    pub fn new() -> Self {
        return VariablePass {
            names_in_use: NRStack::new(),
            refs: HashMap::new(),
            defs: HashMap::new(),
            current_stack_loc: 0,
            is_local: false,
        }
    }
    fn resolve_name(&mut self, name: &str, nodeid_to_resolve: NodeId) {
        // This means we tried to resolve the same nodeid twice!
        assert!(!self.refs.contains_key(&nodeid_to_resolve));

        // Check that the name can be resolved
        let Some((def_nodeid,use_kind)) = self.names_in_use.find(&name) else {
            panic!("Add error message");
        };

        //Create-and-insert a use for this variable.
        let vu = VariableUse { use_kind: use_kind
            //, use_loc: nodeid_to_resolve
            , what_used: def_nodeid
        };
        self.refs.insert(nodeid_to_resolve,vu);
    }
    pub fn get_usage(&self, ni: NodeId) -> (VariableUse,VariableDefinition) {
        //Your program should crash if calling get_usage on something that isn't a variable
        //Your program should crash if we forgot to name resolve required nodes as well.
        let vu = self.refs.get(&ni).unwrap();
        let vd = self.defs.get(&vu.what_used).unwrap();
        return (*vu,*vd)
    }
    fn add_local(&mut self, _s:&str, _stack_location: u32,_nodeid: NodeId) {
        todo!()
    }
    fn add_global(&mut self, _s:&str, _nodeid: NodeId) {
        todo!()
    }
}

impl AstCooker for VariablePass {
    fn visit_function(&mut self, f: &FnDecl) {
        self.is_local = true;
        // We allow for recursion!
        self.names_in_use.push(&f.name,f.nodeid);
        self.recurse_function(f);
        self.is_local = false;
    }

    fn visit_var(&mut self, v: &VarDecl) {
        if self.is_local {
            let variable_stack_loc = self.current_stack_loc;
            self.current_stack_loc += 1;
            self.add_local(&v.name,variable_stack_loc,v.nodeid);
        } else {
            self.add_global(&v.name,v.nodeid);
        }
        self.names_in_use.push(&v.name,v.nodeid);
        
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
