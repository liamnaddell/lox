pub struct VariableDefinition {
    /// This is the location of the variable on the stack.
    /// Only relevant for Local variables
    pub stack_location: u32,
    /// The Node that does the definition (Must be a `var a = x` definition)
    def_node: NodeId,
}


pub enum UseKind {
    Local,
    Global,
    Upvalue
}
struct VariableUse {
    /// How is this variable used
    use_kind: UseKind,
    /// Where is the variable used
    use_loc: NodeId,
    /// Note, to get the VariableDefinition stack_location, key this into VariablePass.defs
    what_used: NodeId,
}

struct NRStackFrame {
    HashMap<String,NodeId> defs,
}

impl NRStackFrame {
    fn new() -> Self {
        return NRStackFrame { defs: HashMap::new() };
    }
    /// Check if the variable is defined in the current scope
    fn find_name(&self, s: &str) -> Option<NodeId> {
        return self.defs.get(s);
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
    fn find(&self, name: &str) -> (NodeId, UseKind) {
        //Tracks what frame we are on
        let mut i = 0;
        // When iterating in reverse order, this is the "first" frame or "global variable" frame
        let global_frame = self.frames.len() - 1;
        // Reverse-iterate through the frames looking for a definition
        while i < global_frame {
            if Some(def_nodeid) = self.frames[i].find_name(name) {
                let use_kind = if i == global_frame {
                    UseKind::Global
                } else if i == 0 {
                    UseKind::Local
                } else {
                    UseKind::Upvalue
                }
                return (def_nodeid,use_kind);
            }
            i += 1;
        }
        return None;
    }
    ///push a new name onto the stack. Errors if trying to push the same
    ///name in the same context multiple times.
    fn push(&mut self, name: &str, nodeid: NodeId) {
        let relevant_frame = &self.defs[self.defs.len() - 1];

        let good = relevant_frame.add_name(name,nodeid);

        if !good {
            panic!("Error handling add");
        }
    }
    fn push_context() {
        self.frames.push(NRStackFrame::new());
    }
    fn pop_context() {
        assert!(self.frames.len() != 0)
        self.frames.pop();
    }
    fn new() -> Self {
        let nr = NRStack { frames: vec!() };
        //add context for the global scope
        nr.push_context();
        return nr;
    }

}

struct VariablePass {
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
    fn resolve_name(&mut self, name: &str, nodeid_to_resolve: NodeId) {
        // This means we tried to resolve the same nodeid twice!
        assert!(!self.refs.contains_key(nodeid_to_resolve));

        // Check that the name can be resolved
        let Some(def_nodeid,use_kind) = self.names_in_use.find(v.name) else {
            panic!("Add error message");
        }

        //Create-and-insert a use for this variable.
        let vu = VariableUse { use_kind: use_kind
            , use_loc: nodeid_to_resolve
            , what_used: def_nodeid
        }
        self.refs.insert(nodeid_to_resolve,vu);
    }
}

impl AstCooker for VariablePass {
    fn visit_function(&mut self, f: &FnDecl) {
        self.is_local = true;
        // We allow for recursion!
        self.names_in_use.push(f.name,f.nodeid);
        self.default_visit_function(f);
        self.is_local = false;
    }

    fn visit_var(&mut self, v: &VarDecl) {
        if self.is_local {
            let variable_stack_loc = self.current_stack_loc;
            self.current_stack_loc += 1;
            self.add_local(stack_location,v.nodeid);
        } else {
            self.add_global(v.nodeid);
        }
        self.names_in_use.push(v.name,v.nodeid);
        
    }

    fn visit_block(&mut self, b: &Block) {
        //Add a new lexical scope for name resolution
        self.names_in_use.push_context();
        self.default_visit_block(b);
        self.names_in_use.pop_context();
    }

    fn visit_assignment(&mut self, a: &Assignment) {
        //TODO: var a = a;
        self.resolve_name(&a.var_name,a.nodeid);
        self.default_visit_assignment(a);
    }

    fn visit_literal(&mut self, l: &Literal) {
        if let Identifier(id) = l.kind {
            self.resolve_name(&id,l.nodeid);
        }
    }
}
