use crate::parse::*;
use crate::bc::*;
use std::collections::HashMap;

/**
 * Couldn't really come up with a better name...
 * It cooks something at each level of the ast
 *
 * It could either recurse on its own, or it could rely on another
 * struct for recursion, i.e. it calls visit_function at a function, etc.
 */
    //TODO: Error handling needs to be done
    //TODO: Code must be TODO free, and warning free, with all tests passing.
    //TODO: Code must be panic-free
pub trait AstCooker {
    pub fn visit_function(&mut self, &FnDecl) { }
    pub fn visit_block(&mut self, &Literal) { }
    pub fn visit_print(&mut self, &Print) { }
    pub fn visit_var(&mut self, &VarDecl) { }
    pub fn visit_if(&mut self, &If) { }
    pub fn visit_while(&mut self, &While) { }
    pub fn visit_literal(&mut self, &Literal) { }
    pub fn visit_unary(&mut self, &Unary) { }
    pub fn visit_call(&mut self, &Call) { }
    pub fn visit_binary(&mut self, &Binary) { }
    pub fn visit_assignment(&mut self, &Assignment) { }
    pub fn visit_expr(&mut self, &Expr) { }
    pub fn visit_stmt(&mut self, &Stmt) { }
    pub fn visit_decl(&mut self, &Decl) { }
    pub fn visit_return(&mut self, &Return) { }
    pub fn visit_program(&mut self, &Program) { }
    pub fn visit_class(&mut self, &ClassDecl) { }
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

pub struct CompilePass {
    cnks: Vec<Chunk>,
    funcs: Vec<Function>,
    globals: Vec<Value>,
    /** 
     * The chunk we are compiling code into,
     * changes when we start compiling a different function
     */
    current_chunk: usize,

    /** used for allocating ct_name_to_id slots */
    next_id: u64,
    /**A bijection of variable/function names and their associated id's */
    ct_name_to_id: HashMap<String,u64>,
    /** A mapping from variable id to index in the `globals` array */
    ct_global_id_to_index: HashMap<u64,usize>,
    ct_stack: CTStack,

}
impl CompilePass {
    pub fn new() -> Self {
        return CompilePass {
            cnks: vec!()
            ct_name_to_id:HashMap::new(),
            ct_global_id_to_index: HashMap::new(),
            globals:vec!(),
            ct_stack: CTStack::new(),
            ct_name_to_id: HashMap::new(),
            current_chunk:0,
            funcs: vec!(),
            next_id:1
        }
    }
    pub fn add_new_chunk(&mut self) -> &Chunk {
        let cnk = Chunk::new();
        let cnk_index = self.cnks.len();
        self.cnks.push(cnk);
        //don't increment when resolving current chunk
        //TODO: Explain better
        if cnk_index != 0 {
            self.current_chunk+=1;
        }
        return &self.cnks[cnk_index];
    }
    pub fn current_chunk(&mut self) -> &mut Chunk {
        return &mut self.cnks[self.current_chunk];
    }
    pub fn display_bc(&self) {
        todo!();
    }
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
    pub fn ct_get_function_index(&mut self, name: &str) -> Option<usize> {
        let id = self.ct_get_id_of_var(name);
        let index = self.ct_get_global_index(id);
        return index;
    }

    pub fn ct_get_global_index(&self, id:u64) -> Option<usize> {
        let v = self.ct_global_id_to_index.get(&id);
        return v.copied();
    }
    pub fn ct_add_function(&mut self, name: String, func: Function) {
        self.funcs.push(func);
        let id = self.ct_get_id_of_var(&name);
        let ind = self.funcs.len() - 1;
        self.ct_global_id_to_index.insert(id, ind);
    }

    pub fn emit_create_var(&mut self, ch: &mut Chunk, var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        //NOTE: Technically not useful for global variables...
        let ofs = self.ct_add_decl(id);
        let ch = self.current_chunk();
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
        let (ofs,_decl) = self.ct_get_stack_offset_of_decl(id).expect("ICE: reference to id which does not exist");
        assert!(false);
        if ofs == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            ch.add_get_global(index);
            return;
        }
        ch.add_get_local(ofs);
    }
}
impl AstCooker for CompilePass {
    pub fn visit_function(&mut self, f: &FnDecl) { 
        let ch=self.current_chunk();
        let name = f.name.clone();
        let mut sub_cnk = Chunk::new();
        self.ct_push_scope();
        for arg in &f.args {
            //we don't actually SET the variable, just DECLARE it since it's on the STACK
            //self.emit_create_var(&mut sub_cnk,&arg);
            let id = self.ct_get_id_of_var(&arg);
            //lmao
            let _ofs = self.ct_add_decl(id);
        }
        self.visit_block(f.fn_def);
        sub_cnk.add_return();

        let func = Function { chunk:sub_cnk, arity: f.args.len()};

        self.ct_pop_scope();
        self.ct_add_function(name,func);
    }
    pub fn visit_block(&mut self, b: &Block) { 
        let ch=self.current_chunk();
        self.ct_push_scope();
        for stmt in &b.decls {
            self.visit_stmt(stmt);
        }
        self.ct_pop_scope();
    }
    pub fn visit_print(&mut self, &Print) { todo!(); }
        let ch=self.current_chunk();
    pub fn visit_var(&mut self, v:&VarDecl) { 
        let ch=self.current_chunk();
        self.visit_expr(v.value);
        self.emit_create_var(ch,&v.name);
    }
    pub fn visit_if(&mut self, i: &If) { 
        let ch=self.current_chunk();
        // emit expr:
        self.visit_expr(i.cond);
        // emit the op (then jump)
        let then = ch.add_jump_if(0xff);
        //emit stmt
        self.visit_expr(i.then);
        ch.add_pop();
        
        match &i.or_else {
            None => {
                self.patch_jump(ch, then);
            },
            Some(s) => {
                let or_else = ch.add_jump_else(0xff);
                self.patch_jump(ch, then);
                self.visit_expr(s);
                self.patch_jump(ch, or_else);
            }
        }
    }
    pub fn visit_while(&mut self, &While) { 
        let ch=self.current_chunk();
        todo!();
    }
    pub fn visit_literal(&mut self, l: &Literal) { 
        let ch=self.current_chunk();
        use LitKind::*;
        match l.kind {
            StringLit(ref s) => {
                ch.add_const_str(s);
            }
            Identifier(ref i) => {
                self.emit_get_var(ch,i);
            }
            NumberLit(num) => {
                ch.add_const_num(num);
            }
            True => {
                ch.add_true();
            }
            False => {
                ch.add_false();
            }
            Nil => {
                ch.add_nil();
            }
        }
    }
    pub fn visit_unary(&mut self, u: &Unary) { 
        let ch=self.current_chunk();
        use UnaryOp::*;
        self.visit_expr(u.sub);
        match &u.op {
            //subtract
            Sub => {
                ch.add_negate();
            }
            // !a
            Neg => {
                ch.add_not();
            }
        }
    }
    pub fn visit_call(&mut self, c: &Call) { 
        let ch=self.current_chunk();
        let findex = self.ct_get_function_index(&c.fn_name);
        let Some(findex) = findex else {
            panic!("ya that function doesnt exist buddy");
        };
        let funct = &self.funcs[findex];
        if c.args.len() != funct.arity {
            panic!("Arity mismatch :/, expected {} args got {}  now i die", funct.arity,c.args.len());

        }
        for arg in &c.args {
            self.visit_expr(arg);
        }
        ch.add_call(findex);
        for _ in 0..c.args.len() {
            //we gotta pop these bad boys off the stack once we are done.
            ch.add_pop();
        }
    }
    pub fn visit_binary(&mut self, b: &Binary) { 
        let ch=self.current_chunk();
        use BinOp::*;
        self.visit_expr(b.left);
        self.visit_expr(b.right);
        match b.op {
            Minus => {ch.add_sub()}
            Plus => {ch.add_add()}
            Star => {ch.add_mul()}
            BangEqual => {ch.add_equal();ch.add_not()}
            EqualEqual => {ch.add_equal()}
            Greater => {ch.add_greater()}
            GreaterEqual => {ch.add_less();ch.add_not()}
            Less => {ch.add_less()}
            LessEqual => {ch.add_greater();ch.add_not()}
            And => {ch.add_and()}
            Or => {ch.add_or()}
            Slash => {ch.add_div()}
        }
    }
    pub fn visit_assignment(&mut self, a: &Assignment) { 
        let ch=self.current_chunk();
        self.visit_expr(a.val_expr);

        assert!(false)
        let id = self.ct_get_id_of_var(&a.var_name);
        let Some((ofs,decl)) = self.ct_get_stack_offset_of_decl(id) else {
            assert!(false,"TODO: Error handling");
            return;
        };
        if ofs == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            ch.add_set_global(index);
            //exprs must return something.
            ch.add_get_global(index);
            return;
        }
        ch.add_set_local(ofs);
        ch.add_get_local(ofs);
    }
    pub fn visit_expr(&mut self, e: &Expr) {
        let ch=self.current_chunk();
        match e {
            Expr::Literal(ref l) => {
                self.visit_literal(l);
            }
            Expr::Unary(ref u) => {
                self.visit_unary(u);
            }
            Expr::Binary(ref b) => {
                self.visit_binary(b);
            }
            Expr::Call(ref c) => {
                self.visit_call(c);
            }
            Expr::Assignment(ref a) => {
                self.visit_assignment(a);
            }
        }
    }
    pub fn visit_stmt(&mut self, s: &Stmt) {
        let ch=self.current_chunk();
        match s {
            Stmt::Print(ref p) => {
                self.visit_expr(p.to_print);
                ch.add_print();
            }
            Stmt::Expr(ref e) => {
                self.visit_expr(e);
                //Expressions return a value on the stack, we discard this value.
                ch.add_pop();
            }
            Stmt::If(ref i) => {
                self.visit_if(i)
            }
            Stmt::Block(ref b) => {
                self.visit_block(b);
            }
            _ => { todo!() }
        }
    }
    pub fn visit_decl(&mut self, d: &Decl) {
        let ch=self.current_chunk();
        match d {
            Decl::Stmt(ref s) => {
                self.visit_stmt(s);
            }
            Decl::VarDecl(ref b) => {
                self.visit_var(b);
            }
            Decl::FnDecl(ref fnd) => {
                self.visit_function(fnd);
            }
            Decl::ClassDecl(ref fnd) => {
                self.visit_class(fnd);
            }
        }
    }
    pub fn visit_return(&mut self, &Return) { todo!(); }
        let ch=self.current_chunk();
    pub fn visit_program(&mut self, p: &Program) { 
        let ch=self.current_chunk();
        for decl in p.decls {
            self.visit_decl(decl);
        }
        ch.add_return();
    }

    pub fn visit_class(&mut self, &ClassDecl) {
        todo!();
    }
}
