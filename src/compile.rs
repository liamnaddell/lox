use crate::bc::*;
use std::collections::HashMap;
use crate::ast::*;
use crate::ast;

/**
 * Couldn't really come up with a better name...
 * It cooks something at each level of the ast
 *
 * It could either recurse on its own, or it could rely on another
 * struct for recursion, i.e. it calls visit_function at a function, etc.
 */
pub trait AstCooker {
    fn visit_function(&mut self, _: &FnDecl) { }
    fn visit_block(&mut self, _: &Block) { }
    #[allow(dead_code)]
    fn visit_print(&mut self, _: &Print) { }
    fn visit_var(&mut self, _: &VarDecl) { }
    fn visit_if(&mut self, _: &If) { }
    #[allow(dead_code)]
    fn visit_while(&mut self, _: &While) { }
    fn visit_literal(&mut self,_:  &Literal) { }
    fn visit_unary(&mut self, _: &Unary) { }
    fn visit_call(&mut self, _: &Call) { }
    fn visit_binary(&mut self, _: &Binary) { }
    fn visit_assignment(&mut self, _: &Assignment) { }
    fn visit_expr(&mut self, _: &Expr) { }
    fn visit_stmt(&mut self, _: &Stmt) { }
    fn visit_decl(&mut self, _: &ast::Decl) { }
    #[allow(dead_code)]
    fn visit_return(&mut self, _: &Return) { }
    fn visit_program(&mut self, _: &Program) { }
    fn visit_class(&mut self, _: &ClassDecl) { }
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
    pub cnks: Vec<Chunk>,
    pub funcs: Vec<Function>,
    pub globals: Vec<Value>,
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
macro_rules! current_chunk {
    ($self:ident) => { &mut $self.cnks[$self.current_chunk] }
}
impl CompilePass {
    pub fn new() -> Self {
        let cc = Chunk::new();
        let func = Function { chunk: 0, arity: 0 };
        return CompilePass {
            cnks: vec!(cc),
            ct_name_to_id:HashMap::new(),
            ct_global_id_to_index: HashMap::new(),
            globals:vec!(),
            ct_stack: CTStack::new(),
            current_chunk:0,
            funcs: vec!(func),
            next_id:1
        }
    }
     fn patch_jump(&mut self, pos: usize) {
         let ch = current_chunk!(self);
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
    pub fn add_new_chunk(&mut self) ->usize {
        let cnk = Chunk::new();
        let cnk_index = self.cnks.len();
        self.cnks.push(cnk);
        //don't increment when resolving current chunk
        //TODO: Explain better
        if cnk_index != 0 {
            self.current_chunk+=1;
        }
        return cnk_index;
    }
    pub fn display_bc(&self) {
        for i in 0..self.funcs.len() {
            let func = &self.funcs[i];
            let ch = &self.cnks[func.chunk];
            println!("<func #{} {} {}",i,func,ch);
        }
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
        //TODO: lol
        self.ct_global_id_to_index.insert(id, ind);
    }

    pub fn emit_create_var(&mut self, var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        //NOTE: Technically not useful for global variables...
        let ofs = self.ct_add_decl(id);
        let ch = current_chunk!(self);
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

    pub fn emit_get_var(&mut self,var_name: &str) {
        let id = self.ct_get_id_of_var(var_name);
        let (ofs,decl) = self.ct_get_stack_offset_of_decl(id).expect("ICE: reference to id which does not exist");
        if decl.scope == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            let ch = current_chunk!(self);
            ch.add_get_global(index);
            return;
        }
        let ch = current_chunk!(self);
        ch.add_get_local(ofs);
    }
}
impl AstCooker for CompilePass {
    fn visit_function(&mut self, f: &FnDecl) { 
        let name = f.name.clone();
        let current_fn = self.current_chunk;
        let cnk_no = self.add_new_chunk();
        self.ct_push_scope();
        for arg in &f.args {
            //we don't actually SET the variable, just DECLARE it since it's on the STACK
            //self.emit_create_var(&mut sub_cnk,&arg);
            let id = self.ct_get_id_of_var(&arg);
            //lmao
            let _ofs = self.ct_add_decl(id);
        }
        self.visit_block(&f.fn_def);
        let sub_cnk = current_chunk!(self);
        sub_cnk.add_return();

        let func = Function { chunk:cnk_no, arity: f.args.len()};

        self.ct_pop_scope();
        //TODO: This is complete spaghet
        self.current_chunk = current_fn;
        self.ct_add_function(name,func);
    }
     fn visit_block(&mut self, b: &Block) { 
        self.ct_push_scope();
        for stmt in &b.decls {
            self.visit_decl(stmt);
        }
        self.ct_pop_scope();
    }
     fn visit_print(&mut self, _: &Print) { todo!(); }
     fn visit_var(&mut self, v:&VarDecl) { 
        self.visit_expr(&v.value);
        self.emit_create_var(&v.name);
    }
     fn visit_if(&mut self, i: &If) { 
        // emit expr:
        self.visit_expr(&i.cond);
        // emit the op (then jump)
        let ch=current_chunk!(self);
        let then = ch.add_jump_if(0xff);
        //emit stmt
        self.visit_stmt(&i.then);
        let ch=current_chunk!(self);
        ch.add_pop();
        
        match &i.or_else {
            None => {
                self.patch_jump(then);
            },
            Some(s) => {
                let or_else = ch.add_jump_else(0xff);
                self.patch_jump(then);
                self.visit_stmt(&s);
                self.patch_jump(or_else);
            }
        }
    }
     fn visit_while(&mut self, _: &While) { 
        todo!();
    }
     fn visit_literal(&mut self, l: &Literal) { 
        use ast::LitKind::*;
        let ch = current_chunk!(self);
        match l.kind {
            StringLit(ref s) => {
                ch.add_const_str(s);
            }
            Identifier(ref i) => {
                self.emit_get_var(i);
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
     fn visit_unary(&mut self, u: &Unary) { 
        use UnaryOp::*;
        self.visit_expr(&u.sub);
        let ch=current_chunk!(self);
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
     fn visit_call(&mut self, c: &Call) { 
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
        let ch=current_chunk!(self);
        ch.add_call(findex);
        for _ in 0..c.args.len() {
            //we gotta pop these bad boys off the stack once we are done.
            ch.add_pop();
        }
    }
     fn visit_binary(&mut self, b: &Binary) { 
        use BinOp::*;
        self.visit_expr(&b.left);
        self.visit_expr(&b.right);
        let ch=current_chunk!(self);
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
     fn visit_assignment(&mut self, a: &Assignment) { 
        self.visit_expr(&a.val_expr);

        assert!(false);
        let id = self.ct_get_id_of_var(&a.var_name);
        let Some((ofs,decl)) = self.ct_get_stack_offset_of_decl(id) else {
            assert!(false,"TODO: Error handling");
            return;
        };
        if decl.scope == 0 {
            let index = self.ct_get_global_index(id).expect("ICE");
            let ch=current_chunk!(self);
            ch.add_set_global(index);
            //exprs must return something.
            ch.add_get_global(index);
            return;
        }
        let ch=current_chunk!(self);
        ch.add_set_local(ofs);
        ch.add_get_local(ofs);
    }
     fn visit_expr(&mut self, e: &Expr) {
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
     fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Print(ref p) => {
                self.visit_expr(&p.to_print);
                let ch=current_chunk!(self);
                ch.add_print();
            }
            Stmt::Expr(ref e) => {
                self.visit_expr(e);
                let ch=current_chunk!(self);
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
     fn visit_decl(&mut self, d: &ast::Decl) {
        match d {
            ast::Decl::Stmt(ref s) => {
                self.visit_stmt(s);
            }
            ast::Decl::VarDecl(ref b) => {
                self.visit_var(b);
            }
            ast::Decl::FnDecl(ref fnd) => {
                self.visit_function(fnd);
            }
            ast::Decl::ClassDecl(ref fnd) => {
                self.visit_class(fnd);
            }
        }
    }
     fn visit_return(&mut self, _: &Return) { todo!(); }
     fn visit_program(&mut self, p: &Program) { 
        for decl in &p.decls {
            self.visit_decl(&decl);
        }
        let ch=current_chunk!(self);
        ch.add_return();
    }

     fn visit_class(&mut self, _: &ClassDecl) {
        todo!();
    }
}
