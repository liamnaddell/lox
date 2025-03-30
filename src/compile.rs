use crate::bc::*;
use crate::ast::*;
use crate::ast;
use crate::vpass;

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
            globals:vec!(),
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
    pub fn ct_add_function(&mut self, _name: String, func: Function) {
        self.funcs.push(func);
    }

    pub fn emit_create_var(&mut self, v: &VarDecl) {
        let ch = current_chunk!(self);
        let vp = vpass::get_vpass();
        let var_def = vp.get_def(v.nodeid);

        if var_def.is_global() {
            //global var
            let index = var_def.stack_location;
            ch.add_set_global(index as usize);
        } else if var_def.is_local() {
            // We analyzed in the variable pass where this definition will be placed on the stack.
            ch.add_set_local(var_def.stack_location as u8);
        } else {
            //upvalue
            todo!();
        }
    }

    pub fn emit_get_var(&mut self,v: &ast::Literal) {
        let vp = vpass::get_vpass();
        let (var_use,var_def) = vp.get_usage(v.nodeid);

        if var_use.is_global() {
            let index = var_def.stack_location;
            let ch = current_chunk!(self);
            ch.add_get_global(index as usize);
            return;
        }
        //not implemented
        assert!(var_use.is_local());
        let ch = current_chunk!(self);
        ch.add_get_local(var_def.stack_location as u8);
    }
}
impl AstCooker for CompilePass {
    fn visit_function(&mut self, f: &FnDecl) { 
        let name = f.name.clone();
        let current_fn = self.current_chunk;
        //NOTE: We don't have to do anything for variables, since they will be pushed onto the
        //stack by the caller.
        let cnk_no = self.add_new_chunk();
        let func = Function { chunk:cnk_no, arity: f.args.len()};
        self.ct_add_function(name,func);

        self.visit_block(&f.fn_def);
        let sub_cnk = current_chunk!(self);
        sub_cnk.add_return();


        //TODO: This is complete spaghet
        self.current_chunk = current_fn;
    }
     fn visit_block(&mut self, b: &Block) { 
        for stmt in &b.decls {
            self.visit_decl(stmt);
        }
    }
     //TODO: Rework this to using recurse_*
     fn visit_print(&mut self, _: &Print) { todo!(); }
     fn visit_var(&mut self, v:&VarDecl) { 
        self.visit_expr(&v.value);
        self.emit_create_var(v);
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
            Identifier(ref _i) => {
                self.emit_get_var(l);
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
        let vp = vpass::get_vpass();
        let (var_use,var_def) = vp.get_usage(c.nodeid);
        let findex = var_def.stack_location as usize;

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

        let vp = vpass::get_vpass();
        let (var_use,var_def) = vp.get_usage(a.nodeid);
        //let id = self.ct_get_id_of_var(&a.var_name);
        let ofs = var_def.stack_location;

        if var_use.is_global() {
            let index = ofs;
            let ch=current_chunk!(self);
            ch.add_set_global(index as usize);
            //exprs must return something.
            ch.add_get_global(index as usize);
            return;
        }
        let ch=current_chunk!(self);
        ch.add_set_local(ofs as u8);
        ch.add_get_local(ofs as u8);
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
