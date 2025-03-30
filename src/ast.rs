use std::rc::Rc;
use crate::token::*;
use std::cmp;

#[derive(Eq,PartialEq,Debug,Copy,Clone,PartialOrd,Ord)]
pub enum BinOp {
    Minus, Plus, Star,
    BangEqual,
    EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    And, Or, Slash
}

impl BinOp {
    pub fn from_tkntype(t: &TokenType) -> Option<BinOp> {
        match t {
            TokenType::Minus => Some(BinOp::Minus),
            TokenType::Plus => Some(BinOp::Plus),
            TokenType::Slash => Some(BinOp::Slash),
            TokenType::Star => Some(BinOp::Star),
            TokenType::BangEqual => Some(BinOp::BangEqual),
            TokenType::EqualEqual => Some(BinOp::EqualEqual),
            TokenType::Greater => Some(BinOp::Greater),
            TokenType::GreaterEqual => Some(BinOp::GreaterEqual),
            TokenType::Less => Some(BinOp::Less),
            TokenType::LessEqual => Some(BinOp::LessEqual),
            TokenType::Or => Some(BinOp::Or),
            TokenType::And => Some(BinOp::And),
            _ => None
        }
    }
    pub fn min() -> BinOp {
        return cmp::min(BinOp::Minus,BinOp::Or);
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Sub,
    Neg,
}

impl UnaryOp {
    pub fn from_tkntype(t: &TokenType) -> Option<UnaryOp> {
        match t {
            TokenType::Minus => Some(UnaryOp::Sub),
            TokenType::Bang => Some(UnaryOp::Neg),
            _ => None
        }
    }
}


#[derive(Debug)]
pub struct ClassDecl;
#[derive(Debug)]
pub struct For;

pub type NodeId = u32;

/**
 * An argument to a function.
 * Used only as part of a function *declaration* not use
 */
#[derive(Debug)]
pub struct FnArg {
    pub arg_name: String,
    pub nodeid: NodeId,
}

impl FnArg {
    pub fn new(arg_name: String, nodeid: NodeId) -> Self {
        FnArg {
            arg_name,
            nodeid
        }
    }
}

#[derive(Debug)]
pub struct FnDecl {
    #[allow(dead_code)]
    pub locus: usize,
    pub name: String,
    pub args: Vec<Rc<FnArg>>,
    pub fn_def: Rc<Block>,
    pub nodeid: NodeId,
}

impl FnDecl {
    pub fn new(locus: usize, name: String, args: Vec<Rc<FnArg>>, fn_def: Rc<Block>, nodeid: NodeId) -> FnDecl {
        FnDecl { locus, name, args, fn_def, nodeid }
    }
}

#[derive(Debug)]
pub struct Block {
    #[allow(dead_code)]
    pub locus: usize,
    pub decls: Vec<Decl>,
    pub nodeid: NodeId,
}

impl Block {
    pub fn new(locus: usize, decls: Vec<Decl>, nodeid: NodeId) -> Block {
        Block { locus, decls, nodeid }
    }
}

#[derive(Debug)]
pub struct Print {
    #[allow(dead_code)]
    pub locus: usize,
    pub to_print: Expr,
    pub nodeid: NodeId,
}

impl Print {
    pub fn new(locus: usize, to_print: Expr, nodeid: NodeId) -> Print {
        Print { locus, to_print, nodeid }
    }
}

#[derive(Debug)]
pub struct VarDecl {
    #[allow(dead_code)]
    pub locus: usize,
    pub name: String,
    pub value: Expr,
    pub nodeid: NodeId,
}

impl VarDecl {
    pub fn new(locus: usize, name: String, value: Expr, nodeid: NodeId) -> VarDecl {
        VarDecl { locus, name, value, nodeid }
    }
}

#[derive(Debug)]
pub struct If {
    #[allow(dead_code)]
    pub locus: usize,
    pub cond: Expr,
    pub then: Stmt,
    pub or_else: Option<Stmt>,
    pub nodeid: NodeId,
}

impl If {
    pub fn new(locus: usize, cond: Expr, then: Stmt, or_else: Option<Stmt>, nodeid: NodeId) -> If {
        If { locus, cond, then, or_else, nodeid }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct While {
    pub locus: usize,
    pub is_true: Expr,
    pub do_block: Rc<Block>,
    pub nodeid: NodeId,
}

impl While {
    pub fn new(locus: usize, is_true: Expr, do_block: Rc<Block>, nodeid: NodeId) -> While {
        While { locus, is_true, do_block, nodeid }
    }
}

#[derive(Debug)]
pub struct Literal {
    #[allow(dead_code)]
    pub locus: usize,
    pub kind: LitKind,
    pub nodeid: NodeId,
}

impl Literal {
    pub fn new(locus: usize, kind: LitKind, nodeid: NodeId) -> Literal {
        Literal { locus, kind, nodeid }
    }
}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub sub: Expr,
    #[allow(dead_code)]
    pub locus: usize,
    pub nodeid: NodeId,
}

impl Unary {
    pub fn new(op: UnaryOp, sub: Expr, locus: usize, nodeid: NodeId) -> Unary {
        Unary { op, sub, locus, nodeid }
    }
}

#[derive(Debug)]
pub struct Call {
    #[allow(dead_code)]
    pub locus: usize,
    pub fn_name: String,
    pub args: Vec<Expr>,
    pub nodeid: NodeId,
}

impl Call {
    pub fn new(locus: usize, fn_name: String, args: Vec<Expr>, nodeid: NodeId) -> Call {
        Call { locus, fn_name, args, nodeid }
    }
}

#[derive(Debug)]
pub struct Binary {
    #[allow(dead_code)]
    pub locus: usize,
    pub op: BinOp,
    pub left: Expr,
    pub right: Expr,
    pub nodeid: NodeId,
}

impl Binary {
    pub fn new(locus: usize, op: BinOp, left: Expr, right: Expr, nodeid: NodeId) -> Binary {
        Binary { locus, op, left, right, nodeid }
    }
}


impl Assignment {
    pub fn new(locus: usize, var_name: String, val_expr: Expr, nodeid: NodeId) -> Assignment {
        Assignment { locus, var_name, val_expr, nodeid }
    }
}



#[derive(Debug)]
pub struct Program {
    #[allow(dead_code)]
    pub locus: usize,
    pub decls: Vec<Decl>,
    pub nodeid: NodeId,
}

impl Program {
    pub fn new(locus: usize, decls: Vec<Decl>, nodeid: NodeId) -> Program {
        Program { locus, decls, nodeid }
    }
}


#[derive(Debug)]
pub enum LitKind {
    StringLit(String),
    Identifier(String),
    NumberLit(f64),
    True,
    False,
    Nil,
}


#[derive(Debug)]
pub struct Assignment {
    #[allow(dead_code)]
    pub locus: usize,
    pub var_name: String,
    pub val_expr: Expr,
    pub nodeid: NodeId,
}

#[derive(Debug)]
pub struct Return {
    #[allow(dead_code)]
    pub locus: usize,
    //TODO(rval): Add return values
    pub nodeid: NodeId,
}

impl Return {
    pub fn new(locus: usize, nodeid: NodeId) -> Return {
        Return { locus, nodeid }
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Rc<Literal>),
    Unary(Rc<Unary>),
    Call(Rc<Call>),
    Binary(Rc<Binary>),
    Assignment(Rc<Assignment>),
}

#[derive(Debug)]
pub enum Stmt {
    Print(Rc<Print>),
    If(Rc<If>),
    #[allow(dead_code)]
    While(Rc<While>),
    Expr(Expr),
    Return(Rc<Return>),
    Block(Rc<Block>), 
}
#[derive(Debug)]
pub enum Decl {
    #[allow(dead_code)]
    ClassDecl(Rc<ClassDecl>),
    FnDecl(Rc<FnDecl>), 
    VarDecl(Rc<VarDecl>), 
    Stmt(Stmt), 
}

/**
 * This Trait is used for defining a compiler pass.
 * A compiler pass is a `struct` that collects or produces information
 *  about an AST.
 *
 * For example, the `compile` pass produces bytecode from an AST.
 * For another example, the `vpass` generates information about the AST,
 *  specfically by solving for name resolution.
 *
 * This trait is divided into two kinds of functions. The `visit_*` functions,
 *  and the `recurse_*` functions.
 *
 * The `visit_*` functions are meant to be overridden by the AST pass in question.
 * For example, the `compile` pass will override `visit_function` to compile the
 *  function.
 *
 * The `recurse_*` functions are meant to provide a mechanism to recurse into the
 *  subnodes of the AST. For example, `recurse_function` will recurse into the 
 *  function body.
 *
 * By default, the `visit_*` functions simply run `recurse_*` functions. When you
 *  override these functions, you should call the appropriate `recurse_*` function
 *  in order to reach every node.
 */
pub trait AstCooker {
    // By default, we just recurse. If you override one of these functions,
    // make sure to call recurse_x to keep recursing!
    fn visit_function(&mut self, f: &FnDecl) { self.recurse_function(f); }
    fn visit_block(&mut self, b: &Block) { self.recurse_block(b); }
    fn visit_print(&mut self, p: &Print) { self.recurse_print(p); }
    fn visit_var(&mut self, v: &VarDecl) { self.recurse_var(v); }
    fn visit_if(&mut self, i: &If) { self.recurse_if(i); }
    fn visit_while(&mut self, w: &While) { self.recurse_while(w); }
    fn visit_literal(&mut self, _: &Literal) {}
    fn visit_unary(&mut self, u: &Unary) { self.recurse_unary(u); }
    fn visit_call(&mut self, c: &Call) { self.recurse_call(c); }
    fn visit_binary(&mut self, b: &Binary) { self.recurse_binary(b); }
    fn visit_assignment(&mut self, a: &Assignment) { self.recurse_assignment(a); }
    fn visit_expr(&mut self, e: &Expr) { self.recurse_expr(e); }
    fn visit_stmt(&mut self, s: &Stmt) { self.recurse_stmt(s); }
    fn visit_decl(&mut self, d: &Decl) { self.recurse_decl(d); }
    fn visit_return(&mut self, r: &Return) { self.recurse_return(r); }
    fn visit_program(&mut self, p: &Program) { self.recurse_program(p); }
    fn visit_class(&mut self, c: &ClassDecl) { self.recurse_class(c); }

    /* Default visitor functions, should not be overridden */
    fn recurse_function(&mut self, f: &FnDecl) {
        //We don't visit block because function "Blocks"
        //are a logical storage unit but do not open a new scope.
        //self.visit_block(&f.fn_def)
        for d in f.fn_def.decls.iter() {
            self.visit_decl(d);
        }
    }
    fn recurse_block(&mut self, b: &Block) {
        for d in b.decls.iter() {
            self.visit_decl(d);
        }
    }
    fn recurse_print(&mut self, p: &Print) {
        self.visit_expr(&p.to_print);
    }
    fn recurse_var(&mut self, v: &VarDecl) {
        self.visit_expr(&v.value);
    }
    fn recurse_if(&mut self, i: &If) {
        self.visit_expr(&i.cond);
        self.visit_stmt(&i.then);
        if let Some(or) = &i.or_else {
            self.visit_stmt(&or);
        }
    }
    fn recurse_while(&mut self, w: &While) {
        self.visit_expr(&w.is_true);
        self.visit_block(&w.do_block);
    }
    fn recurse_unary(&mut self, u: &Unary) {
        self.visit_expr(&u.sub);
    }
    fn recurse_call(&mut self, c: &Call) {
        for arg in c.args.iter() {
            self.visit_expr(arg);
        }
    }
    fn recurse_binary(&mut self, b: &Binary) {
        self.visit_expr(&b.left);
        self.visit_expr(&b.right);
    }
    fn recurse_assignment(&mut self, a: &Assignment) {
        self.visit_expr(&a.val_expr);
    }
    fn recurse_expr(&mut self, e: &Expr) {
        match e {
            Expr::Literal(l) => {
                self.visit_literal(l);
            }
            Expr::Unary(u) => {
                self.visit_unary(u);
            }
            Expr::Call(c) => {
                self.visit_call(c);
            }
            Expr::Binary(b) => {
                self.visit_binary(b);
            }
            Expr::Assignment(a) => {
                self.visit_assignment(a);
            }
        }
    }
    fn recurse_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Print(p) => {
                self.visit_print(p);
            }
            Stmt::If(i) => {
                self.visit_if(i);
            }
            Stmt::While(w) => {
                self.visit_while(w);
            }
            Stmt::Expr(e) => {
                self.visit_expr(e);
            }
            Stmt::Return(r) => {
                self.visit_return(r);
            }
            Stmt::Block(b) => {
                self.visit_block(b);
            }
        }
    }
    fn recurse_decl(&mut self, d: &Decl) {
        match d {
            Decl::ClassDecl(c) => {
                self.visit_class(c);
            }
            Decl::FnDecl(f) => {
                self.visit_function(f);
            }
            Decl::VarDecl(v) => {
                self.visit_var(v);
            }
            Decl::Stmt(s) => {
                self.visit_stmt(s);
            }
        }
    }
    fn recurse_return(&mut self, _: &Return) { todo!(); }
    fn recurse_program(&mut self, p: &Program) {
        for d in p.decls.iter() {
            self.visit_decl(d);
        }
    }
    fn recurse_class(&mut self, _: &ClassDecl) {
        todo!();
    }
}
