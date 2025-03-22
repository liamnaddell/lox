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

#[derive(Debug)]
pub struct FnDecl {
    #[allow(dead_code)]
    pub locus: usize,
    pub name: String,
    pub args: Vec<String>,
    pub fn_def: Rc<Block>,
    pub nodeid: NodeId,
}

impl FnDecl {
    pub fn new(locus: usize, name: String, args: Vec<String>, fn_def: Rc<Block>, nodeid: NodeId) -> FnDecl {
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


impl Return {
    pub fn new(locus: usize, nodeid: NodeId) -> Return {
        Return { locus, nodeid }
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
    While(Rc<While>),
    Expr(Expr),
    For(Rc<For>),
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
