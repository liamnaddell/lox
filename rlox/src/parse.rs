#[derive(Debug)]
pub enum UnaryOp {
    Sub,
    Neg,
}

#[derive(Debug)]
pub enum BinOp {
    //add the others
    And
}

#[derive(Debug)]
pub struct FnDecl {
    pub locus: usize,
    pub name: String,
    pub args: Vec<String>,
    pub fn_def: Box<Block>,
}

#[derive(Debug)]
pub struct Block {
    pub locus: usize,
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Debug)]
pub struct Print {
    pub locus: usize,
    pub to_print: Box<Expr>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub locus: usize,
    pub name: String,
    pub value: Box<Expr>
}

#[derive(Debug)]
pub struct If {
    pub locus: usize,
    pub cond: Box<Expr>,
    pub then: Box<Stmt>,
    pub or_else: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct While {
    pub locus: usize,
    pub is_true: Box<Expr>,
    pub do_block: Box<Block>,
}

#[derive(Debug)]
pub enum LitKind {
    StringLit(String),
    NumberLit(usize),
}

#[derive(Debug)]
pub struct Literal {
    pub locus: usize,
    pub kind: LitKind,
}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub sub: Box<Expr>,
    pub locus: usize,
}

#[derive(Debug)]
pub struct Call {
    pub locus: usize,
    pub fn_name: String,
    pub args: Vec<Box<Expr>>
}

#[derive(Debug)]
pub struct Binary {
    pub locus: usize,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Call(Call),
    Binary(Binary),
}

#[derive(Debug)]
pub struct Return {
    pub locus: usize,
}

#[derive(Debug)]
pub enum Stmt {
    Print(Print),
    If(If),
    While(While),
    Expr(Expr),
    Return(Return),
}

#[derive(Debug)]
pub enum Decl {
    FnDecl(FnDecl), 
    Stmt(Stmt), 
    VarDecl(VarDecl), 
    Block(Block), 
}

#[derive(Debug)]
pub struct Program {
    pub locus: usize,
    pub decls: Vec<Box<Decl>>,
}
