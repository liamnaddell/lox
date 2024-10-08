use crate::token::*;
use std::result::Result as RResult;

//This is a type alias.
//Result is the Maybe monad, but instead of Some or None,
//we have Some or Err(msg)
//<T> is a generic parameter.
type Result<T> = RResult<T,CompileError>;

struct CompileError {
    locus: usize,
    msg: String,
}

//TINA: 'a is a lifetime. 
//This thing means "TknSlice CANNOT LIVE past it's vector reference"
//basically rust extends haskell's algebraic type system to include the lifetime of objects.
struct TknSlice<'a> {
    tkns: &'a Vec<Token>,
    start: usize,
    end: usize,
}

impl<'a> TknSlice<'a> {
    //str is a reference to stack or heap allocated string data
    //String is a heap-allocated string.
    //This is actually pretty advanced rust: I'm saying "Return a token, which lives as long as
    //TknSlice, which itself lives as long as it's Token array.
    fn pop_or(&mut self, err: &str) -> Result<&'a Token> {
        if self.start != self.end {
            self.start += 1;
            return Ok(&self.tkns[self.start - 1]);
        } else {
            return Err(CompileError::from_str(self.start,err));
        }
    }
}

impl CompileError {
    pub fn new(locus: usize, msg: String) -> CompileError {
        return CompileError {locus,msg};
    }
    pub fn from_str(locus: usize, msg: &str) -> CompileError {
        return CompileError {locus,msg:msg.to_string()};
    }
    pub fn emit(&self) {
        //TODO: Add pretty colors 可愛!!
        unimplemented!();
    }
}



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
    Identifier(String),
    NumberLit(f64),
}

#[derive(Debug)]
pub struct Literal {
    pub locus: usize,
    pub kind: LitKind,
}

impl Literal {
    fn parse(mut ts: TknSlice) -> Result<Box<Literal>> {
        //Rust blocks which return a Result are basically
        //the same as do blocks in haskell.
        //the ? operator means "Return an error, if pop_or fails",
        //or continue execution assuming pop_or succeds.
        let maybe_lit = ts.pop_or("not enuf tokens")?;
        // We want to restrict the kind of token in literal to
        // either being an identifier, string, or number.
        // NOTE: There's actually a "Rare CPP W" in here,
        // since in c++ we don't have to clone (make a heap copy) the token data.
        // Rust has a hard time of tracking movement in/out of vectors
        // (which can only really be done by the human mind, as it's NP complete)
        let l: LitKind = match &maybe_lit.tkn_type {
            TokenType::Identifier(s) => {
                LitKind::Identifier(s.clone())
            }
            TokenType::StringLit(s) => {
                LitKind::StringLit(s.clone())
            }
            TokenType::Number(f) => {
                LitKind::NumberLit(*f)
            }
            _ => {
                Err(CompileError::from_str(0,"expected an identifier, string literal, or number"))?
            }
        };
        return Ok(Box::new(Literal {locus:maybe_lit.locus, kind: l}));
    }
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
