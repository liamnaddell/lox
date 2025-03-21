#[derive(Debug)]
pub struct ClassDecl;
#[derive(Debug)]
pub struct For;

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
    pub decls: Vec<Rc<Decl>>,
    pub nodeid: NodeId,
}

impl Block {
    pub fn new(locus: usize, decls: Vec<Rc<Decl>>, nodeid: NodeId) -> Block {
        Block { locus, decls, nodeid }
    }
}

#[derive(Debug)]
pub struct Print {
    #[allow(dead_code)]
    pub locus: usize,
    pub to_print: Rc<Expr>,
    pub nodeid: NodeId,
}

impl Print {
    pub fn new(locus: usize, to_print: Rc<Expr>, nodeid: NodeId) -> Print {
        Print { locus, to_print, nodeid }
    }
}

#[derive(Debug)]
pub struct VarDecl {
    #[allow(dead_code)]
    pub locus: usize,
    pub name: String,
    pub value: Rc<Expr>,
    pub nodeid: NodeId,
}

impl VarDecl {
    pub fn new(locus: usize, name: String, value: Rc<Expr>, nodeid: NodeId) -> VarDecl {
        VarDecl { locus, name, value, nodeid }
    }
}

#[derive(Debug)]
pub struct If {
    #[allow(dead_code)]
    pub locus: usize,
    pub cond: Rc<Expr>,
    pub then: Rc<Stmt>,
    pub or_else: Option<Rc<Stmt>>,
    pub nodeid: NodeId,
}

impl If {
    pub fn new(locus: usize, cond: Rc<Expr>, then: Rc<Stmt>, or_else: Option<Rc<Stmt>>, nodeid: NodeId) -> If {
        If { locus, cond, then, or_else, nodeid }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct While {
    pub locus: usize,
    pub is_true: Rc<Expr>,
    pub do_block: Rc<Block>,
    pub nodeid: NodeId,
}

impl While {
    pub fn new(locus: usize, is_true: Rc<Expr>, do_block: Rc<Block>, nodeid: NodeId) -> While {
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
    pub sub: Rc<Expr>,
    #[allow(dead_code)]
    pub locus: usize,
    pub nodeid: NodeId,
}

impl Unary {
    pub fn new(op: UnaryOp, sub: Rc<Expr>, locus: usize, nodeid: NodeId) -> Unary {
        Unary { op, sub, locus, nodeid }
    }
}

#[derive(Debug)]
pub struct Call {
    #[allow(dead_code)]
    pub locus: usize,
    pub fn_name: String,
    pub args: Vec<Rc<Expr>>,
    pub nodeid: NodeId,
}

impl Call {
    pub fn new(locus: usize, fn_name: String, args: Vec<Rc<Expr>>, nodeid: NodeId) -> Call {
        Call { locus, fn_name, args, nodeid }
    }
}

#[derive(Debug)]
pub struct Binary {
    #[allow(dead_code)]
    pub locus: usize,
    pub op: BinOp,
    pub left: Rc<Expr>,
    pub right: Rc<Expr>,
    pub nodeid: NodeId,
}

impl Binary {
    pub fn new(locus: usize, op: BinOp, left: Rc<Expr>, right: Rc<Expr>, nodeid: NodeId) -> Binary {
        Binary { locus, op, left, right, nodeid }
    }
}


impl Assignment {
    pub fn new(locus: usize, var_name: String, val_expr: Rc<Expr>, nodeid: NodeId) -> Assignment {
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
    pub decls: Vec<Rc<Decl>>,
    pub nodeid: NodeId,
}

impl Program {
    pub fn new(locus: usize, decls: Vec<Rc<Decl>>, nodeid: NodeId) -> Program {
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
    pub val_expr: Rc<Expr>,
    pub nodeid: NodeId,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Call(Call),
    Binary(Binary),
    Assignment(Assignment),
}
#[derive(Debug)]
pub struct Return {
    #[allow(dead_code)]
    pub locus: usize,
    //TODO(rval): Add return values
    pub nodeid: NodeId,
}

#[derive(Debug)]
pub enum Stmt {
    Print(Print),
    If(If),
    #[allow(dead_code)]
    While(While),
    Expr(Expr),
    #[allow(dead_code)]
    For(For),
    #[allow(dead_code)]
    Return(Return),
    #[allow(dead_code)]
    Block(Block), 
}
#[derive(Debug)]
pub enum Decl {
    #[allow(dead_code)]
    ClassDecl(ClassDecl),
    FnDecl(FnDecl), 
    VarDecl(VarDecl), 
    Stmt(Stmt), 
}
