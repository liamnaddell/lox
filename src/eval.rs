use crate::parse::*;
use crate::error::*;
use std::collections::HashMap;

//TBH: I'm honestly amazed that rust lets you do this... Is it a good idea?
impl BinOp {
    fn apply(op: &BinOp, left: Val, right: Val, locus: usize ) -> Result<Val> {
        match (op, left, right) {
            (BinOp::Minus, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::NumberLit(l - r)),
            (BinOp::Plus, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::NumberLit(l + r)),
            (BinOp::Plus, Val::StringLit(l), Val::StringLit(r)) => Ok(Val::StringLit(format!("{l}{r}"))),
            (BinOp::Slash, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::NumberLit(l / r)),
            (BinOp::Star, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::NumberLit(l * r)),
            (BinOp::BangEqual, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l != r)),
            (BinOp::BangEqual, Val::StringLit(l), Val::StringLit(r)) => Ok(Val::Bool(l != r)), 
            (BinOp::EqualEqual, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l == r)),
            (BinOp::EqualEqual, Val::StringLit(l), Val::StringLit(r)) => Ok(Val::Bool(l == r)),
            (BinOp::Greater, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l > r)), 
            (BinOp::GreaterEqual, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l >= r)), 
            (BinOp::Less, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l < r)), 
            (BinOp::LessEqual, Val::NumberLit(l), Val::NumberLit(r)) => Ok(Val::Bool(l <= r)), 
            (BinOp::Or, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l || r)),
            (BinOp::And, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l && r)),
            (_, _, _) => Err(new_err(locus,"could not apply binary operation"))?
        }
    }
}

struct Env {
    hm: HashMap<String,Val>,
}

impl Env {
    fn new() -> Self {
        return Env { hm: HashMap::new() };
    }
    fn set(&mut self, key: &str, val: &Val) {
        self.hm.insert(key.to_string(),val.clone());
    }
    //TODO: This is a copy!
    fn get(&self, key: &str) -> Option<Val> {
        return self.hm.get(key).cloned();
    }
}

impl Block {
    fn eval(&self, env: &Env) -> Result<()> {
        for stmt in &self.stmts {
            stmt.eval(env)?;
        }
        return Ok(());
    }
}

impl Literal {
    //TODO: Ret val?
    fn eval(&self, env: &Env) -> Result<Val> {
        let x = &self.kind;
        match x {
            LitKind::StringLit(y) => { 
                Ok(Val::StringLit(y.clone()))
            },
            LitKind::Identifier(ref y) => { 
                if let Some(v) = env.get(y) {
                    Ok(v)
                } else {
                    Err(new_err(self.locus, "Identifier not found"))
                }
            },
            LitKind::NumberLit(y) => {
                Ok(Val::NumberLit(*y))
            },
            LitKind::True => { 
                Ok(Val::Bool(true))
            },
            LitKind::False => {
                Ok(Val::Bool(false))
            },
            LitKind::Nil => { 
                Ok(Val::Nil)
            },
        } 
    }
}

impl Unary {
    fn eval(&self, env: &Env) -> Result<Val> {
        let op = &self.op;
        let sub = Expr::eval(&self.sub,env)?;
        match (op, sub) {
            (UnaryOp::Sub, Val::NumberLit(y)) => { 
                Ok(Val::NumberLit(-(y.clone())))
            },
            (UnaryOp::Neg, Val::Bool(y)) => { 
                Ok(Val::Bool(!y))
            },
            (UnaryOp::Neg, Val::Nil) => { 
                // NOT SURE IF THIS IS WHAT WE WANT HERE
                Ok(Val::Bool(true))
            },
            (UnaryOp::Sub, Val::Bool(_)) => { 
                Err(new_err(self.locus,"why are u applying a neg to a bool?"))?
            },
            (_, _) => {
                Err(new_err(self.locus,"why are u applying a unary operator to a string/identifier/nil?"))?
            }
        } 
    }
}

impl Binary {
    fn eval(&self, env: &Env) -> Result<Val> {
        let op = &self.op;
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
        return BinOp::apply(op, left, right, self.locus);
    }
}

impl Expr {
    fn eval(&self, env: &Env) -> Result<Val> {
        match self {
            Expr::Literal(ref l) => l.eval(env),
            Expr::Unary(ref u) => Unary::eval(u,env),
            Expr::Binary(ref b) => Binary::eval(b,env),
            Expr::Call(_) => todo!(),
        }
    }
}

impl Stmt {
    fn eval(&self, env: &Env) -> Result<()> {
        match self {
            Stmt::Print(ref p) => {
                let maybe_val = p.to_print.eval(env)?;
                println!("{:?}",maybe_val);
            }
            Stmt::Expr(ref e) => {
                e.eval(env)?;
            }
            _ => { todo!() }
        }
        return Ok(());
    }
}

impl VarDecl {
    fn eval(&self, env: &mut Env) -> Result<()> {
        env.set(&self.name,&self.value.eval(env)?);
        return Ok(());
    }
}

impl Decl {
    fn eval(&self, env: &mut Env) -> Result<()> {
        match self {
            Decl::Stmt(ref s) => {
                return s.eval(env);
            }
            Decl::Block(ref b) => {
                return b.eval(env);
            }
            Decl::VarDecl(ref vd) => {
                return vd.eval(env);
            }
            _ => { todo!() }
        }
    }
}

impl Program {
    fn eval(&self) -> Result<()> {
        let mut env = Env::new();
        for decl in &self.decls {
            decl.eval(&mut env)?;
        }
        return Ok(());
    }
}

pub fn eval(exp: Box<Program>) {
    let maybe_expr = Program::eval(&exp);

    if let Err(e) = maybe_expr {
        e.emit();
    }
}
