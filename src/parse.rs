use crate::token::*;
use crate::error::*;
use std::cmp;

struct TknSlice<'a> {
    tkns: &'a Vec<Token>,
    start: usize,
    end: usize,
}

//TODO: Implement proper pythonized array indexing
impl<'a> TknSlice<'a> {
    fn loc(&self, index: usize) -> usize {
        //TODO: Edge case?
        if self.size() == 0 {
            return self.start;
        }
        assert!(index < self.size());
        return self.get(index).locus;
    }
    //str is a reference to stack or heap allocated string data
    //String is a heap-allocated string.
    //This is actually pretty advanced rust: I'm saying "Return a token, which lives as long as
    //TknSlice, which itself lives as long as it's Token array.
    fn pop_or(&mut self, err: &str) -> Result<&'a Token> {
        if self.start != self.end {
            self.start += 1;
            return Ok(&self.tkns[self.start - 1]);
        } else {
            return Err(new_err(self.start,err));
        }
    }
    fn end(&self) -> usize {
        return self.size()-1;
    }
    fn get(&self, index: usize) -> &'a Token {
        if index >= self.size() {
            panic!("The compiler sucks");
        }
        return &self.tkns[self.start + index];
    }

    fn size(&self) -> usize {
        return self.end - self.start;
    }

    fn sub(&self,new_start:usize,mut new_end:usize) -> TknSlice<'a> {
        if new_end == 0 {
            new_end = self.end;
        } else {
            new_end = self.start + new_end;
        }

        assert!(new_start <= self.end);
        assert!(new_end >= new_start);

        return TknSlice { tkns: self.tkns,start:self.start+new_start,end:new_end };
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Sub,
    Neg,
}

impl UnaryOp {
    fn from_tkntype(t: &TokenType) -> Option<UnaryOp> {
        match t {
            TokenType::Minus => Some(UnaryOp::Sub),
            TokenType::Bang => Some(UnaryOp::Neg),
            _ => None
        }
    }
}
/*
 * COMPILER BUG:
 * FIX: Including And twice results in silly diag error from BinOp (Debug) derive.
#[derive(Debug)]
pub enum BinOp {
    //add the others
    And = 0,
    Minus, Plus, Semicolon, Slash, Star,
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    And, Or,
}
*/

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
    fn from_tkntype(t: &TokenType) -> Option<BinOp> {
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
    fn min() -> BinOp {
        return cmp::min(BinOp::Minus,BinOp::Or);
    }
}
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
    pub fn_def: Box<Block>,
}

impl FnDecl {
    fn parse(ts: TknSlice) -> Result<Box<FnDecl>> {
        let mut args = vec!();
        //fun name(<args>) {}
        if ts.size() < 6 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }

        //stage 1: get name
        assert!(ts.get(0).tkn_type == TokenType::Fun);
        let name = ts.get(1);
        let fn_name = {
            if let TokenType::Identifier(ref fn_name) = name.tkn_type {
                fn_name
            } else {
                return Err(new_err(ts.loc(1),"Function name is not a name"));
            }
        };

        //stage 2: parse argument list
        if ts.get(2).tkn_type != TokenType::LeftParen {
            return Err(new_err(ts.loc(2),"Function has no args"));
        }

        let mut i = 3;
        let mut paren = false;
        while i < ts.end() {
            let mut tkn = ts.get(i);

            if let TokenType::Identifier(ref id) = tkn.tkn_type {
                args.push(id.clone());
                if i + 1 >= ts.end() {
                    return Err(new_err(tkn.locus,"Abrupt end of program :/"));
                }
                i+=1;
                tkn = ts.get(i)
            }
            if tkn.tkn_type == TokenType::RightParen {
                paren = true;
                break;
            } else if tkn.tkn_type == TokenType::Comma {
                i+=1;
            } else {
                return Err(new_err(tkn.locus,"strange token in argument"));
            }
        }
        if !paren {
            return Err(new_err(ts.loc(i),"function arguemnts do not end with ')'"));
        }
        //skip right paren
        i+=1;
        //check for {}
        if ts.get(i).tkn_type != TokenType::LeftBrace || ts.get(ts.end()).tkn_type != TokenType::RightBrace {
            return Err(new_err(ts.loc(i),"Function has no {}"));
        }
        let block = Block::parse(ts.sub(i+1,ts.end()))?;
        return Ok(Box::new(FnDecl {locus:ts.loc(0), name:fn_name.clone(),args:args,fn_def:block}));


    }
}


#[derive(Debug)]
pub struct Block {
    #[allow(dead_code)]
    pub locus: usize,
    pub decls: Vec<Box<Decl>>,
}

impl Block {
    //NOTE: removes semicolons
    fn parse(ts: TknSlice) -> Result<Box<Block>> {
        //<stmt>;
        //<stmt>;
        //...
        let mut b = Block { locus: ts.loc(0), decls:vec!()};
        let mut loc_old = 0;
        let mut loc = 0;

        let mut brace_cnt = 0;
        //NOTE: TODO: We have a lot of this repetitive and spammy brace counting code.
        // Maybe a better engineer can see how to move these into a single function.
        while loc != ts.size() {
            let t = ts.get(loc); 
            if t.tkn_type == TokenType::LeftBrace { brace_cnt += 1; }
            if t.tkn_type == TokenType::RightBrace { brace_cnt -= 1; }
            if t.tkn_type == TokenType::Semicolon && brace_cnt == 0 {
                /* include the semicolon */
                let ts2 = ts.sub(loc_old,loc+1);
                let stmt = Decl::parse(ts2)?;
                b.decls.push(stmt);
                //skip semicolon
                loc_old = loc+1;
            }
            loc += 1;
        }
        //TODO: This should be an assert?
        //How did we get this far with unclosed left brace ://
        if brace_cnt != 0 {
            return Err(new_err(ts.loc(ts.end()),"Unclosed left brace dingus"));
        }

        return Ok(Box::new(b));
    }
}

#[derive(Debug)]
pub struct Print {
    #[allow(dead_code)]
    pub locus: usize,
    pub to_print: Box<Expr>,
}

#[derive(Debug)]
pub struct VarDecl {
    #[allow(dead_code)]
    pub locus: usize,
    pub name: String,
    pub value: Box<Expr>
}

impl VarDecl {
    fn parse(ts: TknSlice) -> Result<Box<VarDecl>> {
        //var <ident> = <expr>
        if ts.size() < 4 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }
        assert!(ts.get(0).tkn_type == TokenType::Var);

        let ident = ts.get(1);
        let iname: String;
        if let TokenType::Identifier(ref s) = ident.tkn_type {
            iname = s.clone();
        } else {
            return Err(new_err(ts.loc(1), "Expected Identifier"));
        }

        let equals = ts.get(2);
        if equals.tkn_type != TokenType::Equal {
            return Err(new_err(ts.loc(2), "Expected equals statement"));
        }

        let expr = Expr::parse(ts.sub(3,0))?;

        return Ok(Box::new(VarDecl { locus: ts.loc(0)
            , name:iname
            , value:expr }));
    }
}

#[derive(Debug)]
pub struct If {
    #[allow(dead_code)]
    pub locus: usize,
    pub cond: Box<Expr>,
    pub then: Box<Stmt>,
    pub or_else: Option<Box<Stmt>>,
}

impl If {
    fn parse(ts: TknSlice) -> Result<Box<If>> {
        let left_paren = ts.get(1);

        if left_paren.tkn_type == TokenType::LeftParen {
            // find index of right paranthesis
            let mut i = 2;
            let mut paren_cnt = 1;
            while ts.end() != (i+1) && paren_cnt != 0 {
                i += 1;
                if ts.get(i).tkn_type == TokenType::RightParen {
                    paren_cnt -= 1;
                }
                if ts.get(i).tkn_type == TokenType::LeftParen {
                    paren_cnt +=1;
                }
            }

            if paren_cnt != 0 {
                return Err(new_err(ts.loc(ts.end()),"messed up the if cond")); 
            }
            
            i+=1;
            let cond = Expr::parse(ts.sub(1,i))?;
            
            // parse the statemet (then)
            let idx = i;
            while ts.end() != (i+1) && ts.get(i + 1).tkn_type != TokenType::Else {
                    i += 1;
            }

            let then = Stmt::parse(ts.sub(idx,
                if ts.get(i+1).tkn_type == TokenType::Else { i } else { i + 2}))?;

            // if there is more left, parse or_else
            if ts.get(i+1).tkn_type == TokenType::Else {
                let idx = i + 2;
                i = ts.end();
                while ts.get(i).tkn_type != TokenType::RightParen {
                    i -= 1;
                }

                let or_else = Stmt::parse(ts.sub(idx,i))?;

                return Ok(Box::new(If { locus: ts.loc(0)
                    , cond: cond
                    , then: then
                    , or_else: Some(or_else) }));
            }
            return Ok(Box::new(If { locus: ts.loc(0)
                , cond: cond
                , then: then
                , or_else: None }));
        }

        return Err(new_err(ts.loc(ts.end()),"messed up if statement somehow"));
    }
}

#[derive(Debug)]
#[allow(dead_code)]
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
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub struct Literal {
    #[allow(dead_code)]
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
            TokenType::False => {
                LitKind::False
            }
            TokenType::True => {
                LitKind::True
            }
            TokenType::Nil => {
                LitKind::Nil
            }
            _ => {
                Err(new_err(0,"expected an identifier, string literal, or number"))?
            }
        };
        return Ok(Box::new(Literal {locus:maybe_lit.locus, kind: l}));
    }

}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub sub: Box<Expr>,
    #[allow(dead_code)]
    pub locus: usize,
}

impl Unary {
    fn parse(mut ts: TknSlice) -> Result<Box<Unary>> {
        let loc = ts.loc(0);
        //should only be called by expr which checks this.
        let op: &TokenType = &ts.pop_or("Expected unary operator")?.tkn_type;

        let una_op = {
            if let Some(u) = UnaryOp::from_tkntype(&op) {
                //this sets una_op to u bcs no ;
                u
            } else {
                //this returns from parse
                return Err(new_err(ts.loc(0),"Not a unary operator"));
            }
        };

        let sub_expr = Expr::parse(ts)?;

        return Ok(Box::new(Unary { op: una_op, sub: sub_expr, locus: loc}));
    }
}

#[derive(Debug)]
pub struct Call {
    #[allow(dead_code)]
    pub locus: usize,
    pub fn_name: String,
    pub args: Vec<Box<Expr>>
}

#[derive(Debug)]
pub struct Binary {
    #[allow(dead_code)]
    pub locus: usize,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct Assignment {
    #[allow(dead_code)]
    pub locus: usize,
    pub var_name: String,
    pub val_expr: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Call(Call),
    Binary(Binary),
    Assignment(Assignment),
}

fn bop_higher_prec(bop:BinOp,maybe_high_prec_bop:BinOp) -> bool {
    return maybe_high_prec_bop > bop;
}

impl Expr {
    fn parse(mut ts: TknSlice) -> Result<Box<Expr>> {
        if ts.size() == 0 {
            panic!("I think this should paic now");
            //return Err(new_err(ts.loc(0),"Emptiness"));
        }
        //TODO: UGGO CODE
        //Skip outer parenthesis
        while ts.size() > 2 && ts.get(0).tkn_type == TokenType::LeftParen &&
            ts.get(ts.size()-1).tkn_type == TokenType::RightParen {
                ts = ts.sub(1,ts.size()-1);
        }

        //First try and parse a literal, since it's easy.
        if ts.size() == 1 {
            //wrong type :(
            let lit: Box<Literal> = Literal::parse(ts)?;

            //this is a move
            let lit_expr: Box<Expr> = Box::new(Expr::Literal(*lit));
            return Ok(lit_expr);
        }

        //Second, try to parse a unary.
        if ts.size() > 1 && UnaryOp::from_tkntype(&ts.get(0).tkn_type).is_some() {
            let una: Box<Unary> = Unary::parse(ts)?;

            //this is a move
            let una_expr: Box<Expr> = Box::new(Expr::Unary(*una));
            return Ok(una_expr);
        }

        //Third, try to parse a call.
        if ts.size() > 2 
            && ts.get(1).tkn_type == TokenType::LeftParen 
            && ts.get(ts.end()).tkn_type == TokenType::RightParen {
        if let TokenType::Identifier(ref fnname) = ts.get(0).tkn_type {
            //TODO: Proper parsing!!
            //this is actually a call expr
            let mut arglist = vec!();
            //「T H I S  S U C K S  I  H A T E  I T」
            // basically the algorithm is that we have a ,-deliminated list of exprs, but exprs can
            // have subfunction calls with parenthesis, which makes our life very annoying, so we
            // count parenthesis and only split on ,'s with "scope" zero.
            let mut i = 2;
            let mut scope = 0;
            let mut prison_begin = i;
            //skip the last )
            while i < ts.size()-1 {
                let tkn = ts.get(i);
                if tkn.tkn_type == TokenType::LeftParen {
                    scope +=1;
                } else if tkn.tkn_type == TokenType::RightParen {
                    if scope == 1 {
                        return Err(new_err(tkn.locus,"too many right paren"));
                    }
                    scope -= 1;
                }
                if tkn.tkn_type == TokenType::Comma && scope == 0 {
                    //parse sub-expr
                    let expr_begin = prison_begin;
                    let expr_end = i;
                    let tse = ts.sub(expr_begin,expr_end);
                    arglist.push(Expr::parse(tse)?);
                    prison_begin = i + 1;

                }
                i += 1;
            }
            if prison_begin < ts.end() {
                //grab that last dude (˶˃ᵕ˂˶).ᐟ.ᐟ
                let tse = ts.sub(prison_begin,ts.end());
                arglist.push(Expr::parse(tse)?);
            }
            return Ok(Box::new(Expr::Call(Call { locus:ts.loc(0),fn_name:fnname.clone(),args:arglist})));
        }}


        //Fourth, try to parse an assignment.
        if ts.size() > 2 && ts.get(1).tkn_type == TokenType::Equal {
            let TokenType::Identifier(ref fnname) = ts.get(0).tkn_type else {
                return Err(new_err(ts.loc(0), "LHS of assignment is not identifier"));
            };
            let sub = Expr::parse(ts.sub(2,0))?;
            return Ok(Box::new(Expr::Assignment( Assignment {locus:ts.loc(1),var_name:fnname.clone(),val_expr: sub})));
        }

        /*
         ******************************************
         * Attempting to parse binary operators now
         ******************************************
         */
        let mut head = ts.end() as isize;
        let mut paren_order = 0;
        //mapping from operator -> location wrt ts.
        let mut bop = BinOp::min();
        //no bop.
        let mut bop_loc = 0;

        while head >= 0 {
            let cur_tkn = ts.get(head as usize);

            // paren_order = 0 => "we are not in parenthesis"
            // paren_order == 0 => "we are in parenthesis"
            if cur_tkn.tkn_type == TokenType::RightParen {
                paren_order+=1;
            }
            if cur_tkn.tkn_type == TokenType::LeftParen {
                if paren_order == 0 {
                    return Err(CompileError::from_str(head as usize,"Unmatched paren"));
                }
                paren_order-=1;
            }

            //Skip trying to find binops from inside parens.
            if paren_order != 0 {
                head-=1;
                continue;
            }

            let maybe_bop = BinOp::from_tkntype(&cur_tkn.tkn_type);
            if maybe_bop.is_none() {
                head-=1;
                continue;
            }

            let maybe_high_prec_bop: BinOp = maybe_bop.unwrap();
            //higher precedence, OR we haven't parsed a bop yet.
            if bop_higher_prec(bop,maybe_high_prec_bop) || bop_loc == 0 {
                bop = maybe_high_prec_bop;
                bop_loc = head as usize;
            }
            head-=1;
        }

        if bop_loc == 0 {
            return Err(CompileError::from_str(ts.loc(0),"Was looking for binary operator, but did not find any"));
        }

        //there was a bin op.
        let left: Box<Expr> = Expr::parse(ts.sub(0,bop_loc))?;
        let right: Box<Expr> = Expr::parse(ts.sub(bop_loc+1,0))?;
        return Ok(Box::new(Expr::Binary(Binary {locus:bop_loc,op:bop,left,right})));
    }
}

#[derive(Debug)]
pub struct Return {
    #[allow(dead_code)]
    pub locus: usize,
    //TODO(rval): Add return values
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

impl Stmt {
    //NOTE: Does !NOT! parse semicolons.
    fn parse(ts: TknSlice) -> Result<Box<Stmt>> {
        if ts.size() == 0 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }

        let first = ts.get(0);
        match first.tkn_type {
            TokenType::Print => {
                let sub = Expr::parse(ts.sub(1,0))?;
                return Ok(Box::new(Stmt::Print(Print{locus:ts.loc(0),to_print:sub}))); 
            },
            TokenType::If => {
                return Ok(Box::new(Stmt::If(*If::parse(ts.sub(0,0))?))); 
            },
            TokenType::While => {
                return Err(new_err(ts.loc(0),"idk bcs while loop not implemented yet"));
            },
            TokenType::Return => {
                return Err(new_err(ts.loc(0),"idk bcs return not implemented yet"));
            },
            _ => {
                let sub = Expr::parse(ts.sub(0,0))?;
                return Ok(Box::new(Stmt::Expr(*sub))); 
            }
        }

    }
}

#[derive(Debug)]
pub enum Decl {
    #[allow(dead_code)]
    ClassDecl(ClassDecl),
    FnDecl(FnDecl), 
    VarDecl(VarDecl), 
    Stmt(Stmt), 
}

impl Decl {
    fn parse(ts: TknSlice) -> Result<Box<Decl>> {
        if ts.size() < 2 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }

        let first = ts.get(0);
        let last = ts.get(ts.end());

        /*
        if first.tkn_type == TokenType::LeftBrace && last.tkn_type == TokenType::RightBrace {
            //TODO: ugl line
            return Ok(Box::new(Decl::Stmt(*Stmt::parse(ts.sub(1,0))?)));
        }
        */

        if last.tkn_type != TokenType::Semicolon {
            return Err(new_err(ts.loc(ts.end()),"forgot semicolon?"));
        }

        if first.tkn_type == TokenType::Fun {
            //strip semicolon TODO: Uggo line
            return Ok(Box::new(Decl::FnDecl(*FnDecl::parse(ts.sub(0,ts.end()))?)));
        }
        if first.tkn_type == TokenType::Var {
            return Ok(Box::new(Decl::VarDecl(*VarDecl::parse(ts.sub(0,ts.end()))?)));
        }


        //TODO: uggo line
        return Ok(Box::new(Decl::Stmt(*Stmt::parse(ts.sub(0,ts.end()))?)));
    }
}

#[derive(Debug)]
pub struct Program {
    #[allow(dead_code)]
    pub locus: usize,
    pub decls: Vec<Box<Decl>>,
}

impl Program {
    //NOTE: Does !NOT! eat semicolons.
    fn parse(ts: TknSlice) -> Result<Box<Program>> {
        /*
         * loop through tokens, and break out decls.
         * decls can have two types
         * 1. semicoloned
         * 2. non-semicoloned
         *
         * semicoloned expressions match the regex `.*;`
         * non-semicoloned expressions match the regex `kw.*{.*}`
         *
         */
        let mut loc = 0;
        let mut brace_cnt = 0;
        let mut p = Program { locus: 0, decls: vec!() };
        let mut loc_old = 0;

        while loc != ts.size() {
            let t = ts.get(loc); 
            let mut delimit_decl = false;
            match t.tkn_type {
                TokenType::LeftBrace => {
                    brace_cnt += 1;
                }
                TokenType::RightBrace => {
                    if brace_cnt == 0 {
                        return Err(new_err(loc,"Too many right brace"));
                    }
                    //delimit_decl = brace_cnt == 1;
                    brace_cnt -= 1;
                }
                TokenType::Semicolon => {
                    delimit_decl = true;
                }
                _ => {}
            }
            if delimit_decl && brace_cnt == 0 {
                let ts2 = ts.sub(loc_old,loc + 1);
                let decl = Decl::parse(ts2)?;
                p.decls.push(decl);
                loc_old = loc+1;
            }
            loc += 1;
        }
        return Ok(Box::new(p));
    }
}

pub fn parse(tkns: Vec<Token>) -> Option<Box<Program>> {
    let tkn_slice = TknSlice { tkns: &tkns, start: 0, end: tkns.len() };

    let maybe_expr = Program::parse(tkn_slice);

    if let Err(e) = maybe_expr {
        e.emit();
        return None;
    }

    return Some(maybe_expr.unwrap());
}
