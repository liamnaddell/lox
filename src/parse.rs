use crate::token::*;
use crate::error::*;
use std::rc::Rc;
use crate::ast::*;
use crate::ast;


//TODO: Switch out Rc< for lifetime param
enum AstNode {
    FnArg(Rc<FnArg>),
    FnDecl(Rc<FnDecl>),
    Block(Rc<Block>),
    Print(Rc<Print>),
    VarDecl(Rc<VarDecl>),
    If(Rc<If>),
    While(Rc<While>),
    Literal(Rc<Literal>),
    Unary(Rc<Unary>),
    Call(Rc<Call>),
    Binary(Rc<Binary>),
    Assignment(Rc<Assignment>),
    Expr(Expr),
    Return(Rc<Return>),
    Stmt(Rc<Stmt>),
    Decl(Rc<Decl>),
    Program(Rc<Program>),
}
struct AstNodeStore {
    n: AstNode,
}

struct AstStore {
    ns: Vec<AstNodeStore>,
}
macro_rules! do_shit {
    ($name: ident, $get:ident,$add:ident) => {
        fn $get(&self, ni: NodeId) -> Rc<$name> {
            let n = &self.ns[ni as usize];
            let AstNode::$name(ref f) = n.n else {
                panic!("ICE");
            };
            return f.clone()
        }
        fn $add(&mut self, mut f: $name) -> Rc<$name> {
            let ni = self.ns.len();
            f.nodeid = ni as u32;
            let frc = Rc::new(f);
            self.ns.push(AstNodeStore { n: AstNode::$name(frc.clone())});
            return frc;
        }
    }
}
impl AstStore {
    do_shit!(FnArg,get_fnarg,add_fnarg);
    do_shit!(FnDecl,get_fndecl,add_fndecl);
    do_shit!(Block,get_block,add_block);
    do_shit!(Print,get_print,add_print);
    do_shit!(VarDecl,get_vardecl,add_vardecl);
    do_shit!(If,get_if,add_if);
    do_shit!(While,get_while,add_while);
    do_shit!(Literal,get_literal,add_literal);
    do_shit!(Unary,get_unary,add_unary);
    do_shit!(Call,get_call,add_call);
    do_shit!(Binary,get_binary,add_binary);
    do_shit!(Assignment,get_assignment,add_assignment);
    do_shit!(Return,get_return,add_return);
    do_shit!(Program,get_program,add_program);
    fn get_root(&self) -> Rc<Program> {
        let pgi = self.ns.len() - 1;
        return self.get_program(pgi as u32);
    }
    fn new() -> Self {
        return AstStore { ns: vec!() };
    }
}

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



fn bop_higher_prec(bop:BinOp,maybe_high_prec_bop:BinOp) -> bool {
    return maybe_high_prec_bop > bop;
}

pub struct ParsePass {
    ast: AstStore,
    success: bool,
}

macro_rules! bloob {
    ($name: ident,$ty: ident) => {
        fn $name(&mut self, a: $ty) -> Rc<$ty> {
            //something like "add_$ty"
            let rv = self.ast.$name(a);
            return rv;
        }
    }
}

impl ParsePass {
    bloob!(add_fndecl,FnDecl);
    bloob!(add_block,Block);
    bloob!(add_print,Print);
    bloob!(add_vardecl,VarDecl);
    bloob!(add_if,If);
    bloob!(add_while,While);
    bloob!(add_literal,Literal);
    bloob!(add_unary,Unary);
    bloob!(add_call,Call);
    bloob!(add_binary,Binary);
    bloob!(add_assignment,Assignment);
    bloob!(add_return,Return);
    bloob!(add_program,Program);

    fn parse_fndecl(&mut self, ts: TknSlice) -> Result<Rc<FnDecl>> {
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
                args.push(self.ast.add_fnarg(FnArg::new(id.clone(),0)));
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
        let block = self.parse_block(ts.sub(i+1,ts.end()))?;
        let fnd = FnDecl::new(ts.loc(0), fn_name.clone(),args,block,0);
        return Ok(self.add_fndecl(fnd));


    }

    //NOTE: removes semicolons
    fn parse_block(&mut self, ts: TknSlice) -> Result<Rc<Block>> {
        //<stmt>;
        //<stmt>;
        //...
        let mut b = Block::new(ts.loc(0), vec!(),0);
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
                let stmt = self.parse_decl(ts2)?;
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

        return Ok(Rc::new(b));
    }


    fn parse_vardecl(&mut self,ts: TknSlice) -> Result<Rc<VarDecl>> {
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

        let expr = self.parse_expr(ts.sub(3,0))?;

        let vd = self.add_vardecl(VarDecl::new(ts.loc(0), iname, expr,0));
        return Ok(vd);
    }


    fn parse_if(&mut self,ts: TknSlice) -> Result<Rc<If>> {
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
            let cond = self.parse_expr(ts.sub(1,i))?;
            
            // parse the statemet (then)
            let idx = i;
            while ts.end() != (i+1) && ts.get(i + 1).tkn_type != TokenType::Else {
                    i += 1;
            }

            let then = self.parse_stmt(ts.sub(idx,
                if ts.get(i+1).tkn_type == TokenType::Else { i } else { i + 2}))?;

            // if there is more left, parse or_else
            if ts.get(i+1).tkn_type == TokenType::Else {
                let idx = i + 2;
                i = ts.end();
                while ts.get(i).tkn_type != TokenType::RightParen {
                    i -= 1;
                }

                let or_else = self.parse_stmt(ts.sub(idx,i))?;

                let ify = self.add_if(If::new(ts.loc(0), cond, then, Some(or_else),0));
                return Ok(ify);
            }
            let ify = self.add_if(If::new(ts.loc(0), cond, then, None,0));
            return Ok(ify);
        }

        return Err(new_err(ts.loc(ts.end()),"messed up if statement somehow"));
    }

    fn parse_literal(&mut self, mut ts: TknSlice) -> Result<Rc<Literal>> {
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
        let lt = self.add_literal(Literal::new(maybe_lit.locus, l, 0));
        return Ok(lt);
    }



    fn parse_unary(&mut self, mut ts: TknSlice) -> Result<Rc<Unary>> {
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

        let sub_expr = self.parse_expr(ts)?;

        let un = self.add_unary(Unary::new(una_op, sub_expr, loc,0));
        return Ok(un);
    }

    fn parse_expr(&mut self, mut ts: TknSlice) -> Result<Expr> {
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
            let lit = self.parse_literal(ts)?;

            //this is a move
            let lit_expr: Expr = Expr::Literal(lit);
            return Ok(lit_expr);
        }

        //Second, try to parse a unary.
        if ts.size() > 1 && UnaryOp::from_tkntype(&ts.get(0).tkn_type).is_some() {
            let una: Rc<Unary> = self.parse_unary(ts)?;

            //this is a move
            let una_expr: Expr = Expr::Unary(una);
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
                    arglist.push(self.parse_expr(tse)?);
                    prison_begin = i + 1;

                }
                i += 1;
            }
            if prison_begin < ts.end() {
                //grab that last dude (˶˃ᵕ˂˶).ᐟ.ᐟ
                let tse = ts.sub(prison_begin,ts.end());
                arglist.push(self.parse_expr(tse)?);
            }
            let c = self.add_call(Call::new(ts.loc(0),fnname.clone(),arglist,0));
            return Ok(Expr::Call(c));
        }}

        //Fourth, try to parse an assignment.
        if ts.size() > 2 && ts.get(1).tkn_type == TokenType::Equal {
            let TokenType::Identifier(ref fnname) = ts.get(0).tkn_type else {
                return Err(new_err(ts.loc(0), "LHS of assignment is not identifier"));
            };
            let sub = self.parse_expr(ts.sub(2,0))?;
            let ass = self.add_assignment(Assignment::new(ts.loc(1),fnname.clone(), sub,0));
            return Ok(Expr::Assignment(ass));
        }

        /*
         ******************************************
         * Attempting to parse binary operators now
         ******************************************
         */
        let mut head = ts.end() as isize;
        let mut paren_order = 0;
        //mapping from operator -> location wrt ts.
        let mut bop = ast::BinOp::min();
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
        let left: Expr = self.parse_expr(ts.sub(0,bop_loc))?;
        let right: Expr = self.parse_expr(ts.sub(bop_loc+1,0))?;
        let bin = self.add_binary(Binary::new(bop_loc,bop,left,right,0));
        return Ok(Expr::Binary(bin));
    }


    //NOTE: Does !NOT! parse semicolons.
    fn parse_stmt(&mut self, ts: TknSlice) -> Result<Stmt> {
        if ts.size() == 0 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }

        let first = ts.get(0);
        match first.tkn_type {
            TokenType::Print => {
                let sub = self.parse_expr(ts.sub(1,0))?;
                let prt = self.add_print(Print::new(ts.loc(0),sub,0));
                return Ok(Stmt::Print(prt)); 
            },
            TokenType::If => {
                return Ok(Stmt::If(self.parse_if(ts.sub(0,0))?)); 
            },
            TokenType::While => {
                return Err(new_err(ts.loc(0),"idk bcs while loop not implemented yet"));
            },
            TokenType::Return => {
                return Err(new_err(ts.loc(0),"idk bcs return not implemented yet"));
            },
            _ => {
                let sub = self.parse_expr(ts.sub(0,0))?;
                return Ok(Stmt::Expr(sub)); 
            }
        }

    }


    fn parse_decl(&mut self, ts: TknSlice) -> Result<Decl> {
        if ts.size() < 2 {
            return Err(new_err(ts.loc(0),"Emptiness"));
        }

        let first = ts.get(0);
        let last = ts.get(ts.end());

        /*
        if first.tkn_type == TokenType::LeftBrace && last.tkn_type == TokenType::RightBrace {
            return Ok(Decl::Stmt(*self.parse_stmt(ts.sub(1,0))?));
        }
        */

        if last.tkn_type != TokenType::Semicolon {
            return Err(new_err(ts.loc(ts.end()),"forgot semicolon?"));
        }

        if first.tkn_type == TokenType::Fun {
            return Ok(Decl::FnDecl(self.parse_fndecl(ts.sub(0,ts.end()))?));
        }
        if first.tkn_type == TokenType::Var {
            return Ok(Decl::VarDecl(self.parse_vardecl(ts.sub(0,ts.end()))?));
        }

        return Ok(Decl::Stmt(self.parse_stmt(ts.sub(0,ts.end()))?));
    }


    //NOTE: Does !NOT! eat semicolons.
    fn parse_program(&mut self, ts: TknSlice) -> Result<Rc<Program>> {
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
        let mut p = Program { nodeid: 0, locus: 0, decls: vec!() };
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
                let decl = self.parse_decl(ts2)?;
                p.decls.push(decl);
                loc_old = loc+1;
            }
            loc += 1;
        }
        let pp = self.add_program(p);
        return Ok(pp);
    }
    fn new() -> Self {
        return ParsePass {ast: AstStore::new(), success: false };
    }

    fn parse(&mut self,tkns: Vec<Token>) {
        let tkn_slice = TknSlice { tkns: &tkns, start: 0, end: tkns.len() };

        let maybe_expr = self.parse_program(tkn_slice);

        if let Err(e) = maybe_expr {
            e.emit();
        } else {
            self.success = true;
        }
    }
    pub fn get_program(&self) -> Rc<Program> {
        assert!(self.success);
        return self.ast.get_root();
    }
    pub fn success(&self) -> bool {
        return self.success;
    }
}

pub fn parse(tkns: Vec<Token>) -> Option<ParsePass> {
    let mut pp = ParsePass::new();
    pp.parse(tkns);
    if pp.success() {
        return Some(pp);
    } else {
        return None;
    }
}
