#pragma once
#include "common.h"
#include "token.h"
//#include "ast_fwd.h"

using namespace Token;

namespace AST {

class visitor;
class visitable {
  public:
  virtual void accept(visitor &v) = 0;
};


/*
TODO: Represent the following grammar (at first)
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;

With the following rules:

Name 	        Operators   Associates
Equality 	    == != 	    Left
Comparison 	  > >= < <=   Left
Term 	        - + 	      Left
Factor       	/ *         Left
Unary 	      ! - 	      Right

Q: Do we really care what direction we associate to? Why bother?

After that: There is the advanced grammar:
program        → statement* EOF ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
And, to handle variables:
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

We need the following "Blocks" to handle environments (var declarations):
statement      → exprStmt
               | printStmt
               | block ;

block          → "{" declaration* "}" ;

For control flow:
statement      → exprStmt
               | ifStmt
               | printStmt
               | block ;

ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;

statement      → exprStmt
               | ifStmt
               | printStmt
               | whileStmt
               | block ;

whileStmt      → "while" "(" expression ")" statement ;
statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | whileStmt
               | block ;

forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;
For loops should be desuraged to while loops.

and, for functions::

unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" )* ;
declaration    → funDecl
               | varDecl
               | statement ;
funDecl        → "fun" function ;
function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;

And finally, for return:
  statement    → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

returnStmt     → "return" expression? ";" ;

TODO: Another challenge, coding up environments, specifically wrt closures.
        */

/*
 * What's in a parser...
 *
 * A parser consumes a list of tokens, and outputs an AST
 *
 * Consider a token stream as follows:
 *
 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 *
 * When parsing, we find an operator in the middle:
 *
 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 *              |
 * Now, we can divide the problem into running the parser on the left, then by running the parser on the right.
 *
 * After writing this divide-and-conquer algorithm, we essentially have two choices for how to write our token -> AST logic:
 *
 * 1. We can use spaghetti logic. Use if statements, switches, etc to map the token stream to AST
 *     -> Easy to write, requires little skill, very fast
 * 2. Formal methods
 *     -> Hard to figure out, but more learning outcomes, more maintainable.
 * 
 * Consider a map, from a C++ class which represents an AST node to a set of tokens consumed by the node.
 * For example, a var declaration consumes var <ident> = <expr> ;. Where the characters are terminals, and <expr> is a non-terminal.
 * As such, our map would map from VarDecl to {var, <ident>, =, <expr>, ;}
 * VarDecl must track <ident> and <expr>. Even though <expr> is a non-terminal, and <ident> is a terminal-ish-y-thing.
 * This sounds like a great time to use virtual methods and callbacks. =). Have the VarDecl extend some base class, and override an accept<token::Ident>, and set the inner identity.
 *
 * There are two considerations here which are left out. For one, we can have recursive CFGs, such as { <Expr>; <Expr>; ... };
 * We can restrict such a problem, which contrives the solution; to consider a CFG modified to support a list. As long as such a list can be parsed equally from the right and left,
 * we can modify our aformentioned map to indicate that we would like to parse a sequential set of <expr>'s. Our callback would push these to a vector.
 * Secondly, we can specify multiple options for what to parse next. For example, a boolean parses either true or false. To fix this, one can use inheritance, to allow for passing 
 * "equivalent" expressions between functions. The characteristic function for such a superclass would represent the disjoint union of the characteristic functions for subclasses.
 *  Such an object could be expressed through the variant framework.
 *
 * Secondly, consider the following problems:
 *  * An AST can not be represented totally by a perfect class hierarchy.
 *  * We must specify our own hierarchy, which is implicitly encoded using the previously mentioned map. 
 *
 * As a note, it's very hard to figure out where a parser will stop parsing. Consider the following collection of statements:
 *  A; { B ; C }; D. As such, if we are trying to parse [<Expr>], we must attempt to parse, starting from the right OR left:
 *  A; [Exprs]
 *  A ; {[Exprs]}; [Exprs]
 *  A ; { B; C}; D
 *  The same parse sequence would work starting from the right. Such a rule must be applied to n-ary operators.
 */



// Toplev declarations
// either stmt, block, or var decl
// or fn decl
class decl: public visitable {
public:
  int a;
};

class program  {
public:
  vector<unique_ptr<decl>> stmts;
  program() {
    stmts.reserve(0);
  }
};

class literal;
class block;

class fn_decl: public decl {
  public:
    //fn name
    //TODO: Fix literal -> ident conv
    ident name;
    vector<ident> args = {};
    unique_ptr<block> fn_def;
    virtual void accept(visitor &v);
};

/*
 * Could be one of:
 *  expr, for, if, print, return, while, or block.
 */

class stmt: public decl { };

class return_stmt {
    virtual void accept(visitor &) {};
};

class expr; 

class print_stmt: public stmt {
public:
  unique_ptr<expr> to_print;
  virtual void accept(visitor &v);
  static unique_ptr<print_stmt> create(unique_ptr<expr> v) {
    auto p = unique_ptr<print_stmt>(new print_stmt());
    p->to_print = move(v);
    return p;
  }
};

class var_decl: public decl {
public:
  /*
   * var x = value
   * x is the ident
   * value is the value
   */
  ident name;
  unique_ptr<expr> value;
  virtual void accept(visitor &v);
  static unique_ptr<var_decl> create(ident name, unique_ptr<expr> v) {
    unique_ptr<var_decl> p = unique_ptr<var_decl>(new var_decl());
    p->name = name;
    p->value = move(v);
    return p;
  }
};
//TODO: Formatting w/ clang-format rules.

class block: public decl {
  public:
  vector<unique_ptr<stmt>> stmts = {};
  virtual void accept(visitor &v);
  static unique_ptr<block> create() {
    auto p = unique_ptr<block>(new block());
    return p;
  }
};

class if_stmt: public stmt {
  public:
  unique_ptr<expr> condition;
  unique_ptr<stmt> then_stmt;
  unique_ptr<stmt> else_stmt;
  virtual void accept(visitor &v);
  static unique_ptr<if_stmt> create(unique_ptr<expr> condition, unique_ptr<stmt> then_stmt, unique_ptr<stmt> else_stmt) {
    auto p = unique_ptr<if_stmt>(new if_stmt());
    p->condition=move(condition);
    p->then_stmt=move(then_stmt);
    p->else_stmt=move(else_stmt);
    return p;
  }
};

class while_stmt: public stmt {
  public:
  unique_ptr<expr> is_true;
  unique_ptr<stmt> do_stmt;
  virtual void accept(visitor &v);
  static unique_ptr<while_stmt> create(unique_ptr<expr> is_true, unique_ptr<stmt> do_stmt) {
    auto p = unique_ptr<while_stmt>(new while_stmt());
    p->is_true=move(is_true);
    p->do_stmt=move(do_stmt);
    return p;
  }
};

class for_stmt {
    //Decornsyrup to while stmt.
};

class expr : public stmt {
public:
  unsigned locus;
};

//TODO: These are dummy classes, see Expr.java for a more accurate set
class literal : public expr {
  public:
    //TODO: Fix this later
  optional<token> tkn;
  /* visitable */
  //TODO: Is this required? Isn't default behavior just to rage quit and call nothing?
  virtual void accept(visitor &) {}
  static unique_ptr<literal> create(token tkn) {
    auto p = unique_ptr<literal>(new literal());
    p->tkn=tkn;
    return p;
  }
};

class unary : public expr {
  tkn_type op;
  unique_ptr<expr> sub;
  /* visitable */
  virtual void accept(visitor &v) {
    sub->accept(v);
  }
  static unique_ptr<unary> create(tkn_type op, unique_ptr<expr> sub) {
    auto p = unique_ptr<unary>(new unary());
    p->op=op;
    p->sub=move(sub);
    return p;
  }
};

//TODO: crafty int guy thinks calls are unary operators
//I think that's 心の病気.
//My way is 正義.
class call : public expr {
  public:
    ident fn_name;
    vector<unique_ptr<expr>> args = {};
  virtual void accept(visitor &v) {
    for (auto &a : args)
      a->accept(v);
  }
  static unique_ptr<call> create(ident fn_name) {
    auto p = unique_ptr<call>(new call());
    p->fn_name=fn_name;
    return p;
  }
};

class binary : public expr {
  public:
  tkn_type op;
  unique_ptr<expr> left;
  unique_ptr<expr> right;
  /* visitable */
  virtual void accept(visitor &v) override {
    left->accept(v);
    right->accept(v);
  }
  static unique_ptr<binary> create(tkn_type op, unique_ptr<expr> left, unique_ptr<expr> right) {
    auto p = unique_ptr<binary>(new binary());
    p->op=op;
    p->left = move(left);
    p->right = move(right);
    return p;
  }
};

class ast {
  public:
    unique_ptr<program> root;
  bool eval(void);
};

class visitor {
  public:
    virtual void visit(program &l);
    virtual void visit(literal &l);
    virtual void visit(binary &l);
    virtual void visit(unary &l);
    virtual void visit(block &l);
    virtual void visit(fn_decl &l);
    virtual void visit(return_stmt &l);
    virtual void visit(print_stmt &l);
    virtual void visit(var_decl &l);
    virtual void visit(if_stmt &l);
    virtual void visit(while_stmt &l);
    virtual void visit(call &l);
};

optional<ast> parse_tkns(const std::vector<Token::token> &tkns);
void print_ast(const ast&);

#if 0
void fn_decl::accept(visitor &v) {
    this->ident->accept(v);
    this->args->accept(v);
    this->fn_def->accept(v);
}
#endif
} /* namespace AST */
