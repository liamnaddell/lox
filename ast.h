#pragma once
#include "common.h"
#include "token.h"
#include "ast_fwd.h"

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

class fn_decl {
  public:
    //fn name
    ident name;
    vector<ident> args;
    block *fn_def;
    virtual void accept(visitor &v);
#if 0
    fn_decl(ident name, unique_ptr<block> fn_def): name(name) {
      this->fn_def=move(fn_def);
      args.reserve(8);
    }
#endif
    fn_decl(ident name, unique_ptr<block> fn_def);
    void push_arg(ident &b) {
      args.push_back(b);
    }
};


class return_stmt {
    virtual void accept(visitor &) {};
};

class print_stmt {
public:
  expr *to_print;
  virtual void accept(visitor &v);
  print_stmt(unique_ptr<expr> &&v);
};

class var_decl {
public:
  /*
   * var x = value
   * x is the ident
   * value is the value
   */
  ident name;
  expr *value;
  virtual void accept(visitor &v);
  var_decl(ident name, unique_ptr<expr> v);
};
//TODO: Formatting w/ clang-format rules.

class block {
  public:
  vector<stmt *> stmts;
  virtual void accept(visitor &v);
#if 0
  void push_stmt(unique_ptr<stmt> s) {
      stmts.push_back(move(s));
  }
#endif
};

class if_stmt {
  public:
  expr *condition;
  stmt *then_stmt;
  stmt *else_stmt;
  virtual void accept(visitor &v);
  if_stmt(unique_ptr<expr> condition, unique_ptr<stmt> then_stmt, unique_ptr<stmt> else_stmt);
#if 0
    {
    this->condition=move(condition);
    this->then_stmt=move(then_stmt);
    this->else_stmt=move(else_stmt);
  }
#endif
};

class while_stmt {
  public:
  expr *is_true;
  stmt *do_stmt;
  virtual void accept(visitor &v);
  while_stmt(unique_ptr<expr> is_true, unique_ptr<stmt> do_stmt);
#if 0
  while_stmt(unique_ptr<expr> is_true, unique_ptr<stmt> do_stmt) {
    this->is_true=move(is_true);
    this->do_stmt=move(do_stmt);
  }
#endif
};

class for_stmt {
  //Decornsyrup to while stmt.
};

//TODO: These are dummy classes, see Expr.java for a more accurate set
class literal {
  public:
  //TODO: Fix this later
  variant<string,unsigned> lit;
  /* visitable */
  //TODO: Is this required? Isn't default behavior just to rage quit and call nothing?
  virtual void accept(visitor &) {}
  literal(token &tkn) {
    switch (tkn.type) {
      case IDENTIFIER:
      case STRING:
        this->lit = tkn.lexeme;
        break;
      case NUMBER:
        this->lit = tkn.literal;
        break;
      default:
        assert(false);
    }
  }
  string as_string() {
    //TODO: Fix
    return format("[literal: {}]",std::get<string>(lit));
  }
};

class unary {
  //TODO: FIX!
  tkn_type op;
  expr *sub;
  /* visitable */
#if 0
  virtual void accept(visitor &v) {
    sub->accept(v);
  }
  unary(tkn_type op, unique_ptr<expr> sub): op(op) {
    this->sub=move(sub);
  }
#endif
  unary(tkn_type op, unique_ptr<expr> sub);
};

//TODO: crafty int guy thinks calls are unary operators
//I think that's 心の病気.
//My way is 正義.
class call {
  public:
    ident fn_name;
    vector<unique_ptr<expr>> args;
#if 0
  virtual void accept(visitor &v) {
    for (auto &a : args)
      a->accept(v);
  }
#endif
  call(ident fn_name);
};

class binary {
  public:
  tkn_type op;
  expr *left;
  expr *right;
  /* visitable */
#if 0
  virtual void accept(visitor &v) override {
    left->accept(v);
    right->accept(v);
  }
#endif
  binary(tkn_type op, unique_ptr<expr> left, unique_ptr<expr> right);
#if 0
  binary(tkn_type op, unique_ptr<expr> left, unique_ptr<expr> right);
    {
    this->left = move(left);
    this->right = move(right);
  }
#endif
};

class expr {
  public:
    variant<literal,unary,call,binary> sub;
};

class stmt { 
  variant<print_stmt,if_stmt,while_stmt,expr,return_stmt> sub;
};

// Toplev declarations
// either stmt, block, or var decl
// or fn decl
class decl {
public:
  variant<fn_decl*,stmt*,var_decl*,block*> sub;
};

class program {
public:
  vector<decl *> stmts;
  program();
#if 0
  void push_decl(unique_ptr<decl> d) {
      stmts.push_back(move(d));
  }
#endif
  virtual void accept(visitor &v);
};

class ast {
  public:
    unique_ptr<program> root;
  bool eval(void);
};

class visitor {
  public:
    virtual void visit(program *) {};
    virtual void visit(literal *) {};
    virtual void visit(binary *) {};
    virtual void visit(unary *) {};
    virtual void visit(block *) {};
    virtual void visit(fn_decl *) {};
    virtual void visit(return_stmt *) {};
    virtual void visit(print_stmt *) {};
    virtual void visit(var_decl *) {};
    virtual void visit(if_stmt *) {};
    virtual void visit(while_stmt *) {};
    virtual void visit(call *) {};
};


optional<ast> parse_tkns(const std::vector<Token::token> &tkns);
void print_ast(const ast&);

} /* namespace AST */
