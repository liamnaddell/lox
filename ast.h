#pragma once
#include "common.h"
#include "token.h"

using namespace Token;

namespace AST {

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
  statement      → exprStmt
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
// class parsable {
//   constexpr virtual vector<parsable> char_func();
//   constexpr virtual bool is_token() {
//     return false;
//   };
// };

//TODO: Define visitor pattern
class tkn_slice {
  public:
  vector<token> &tkns;
  unsigned start;
  unsigned end;

  // no way in hell
  tkn_slice(vector<token> tkns, unsigned s, unsigned e): tkns(tkns), start(s), end(e) {}

  unsigned size() {
    assert(tkns.size() > (end-start));
    return end - start;
  }

  token peek() {
    assert (size() != 0);
    return tkns[start];
  }

  // pop is supposed to remove no?
  token pop() {
    assert( start < end);
    token t = tkns[start];
    start++;
    return t;
  }
  // maybe an advance func????????
};

class expr {
  unsigned locus;
  unsigned consumed;

  public:
  static unique_ptr<expr> parse(tkn_slice tkns) {}
};

//TODO: These are dummy classes, see Expr.java for a more accurate set
class literal : public expr {
  token tkn;
  
  public:
  literal(token t): tkn(t) {}

  static unique_ptr<literal> parse(tkn_slice tkns) {
    if (tkns.size() != 1)
      return nullptr;

    // shouldnt we also pop here even tho it is a leaf node? so it knows 
    // the slice is done?
    token h = tkns.peek();
    tkn_type t = h.get_type();

    if (t == TRUE || t == NIL || t == FALSE || t == NUMBER || t == STRING) {
      // ignoring vs code error for now
      return make_unique<literal> (*(new literal(h)));
    }
    return nullptr;
  };
};

class unary : public expr {
  tkn_type op;
  unique_ptr<expr> sub;

  public:
  unary(tkn_type op, unique_ptr<expr> sub): op(op) {
    this->sub = move(sub);
   }

  static unique_ptr<unary> parse(tkn_slice tkns) {
    token op = tkns.pop();
    //TODO: Valdiate this BS, and convert to a specific operator enum.
    // auto e = make_unique<expr>(expr::parse(tkns));
    unique_ptr e = expr::parse(tkns);
    if (e == nullptr)
      return nullptr;
    // return make_unique<unary> (new unary(op.get_type(), move(e)));
    return unique_ptr<unary> (new unary(op.get_type(), move(e)));
  }

};

// I HATE THIS AND IDK IF THERE IS ANY OTHER WAY

static void slicer(vector<token> &arr, unsigned s, unsigned e, vector<token> *sliced) {
    auto start = arr.begin() + s;
    auto end = arr.begin() + e + 1;
    copy(start, end, (*sliced).begin());
};


class binary : public expr {
  tkn_type op;
  unique_ptr<expr> left;
  unique_ptr<expr> right;

  public:
  binary(tkn_type op, unique_ptr<expr> left, unique_ptr<expr> right): op(op){ 
    this->left = move(left);
    this->right = move(right);
  }

  static unique_ptr<binary> parse(tkn_slice tkns) {
    // JUST PUTTING THIS HERE, NO WAY THIS IS ACC CORRECT
    // find op location -- need to fix this, looks way too bad and im sorry:
    token *op = nullptr;
    unsigned find_op = -1;
    for (token t: tkns.tkns) {
      if (t.get_type() == AND || t.get_type() == OR ||t.get_type() == EQUAL || t.get_type() == GREATER || t.get_type() ==  GREATER_EQUAL || t.get_type() == LESS || t.get_type() == LESS_EQUAL) {
        *op = t;
        find_op++;
      };
    };

    if (op == nullptr)
      return nullptr;

    vector<token> *l = nullptr;
    vector<token> *r = nullptr;
    slicer(tkns.tkns, tkns.start, find_op, l);
    slicer(tkns.tkns, find_op, tkns.end, r);
    if ( l == nullptr || r == nullptr)
      return nullptr;

    // auto left = make_unique<expr>(expr::parse(*(new tkn_slice((*l), tkns.start, find_op))));
    // auto right = make_unique<expr>(expr::parse(*(new tkn_slice((*r), find_op, tkns.end))));
    unique_ptr left = expr::parse(*(new tkn_slice((*l), tkns.start, find_op)));
    unique_ptr right = expr::parse(*(new tkn_slice((*r), find_op, tkns.end)));
    if (left == nullptr || right == nullptr)
      return nullptr;

    // return make_unique<binary> (new binary((*op).get_type(), move(left), move(right)));
    return unique_ptr<binary> (new binary((*op).get_type(), move(left), move(right)));
  }

};

class grouping : public expr {
  tkn_type op;

  static unsigned parse();
};

class ast {
  public:
  unique_ptr<expr> root;
  bool eval(void);
  //initialize parser, and force it to try and parse an expression. We need to seed the parser with the full user input.
  //every parsable item inherits from the abstract parser class.
  // a parser returns an amount of tokens parsed.
  // AST::Expr e;
  // e.parse(user_input);
  // Expr::parse() {
    // unsigned tokens_ate = Binop.parse()
    // if 0, parse fail
    // ...
    // error("nice expression jackass")
  // }
};

bool parse_tkns(const std::vector<Token::token> &tkns, ast &ast);

} /* namespace AST */
