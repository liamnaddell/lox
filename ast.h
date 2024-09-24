#pragma once
#include <memory>
#include "ast.h"
#include "token.h"

namespace AST {

using std::unique_ptr;

//TODO: Define visitor pattern
class ast {

};

class expr {};

//TODO: These are dummy classes, see Expr.java for a more accurate set
class literal : public expr {
  token::token tkn;
};

class unary : public expr {
  token::tkn_type op;
  unique_ptr<expr> sub;
};

class binary : public expr {
  token::tkn_type op;
  unique_ptr<expr> left;
  unique_ptr<expr> right;
};

}

