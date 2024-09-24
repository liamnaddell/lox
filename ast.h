#pragma once
#include <memory>
#include "ast.h"
#include "token.h"

using namespace Token;

namespace AST {

using std::unique_ptr;

//TODO: Define visitor pattern
class ast {
  public:
  bool eval(void);
};

class expr {};

//TODO: These are dummy classes, see Expr.java for a more accurate set
class literal : public expr {
  token tkn;
};

class unary : public expr {
  tkn_type op;
  unique_ptr<expr> sub;
};

class binary : public expr {
  tkn_type op;
  unique_ptr<expr> left;
  unique_ptr<expr> right;
};

bool parse_tkns(const std::vector<Token::token> &tkns, ast &ast);

} /* namespace AST */
