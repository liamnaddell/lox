#include "ast_fwd.h"

//TODO: Right namespace?
namespace AST {
class visitor {
  public:
    virtual void visit(literal &l) = 0;
    virtual void visit(binary &l) = 0;
    virtual void visit(unary &l) = 0;
};

class visitable {
  public:
  virtual void accept(visitor &v) = 0;
};
}
