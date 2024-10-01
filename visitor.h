#include "ast_fwd.h"

//TODO: Right namespace?
namespace AST {
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

class visitable {
  public:
  virtual void accept(visitor &v) = 0;
};

void print_ast(const ast&);
}
