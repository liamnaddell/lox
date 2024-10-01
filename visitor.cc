#include "ast.h"
#include "visitor.h"

namespace AST {

virtual void visitor::program(program &l) {
  for (unique_ptr<decl> &s : l.stmts)
    *s.accept(this);
}


class ast_visitor : public de{

};
void print_ast(const ast &tree) {
  ast_visitor v;
  v.visit(*tree.root);
}
} /* AST */
