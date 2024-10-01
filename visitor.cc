#include "ast.h"

namespace AST {

/*
void visitor::visit(program &l) {
  for (unique_ptr<decl> &s : l.stmts)
    *s.accept(this);
}

*/

#if 0
void fn_decl::accept(visitor &v) {
    this->ident->accept(v);
    this->args->accept(v);
    this->fn_def->accept(v);
}
#endif

class ast_visitor : public visitor {

};
void print_ast(const ast &tree) {
  ast_visitor v;
  //v.visit(*tree.root);
}
} /* AST */
