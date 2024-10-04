#include "ast.h"

namespace AST {

/*
void visitor::visit(program &l) {
  for (unique_ptr<decl> &s : l.stmts)
    *s.accept(this);
}

*/

void fn_decl::accept(visitor &v) {
    this->fn_def->accept(v);
}

void print_stmt::accept(visitor &v) {
  this->to_print->accept(v);
}

void var_decl::accept(visitor &v) {
  this->value->accept(v);
}

void block::accept(visitor &v) {
  for (unique_ptr<stmt> &s : stmts)
    s->accept(v);
}

#if 0
void visitor::visit(call &) { }
void visitor::visit(program &) {}
void visitor::visit(literal &) {}
void visitor::visit(binary &) {}
void visitor::visit(unary &) {}
void visitor::visit(block &) {}
void visitor::visit(fn_decl &) {}
void visitor::visit(return_stmt &) {}
void visitor::visit(var_decl &) {}
void visitor::visit(if_stmt &) {}
void visitor::visit(while_stmt &) {}
void visitor::visit(print_stmt &) {}
#endif

class ast_printer : public visitor {
  public:
    void visit(program *) override {
      puts("program:");
    }
    virtual void visit(literal *l) override {
      cout << l->as_string() << '\n';
    }
    virtual void visit(binary *) override {
    }
    virtual void visit(unary *) override {
    }
    virtual void visit(block *) override {
      puts("block");
    }
    virtual void visit(fn_decl *) override {
      puts("fn");
    }
    virtual void visit(return_stmt *) override {
      puts("return");
    }
    virtual void visit(print_stmt *) override {
      puts("print");
    }
    virtual void visit(var_decl *) override {
      puts("var_decl");
    }
    virtual void visit(if_stmt *) override {
      puts("if");
    }
    virtual void visit(while_stmt *) override {
      puts("while");
    }
    virtual void visit(call *) override {
      puts("call");
    }
};

void print_ast(const ast &tree) {
  ast_printer v;
  tree.root->accept(v);
}

} /* AST */
