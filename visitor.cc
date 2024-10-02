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

class ast_printer : public visitor {
  public:
    virtual void visit(program &l) {
      puts("program");
    }
    virtual void visit(literal &l) {
      puts("literal");
    }
    virtual void visit(binary &l) {
      puts("binary");
    }
    virtual void visit(unary &l) {
      puts("unary");
    }
    virtual void visit(block &l) {
      puts("block");
    }
    virtual void visit(fn_decl &l) {
      puts("fn");
    }
    virtual void visit(return_stmt &l) {
      puts("return");
    }
    virtual void visit(print_stmt &l) {
      puts("print");
    }
    virtual void visit(var_decl &l) {
      puts("var_decl");
    }
    virtual void visit(if_stmt &l) {
      puts("if");
    }
    virtual void visit(while_stmt &l) {
      puts("while");
    }
    virtual void visit(call &l) {
      puts("call");
    }
};

void print_ast(const ast &tree) {
  ast_printer v;
  v.visit(*tree.root);
}

} /* AST */
