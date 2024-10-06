#include "ast.h"

namespace AST {

//TODO: Move somewhere else? Other header?
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
