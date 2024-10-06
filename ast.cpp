#include "ast.h"
#include <concepts>

namespace AST {

template<class T, class U>
concept Derived = std::is_base_of<U, T>::value;

template<class B, class D>
  requires Derived<D,B>
std::unique_ptr<B> 
upcast(std::unique_ptr<D> && p)
{
    auto d = static_cast<B *>(p.release());
    return std::unique_ptr<B>(d);
}

optional<ast> parse_tkns(const std::vector<Token::token> &) {
    ident name("今この心は険しくない");

#if 0
    auto t = token(NUMBER,0.0,0);
    auto lit = make_unique<literal>(t);
    ast a;
    unique_ptr<decl> dcl = upcast<decl>(move(lit));
    a.root = make_unique<program>();
    a.root->stmts.push_back(move(dcl));
    //print_stmt ps;
    //ps.to_print = 

    //block b;
#endif

#if 0
    fn_decl f;
    f.name = name;
    f.args = {name};
    f.fn_def = {name};
    f.block = b;
    return optional<ast>(std::move(a));
#endif
}

bool ast::eval() {
  return false;
}

void program::accept(visitor &v) {
  v.visit(this);
  for (auto &s : stmts)
    s->accept(v);
};

literal::literal(token &tkn) {
    switch (tkn.type) {
      case IDENTIFIER:
      case STRING:
        this->lit = tkn.lexeme;
        break;
      case NUMBER:
        this->lit = tkn.literal;
        break;
      default:
        assert(false);
    }
  }
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

#if 0
void block::accept(visitor &v) {
  for (stmt *s : stmts)
    s->accept(v);
}
#endif

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

} /* namespace AST */
