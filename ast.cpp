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

    auto t = token(NUMBER,0.0,0);
    auto lit = make_unique<literal>(t);
    ast a;
    unique_ptr<decl> dcl = upcast<decl>(move(lit));
    a.root = make_unique<program>();
    a.root->stmts.push_back(move(dcl));
    //print_stmt ps;
    //ps.to_print = 

    //block b;

#if 0
    fn_decl f;
    f.name = name;
    f.args = {name};
    f.fn_def = {name};
    f.block = b;
#endif
    return optional<ast>(std::move(a));
}

bool ast::eval() {
  return false;
}

void program::accept(visitor &v) {
  v.visit(this);
  for (auto &s : stmts)
    s->accept(v);
};

} /* namespace AST */
