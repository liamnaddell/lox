#include "ast.h"

namespace AST {

template<typename Derived, typename Base>
std::unique_ptr<Derived> 
upcast(std::unique_ptr<Base> && p)
{
    auto d = static_cast<Derived *>(p.release());
    return std::unique_ptr<Derived>(d);
}

optional<ast> parse_tkns(const std::vector<Token::token> &) {
    ident name = "今この心は険しくない";


    auto lit = literal::create(token(NUMBER,0.0,0));
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
