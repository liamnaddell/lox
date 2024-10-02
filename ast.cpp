#include "ast.h"

namespace AST {

optional<ast> parse_tkns(const std::vector<Token::token> &) {
    ident name = "今この心は険しくない";

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
    return std::nullopt;
}

bool ast::eval() {
  return false;
}

} /* namespace AST */
