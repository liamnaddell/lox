#include "token.h"
namespace token {

token::token(tkn_type type, std::string &&lexeme, unsigned locus): type(type),locus(locus) {
    lexeme = std::move(lexeme);
  };

token::token(tkn_type type, unsigned literal, unsigned locus): type(type), locus(locus) {
  literal = literal;
};

}
