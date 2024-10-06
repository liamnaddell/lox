#pragma once

#include "common.h"
namespace Token {

enum tkn_type {
  //scream cased to avoid keyword conflicts with cpp
  LEFT_PAREN = 1,
  // Single-character tokens.
  RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  //avoid C variable EOF
  EoF
};


class token {
  public:
  enum tkn_type type;
  //TODO: This is a waste of space but you can't put strings in unions, 
  //and inheritence is way worse
  /* Identifier */
  std::string lexeme;
  /* TODO: Type is probably wrong */
  unsigned literal;

  /* Token location in the user input text stream */
  unsigned locus; 

  /* Identifier or string */
  token(tkn_type type, std::string &&lexeme, unsigned locus); 
  /* number literal */
  token(tkn_type type, unsigned literal, unsigned locus);

  /* simple token with no associated data */
  token(tkn_type type, unsigned locus);
  /* copy constructriono*/
  //token(token &);

  std::string as_string();

  tkn_type get_type() const {
    return this->type;
  };

  virtual unsigned parse(){
    return 1; //dummy, changes are needed
  }; 
};

bool tokenize_input(const string &source, vector<token> &tkns);

}
