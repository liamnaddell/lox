#pragma once

#include <string>

namespace token {

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
  Eof
};


class token {
  enum tkn_type type;
  //TODO: This is a waste of space but you can't put strings in unions, 
  //and inheritence is way worse
  /* Identifier */
  std::string lexeme;
  /* TODO: Type is probably wrong */
  unsigned literal;

  /* Token location in the user input text stream */
  unsigned locus; 

  token(tkn_type type, std::string &&lexeme, unsigned locus); 
  token(tkn_type type, unsigned literal, unsigned locus);

  /*
TODO: Fix this ONCE WE KNOW WHAT WE WANT TO STORE!
  std::string as_string() {}
  */
};

}
