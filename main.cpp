#include "token.h"
#include "ast.h"
#include "error.h"
#include <cstdio>
#include <vector>

using std::string;
using Token::token;

int main(int argc, char *argv[]) {
  char buf[512];
  FILE *sc_file = nullptr;
  size_t read_bytes = 0;
  bool res;

  if (argc != 2) {
    Error::emit("You SUCK at specifying file input");
    return -1;
  }

  char *fname = argv[1];

  string source = "";

  while (( read_bytes = fread(buf,sizeof(char),512,sc_file) ) != 0)
      source.append(buf);

  Error::error_init(source);

  std::vector<token> tkns;

  res = tokenize_input(source,tkns);
  if (res == false) {
    Error::emit("Tokenize failed");
    return -2;
  }


  //TODO: T>I>N>A
  AST::ast ast;
  res = AST::parse_tkns(tkns,ast);

  if (res == false) {
    Error::emit("[畜生] Parse failed");
    return -3;
  }

  res = ast.eval();
  if (res == false) {
    Error::emit("Nice code jackass (eval failed)");
    return -3;
  }

  return 0;
}

