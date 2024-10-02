#include "token.h"
#include "ast.h"
#include "error.h"
#include "common.h"
#include <cstdio>

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

  sc_file = fopen(fname,"r");
  if (sc_file == nullptr) {
    Error::emit("You SUCK at specifying files that exist");
    return -1;
  }

  string source = "";

  while (( read_bytes = fread(buf,sizeof(char),512,sc_file) ) != 0)
      source.append(buf,read_bytes);

  Error::error_init(&source);

  std::vector<token> tkns;

  res = tokenize_input(source,tkns);
  if (res == false) {
    Error::emit("Tokenize failed");
    return -2;
  }

  for (auto tkn : tkns)
    std::cout << tkn.as_string() << ',' << '\n';


  //TODO: T>I>N>A
  AST::ast ast;
  optional<AST::ast> a = AST::parse_tkns(tkns);

  if (!a.has_value()) {
    Error::emit("[畜生] Parse failed");
    return -3;
  }

  AST::print_ast(*a);

  res = ast.eval();
  if (res == false) {
    Error::emit("Nice code jackass (eval failed)");
    return -3;
  }

  return 0;
}

