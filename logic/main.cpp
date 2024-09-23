#include <iostream>
#include "TokenizerPass.h"
#include "ParserPass.h"

int main(int argc, char *argv[]) {
  std::cout << "Input a wff boolean formula\n"; 
  std::string inp;
  std::getline(std::cin,inp);
  TokenizerPass tkp(inp);
  tkp.tokenize();
  for (auto &tkn : tkp.tkns)
    std::cout << tkn->to_string() << '\n';
  ParserPass psp(tkp);

  std::cout << "Done, exiting\n";
  return 0;
}
