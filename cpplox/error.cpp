#include "error.h"
#include "common.h"
#include <optional>
#include <cstring>

using std::string;
using std::optional;

namespace Error {

//TODO: Fix this hack
static const string *source = nullptr;

void emit(const char *msg) {
  puts(msg);
}

void emit(const char *msg, unsigned locus) {
  printf("\033[31;1mERROR:\033[0m %s\n",msg);
  assert(source != nullptr);

  unsigned begin = locus;
  unsigned end = locus;
  const string &data = *source;

  char pad[] = "  | ";

  while (begin != 0) {
    if (data[begin] == '\n')
      break;
    begin-=1;
  }
  if (data[begin] == '\n')
    begin+=1;

  while (end != data.size()) {
    if (data[end] == '\n')
      break;
    end+=1;
  }

  locus -=begin;


  puts(pad);
  cout << pad;
  for (unsigned i = begin; i < end; i++)
    putchar(data[i]);

  putchar('\n');
  cout << pad;

  for (unsigned i = 0; i < locus; i++)
    putchar(' ');

  puts("^\n");
}

void error_init(const string *s) {
  source = s; 
}

} /* namespace Error */
