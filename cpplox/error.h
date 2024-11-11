#pragma once
#include <string>

using std::string;

namespace Error {

//Initialize singleton source instance.
void init(string &s);

void emit(const char *msg);
void emit(const char *msg, unsigned locus);
void error_init(const string *s);

}
