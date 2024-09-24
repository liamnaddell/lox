#pragma once
#include <string>

using std::string;

namespace Error {

//Initialize singleton source instance.
void init(string &s);

//TODO: Add formatted input and locus/cursor.
void emit(const char *msg);
void emit(const char *msg, unsigned locus);
void error_init(const string &source);

}
