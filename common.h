#pragma once

#include <string>
#include <vector>
#include <cassert>
#include <memory>
#include <iostream>
#include <optional>
#include <variant>
#include <format>

using std::string;
using std::vector;
using std::cout;
using std::cin;
using std::cerr;
using std::unique_ptr;
using std::make_unique;
using std::optional;
using std::nullopt;
using std::variant;
using std::format;

static inline bool is_num(char c) {
  return c >= '0' && c <= '9';
}

static inline bool is_upper(char c) {
  return c >= 'A' && c <= 'Z';
}

static inline bool is_lower(char c) {
  return c >= 'a' && c <= 'z';
}

static inline bool is_alpha(char c) {
  return is_lower(c) || is_upper(c);
}

static inline bool is_alphanum(char c) {
  return is_alpha(c) || is_num(c);
}

static inline void to_lowercase(string &s) {
  for (char &c : s)
    if (is_upper(c))
      c += 'a' - 'A';
}

static inline string as_lowercase(string s) {
  to_lowercase(s);
  return s;
}

static inline bool is_valid_ident(string s) {
  assert(s != "");
  for (char c : s)
    if (!is_alphanum(c))
      return false;
  
  return is_alpha(s[0]);
}

class ident {
  private:
    string inner;
  public:
    ident(string i) {
      assert(is_valid_ident(i));
      inner = i;
    };
};
