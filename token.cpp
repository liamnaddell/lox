#include "token.h"
#include "error.h"
#include <cassert>
#include <unordered_map>
using namespace Error;

namespace Token {

token::token(tkn_type type, std::string &&lexeme, unsigned locus): type(type),locus(locus) {
    lexeme = std::move(lexeme);
  };

token::token(tkn_type type, unsigned literal, unsigned locus): type(type), locus(locus) {
  literal = literal;
};

class Tokenizer {
  public:
  const string &source;
  vector<token> &tkns;
  /* current index into source */

  unsigned i;

  bool is_next(const char *cs) {
    bool found = true;
    int j = i;
    while (*cs != '\0' && j < source.size()) {
      if (*cs != source[j])
        return false;

      j++;
    }
    return true;
  };
 
  bool handle_complex_tkns(void) {
    /* Need to scan identifiers, numbers, and keywords */

#if 0
#define tina(s) { ##s, #s }
    std::unordered_map<string,Token::tkn_type> idents =
      {tina(AND)};
#undef tina
#endif
    //TODO: Get this out of trying to find the next identifier-y-thing (i.e. next alphanum textblock)
    std::string bs = "and";
    std::unordered_map<string,Token::tkn_type> idents =
    {{"AND",AND}};

    //first, check for keywords
    if (auto kw = idents.find(bs); kw != idents.end()) {
      const string &s = kw->first;
      Token::tkn_type tt = kw->second;
      tkns.push_back(token(tt,i));
      i+=s.size();
      return true;
    }
    //TODO: Check for numbers and identifiers

    return false;
  }
  bool handle_string(void) {
    assert(source[i] == '"');
    string s = "";
    unsigned j = i;
    bool done = false;
    char c;
    while (j < source.size() && !done) {
      c=source[j];
      if (c == '"')
        done = true;
      else
        s += c;
      j+=1;
    }

    if (done) {
      i=j-1;
      tkns.push_back(token(STRING,std::move(s),i));
    }
    return done;
  }


  bool tokenize(void) {
    char c;
    bool res;
    while (i != source.size()) {
      c = source[i];

      switch (c) {
        case '(':
          tkns.push_back(token(LEFT_PAREN,i));
          break;
        case ')':
          tkns.push_back(token(RIGHT_PAREN,i));
          break;
        case '{':
          tkns.push_back(token(LEFT_BRACE,i));
          break;
        case '}':
          tkns.push_back(token(RIGHT_BRACE,i));
          break;
        case ',':
          tkns.push_back(token(COMMA,i));
          break;
        case '.':
          tkns.push_back(token(DOT,i));
          break;
        case '-':
          tkns.push_back(token(MINUS,i));
          break;
        case '+':
          tkns.push_back(token(PLUS,i));
          break;
        case ';':
          tkns.push_back(token(SEMICOLON,i));
          break;
        case '/':
          tkns.push_back(token(SLASH,i));
          break;
        case '*':
          tkns.push_back(token(STAR,i));
          break;
        case '!':
          if (is_next("!=")) {
            tkns.push_back(token(BANG_EQUAL,i));
            i+=1;
          } else {
            tkns.push_back(token(BANG,i));
          }
          break;
        case '=':
          if (is_next("==")) {
            tkns.push_back(token(EQUAL_EQUAL,i));
            i+=1;
          } else {
            tkns.push_back(token(EQUAL,i));
          }
          break;
        case '>':
          if (is_next(">=")) {
            tkns.push_back(token(GREATER_EQUAL,i));
            i+=1;
          } else {
            tkns.push_back(token(GREATER,i));
          }
          break;
        case '<':
          if (is_next("<=")) {
            tkns.push_back(token(LESS_EQUAL,i));
            i+=1;
          } else {
            tkns.push_back(token(LESS,i));
          }
          break;
        case '"':
          res = handle_string();
          if (res == true)
          // Let handle_string solve incrementing i;
            goto no_inc;
          else
            goto error;

        default:
          res = handle_complex_tkns();
          if (res == true)
            // skip i+=1;
            goto no_inc;
          else
            goto error;
      }
      i+=1;
no_inc:
      i+=0;
    }
    tkns.push_back(token(EoF,i));

error:
    return false;
  }

  Tokenizer(const string &source, vector<token> &tkns): source(source),tkns(tkns), i(0) {}
};


bool tokenize_input(const string &source, vector<token> &tkns) {
  Tokenizer t(source,tkns);
  return t.tokenize();
}

} /* namespace Token */
