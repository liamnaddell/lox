#include "token.h"
#include "error.h"
#include <unordered_map>
#include <format>
using namespace Error;

namespace Token {

token::token(tkn_type type, std::string &&lexeme, unsigned locus): type(type),locus(locus) {
    this->lexeme = std::move(lexeme);
}

token::token(tkn_type type, unsigned literal, unsigned locus): type(type), literal(literal), locus(locus) {}

token::token(tkn_type type, unsigned locus): type(type), locus(locus) {}

string token::as_string() {
  string kw_string = "";
#define abom(s) \
  case s : { \
    kw_string = #s; \
    break; }

  switch (type) {
    abom(AND)
    abom(RIGHT_PAREN)
    abom(LEFT_PAREN)
    abom(BANG)
    abom(BANG_EQUAL)
    abom(EQUAL)
    abom(EQUAL_EQUAL)
    abom(GREATER)
    abom(GREATER_EQUAL)
    abom(LESS)
    abom(LESS_EQUAL)
    abom(LEFT_BRACE)
    abom(RIGHT_BRACE)
    abom(COMMA)
    abom(DOT)
    abom(MINUS)
    abom(PLUS)
    abom(SEMICOLON)
    abom(SLASH)
    abom(STAR)
    abom(CLASS)
    abom(ELSE)
    abom(FALSE)
    abom(FUN)
    abom(FOR)
    abom(IF)
    abom(NIL)
    abom(OR)
    abom(EoF)
    abom(PRINT)
    abom(RETURN)
    abom(SUPER)
    abom(THIS)
    abom(TRUE)
    abom(VAR)
    abom(WHILE)
    case IDENTIFIER:
      kw_string = std::format("IDENTIFIER: {}",lexeme);
      break;
    case STRING:
      kw_string = std::format("STRING: {}",lexeme);
      break;
    case NUMBER:
      kw_string = std::format("NUMBER: {}",literal);
      break;

    default:
      assert(false);
  }
#undef abom
  return kw_string;
}

class Tokenizer {
  public:
  const string &source;
  vector<token> &tkns;
  /* current index into source */

  unsigned i;

  bool is_next(const char *cs) {
    unsigned j = i;
    while (*cs != '\0' && j < source.size()) {
      if (*cs != source[j])
        return false;
      j++;
      cs++;
    }
    return true;
  };

  void munch_alphanum(string &out) {
    char c;
    out = "";
    for (unsigned j = i; j < source.size(); j++) {
      c = source[j];

      //probably an identifier or number of some sort;
      if (is_alphanum(c))
        out += c;
      else
        return;
    }
  }

  bool conv_num(unsigned &num, const string &s) {
    unsigned n = 0;

    for (char c : s) {
      if (!(is_num(c)))
        return false;
      //shift largest digit forwards
      n*=10;
      //Convert ascii "9" to unsigned 9.
      n+=c - 0x30;
    }
    num=n;
    return true;
  }
 
  bool handle_complex_tkns(void) {
    string maybe_ident;
    unsigned num;
    /* Need to scan identifiers, numbers, and keywords */

#define tina(s) {as_lowercase(#s), s }
    //create a hash map of available tokens.
    static std::unordered_map<string,Token::tkn_type> idents = {
      tina(AND), tina(CLASS), tina(ELSE), 
      tina(FALSE), tina(FUN), tina(FOR), tina(IF),
      tina(NIL), tina(OR), tina(PRINT), 
      tina(RETURN), tina(SUPER), tina(THIS), 
      tina(TRUE), tina(VAR), tina(WHILE)
    };
#undef tina

    //get the next thing that looks like some sort of number or identifier.
    munch_alphanum(maybe_ident);
    if (maybe_ident.size() == 0)
      return false;

    //first, check for keywords
    if (auto kw = idents.find(maybe_ident); kw != idents.end()) {
      const string &s = kw->first;
      Token::tkn_type tt = kw->second;
      tkns.push_back(token(tt,i));
      i+=s.size();
      return true;
    }

    //second, check for numbers
    if (conv_num(num,maybe_ident)) {
      tkns.push_back(token(NUMBER,num,i));
      i+=maybe_ident.size();
      return true;
    }

    if (is_valid_ident(maybe_ident)) {
      i+=maybe_ident.size();
      tkns.push_back(token(IDENTIFIER,std::move(maybe_ident),i));
      return true;
    }
    
    return false;
  }

  bool handle_string(void) {
    assert(source[i] == '"');
    string s = "";
    unsigned j = i+1;
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
      //j must be at least 2 more than i, bcs ""
      assert(j > i+1);
      j = i;
      i += 2 + s.size();
      tkns.push_back(token(STRING,std::move(s),j));
    }
    return done;
  }


  bool tokenize(void) {
    char c;
    bool res;
    while (i < source.size()) {
      c = source[i];

      switch (c) {
        case '\n':
          break;
        case ' ':
          break;
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
    return true;

error:
    Error::emit("Yea.. That doesn't look like a token to me",i);
    return false;
  }

  Tokenizer(const string &source, vector<token> &tkns): source(source),tkns(tkns), i(0) {}
};


bool tokenize_input(const string &source, vector<token> &tkns) {
  Tokenizer t(source,tkns);
  return t.tokenize();
}

} /* namespace Token */
