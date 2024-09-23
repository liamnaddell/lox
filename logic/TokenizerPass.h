#pragma once
#include <vector>
#include <string>
#include <memory>
#include <cassert>

typedef int locus_t;

enum TknType {
  Unk = 0,
  Sym,
  UnOp,
  BinOp,
  /* Parenthesis, either R or L */
  Parent,
};

enum Op {
  And,
  Or,
  Not,
  To,
  Bij,
};

std::string op_to_string(enum Op);

//TODO: Cleanup.
static locus_t amt_left(std::string &s,locus_t loc) {
  assert(s.size() > loc);
  loc = s.size() - loc;
  return loc;
}

//TODO: Fix, cppcheck
static bool next_chars_are(std::string &uinp,locus_t loc,std::string cs) {
  //can either parse a two character
  locus_t bytes_left = amt_left(uinp,loc);
  size_t to_read = cs.size();
  assert(to_read != 0);
  bool valid = true;

  if (bytes_left < to_read)
    return false;

  for (locus_t i = 0; i < to_read; i++) {
    valid = valid && (uinp[loc+i] == cs[i]);
  }

  return valid;
}
class Token {
  private:
    locus_t loc;
  public:
    virtual std::string to_string() = 0;
    virtual TknType get_type() = 0;
    /* returns nullptr on parse failure */
    Token(locus_t loc) : loc(loc) {}
};


class VarTkn : public Token {
  private:
    char var;
  public:
    VarTkn(char var, locus_t loc) : var(var), Token(loc) {}
    std::string to_string();
    TknType get_type();
    static std::unique_ptr<Token> tokenize(std::string &s, locus_t loc,locus_t &nloc) {
      std::unique_ptr<Token> tkn = nullptr;
      locus_t left = amt_left(s,loc);

      char var_name = s[loc];

      if (!((var_name >= 'A' && var_name <= 'Z') || (var_name >= 'a' && var_name <= 'z'))) {
        //invalid
        return tkn;
      }
      nloc+=1;
      return std::unique_ptr<Token> (new VarTkn(var_name,loc));
    }
};

class Paren : public Token {
  private:
    /* Is it a right or left */
    bool r;
  public:
    Paren(bool r, locus_t loc) : r(r), Token(loc) {}
    std::string to_string();
    TknType get_type();
    static std::unique_ptr<Token> tokenize(std::string &s, locus_t loc,locus_t &nloc) {
      std::unique_ptr<Token> tkn = nullptr;
      locus_t left = amt_left(s,loc);

      char paren = s[loc];

      if (paren != '(' && paren != ')')
        return tkn;

      nloc+=1;
      return std::unique_ptr<Token> (new Paren(paren == ')',loc));
    }
};

class OpTkn : public Token {
  private:
    enum Op op;
  public:
    OpTkn(locus_t loc) : Token(loc) {}
    std::string to_string();
    TknType get_type();
    static std::unique_ptr<Token> tokenize(std::string &s, locus_t loc,locus_t &nloc) {
      OpTkn *tkn = new OpTkn(loc);
      std::unique_ptr<Token> ptr (tkn);
      locus_t left = amt_left(s,loc);
      nloc+=3;

      if (next_chars_are(s,loc,"or")) {
        tkn->op = Op::Or;
        nloc-=1;
        return ptr;
      }

      if (next_chars_are(s,loc,"and")) {
        tkn->op = Op::And;
        return ptr;
      }

      if (next_chars_are(s,loc,"not")) {
        tkn->op = Op::Not;
        return ptr;
      }

      if (next_chars_are(s,loc,"bij")) {
        tkn->op = Op::Bij;
        return ptr;
      }

      nloc=loc;
      return nullptr;
    }
};

//TODO: Refactor this to make all ops a specific token type.
class NotTkn : public Token {
  private:
  public:
    NotTkn(locus_t loc) : Token(loc) {}
    std::string to_string();
    TknType get_type();
};

class AndTkn : public Token {
  private:
  public:
    AndTkn(locus_t loc) : Token(loc) {}
    std::string to_string();
    TknType get_type();
};


class TokenizerPass {
  private:
    /* User input */
    std::string uinp;
    locus_t loc = 0;
    void push_tkn(std::unique_ptr<Token> t) {
      this->tkns.push_back(std::move(t));
    }
    locus_t next_chars_are(std::string cs);
    /* amt left to parse */
    locus_t amt_left() const;
  public:
    std::vector<std::unique_ptr<Token>> tkns;
    TokenizerPass(std::string uinp) : uinp(uinp) {}
    TokenizerPass(TokenizerPass &tkp) {
      this->uinp = std::move(tkp.uinp);
      this->tkns = std::move(tkp.tkns);
    }
    void tokenize(void);
};
