#pragma once

#include "TokenizerPass.h"
#include <cassert>

class ParseTree {
  private:
    /* Should never show up */
    enum TknType ty = TknType::Unk;
  public:
    enum TknType get_ty(void) const {
      assert(ty != TknType::Unk);
      return ty;
    }
};

class BinExpr : public ParseTree {
  private:
    enum Op op;
    std::unique_ptr<ParseTree> left;
    std::unique_ptr<ParseTree> right;
  public:
    BinExpr(enum Op op, std::unique_ptr<ParseTree> &&l,
        std::unique_ptr<ParseTree> &&r): op(op) {
      left=std::move(l);
      right=std::move(r);
    }
};

class UnExpr : public ParseTree {
  /* Either NOT or () */
  enum Op op;
  /* subtree */
  private:
    std::unique_ptr<ParseTree> st;
  public:
    UnExpr(std::unique_ptr<ParseTree> &&sut) {
      st=std::move(sut);
    }
};

class VarExpr : public ParseTree {
  private:
    char var_name;
  public:
    VarExpr(char v): var_name(v) {}
}
enum ParseKind {
  UnkExpr = 0,
  /* Parenthesized Expr */
  PtnExpr,
  Expr,
};

/* A region to parse, and the kind of expression that is anticipated */
class Context {
  public:
    /* Python style indexes, [start,stop) */
    unsigned start;
    unsigned stop;
    enum ParseKind to_parse;
    Context(unsigned start,unsigned stop,enum ParseKind to_parse): 
      start(start),stop(stop),to_parse(to_parse) {}
  /* We don't unique_ptr, because we expect 
   * the caller to downcast, NULLABLE!!!*/
    virtual ParseTree *parse(ParserPass &ps) = 0;
};

class ParenContext : public Context {
  ParenContext(unsigned start,unsigned stop,enum ParseKind to_parse): 
      Context(start,stop,to_parse) {}
  bool parse(ParserPass &ps);
};

class ParserPass {
  private:
    TokenizerPass tkp;
    /* A stack of contexts */
    std::vector<Context> ctx;
    /* The resultant parse tree */
    std::unique_ptr<ParseTree> ptree;
  public:
    /* Move constructor */
    ParserPass(TokenizerPass &tkp) : tkp(tkp) {}
    bool parse(void);
};
