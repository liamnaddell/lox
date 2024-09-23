#include "TokenizerPass.h"
#include <string>
#include <memory>
#include <cassert>

std::string op_to_string(enum Op op) {
  switch (op) {
    case And:
      return "and";
    case Or:
      return "or";
    case Not:
      return "Not";
    case Bij:
      return "Bij";
    case To:
      return "To";
  }
  assert(false);
}

std::string OpTkn::to_string() {
  //TODO: Fix
  return op_to_string(op);
}

TknType OpTkn::get_type() {
  return op == Not ? UnOp : BinOp;
}

std::string VarTkn::to_string() {
  return std::string(1,var);
}

TknType VarTkn::get_type() {
  return TknType::Sym;
}

std::string NotTkn::to_string() {
  return "not";
}

TknType NotTkn::get_type() {
  return TknType::UnOp;
}

std::string AndTkn::to_string() {
  return "and";
}

TknType AndTkn::get_type() {
  return TknType::BinOp;
}

std::string Paren::to_string() {
  if (this->r == true) {
    return ")";
  } else {
    return "(";
  }
}

TknType Paren::get_type() {
  Paren p(true,0);
  return TknType::Parent;
}

locus_t TokenizerPass::amt_left() const {
  assert(loc < uinp.size());
  return uinp.size() - loc;
}

void TokenizerPass::tokenize() {
  std::string &uinp = this->uinp;
  unsigned n = uinp.size();
  locus_t nloc;
  std::unique_ptr<Token> p = nullptr;
  for (this->loc = 0; loc < n;) {
    locus_t nloc = loc;

    if (uinp[loc] == ' ') {
      loc++;
      continue;
    }
    p = Paren::tokenize(uinp,loc,nloc);
    if (p != nullptr)
      goto success;

    p = OpTkn::tokenize(uinp,loc,nloc);
    if (p != nullptr)
      goto success;

    p = VarTkn::tokenize(uinp,loc,nloc);
    if (p != nullptr)
      goto success;

    //TODO: couldn't tokenize anything
    assert(false);
success:
      push_tkn(std::move(p));
      assert(nloc > loc);
      loc=nloc;


  }
}


