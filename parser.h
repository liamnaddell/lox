#pragma once

#include __FILE__
#include "common.h"
#include "token.h" // BUT I DONT WNAT THIS HERE?


class parser {
    // set of tokens that this parser is handling
    vector<Token::token> tkns;
    Token::token *curr;

    virtual unsigned parse() = 0; 

    // not sure what to return
    
    void set_tkns(vector<Token::token> &&ts) {
        tkns = move(ts);
        //  WHAT THE ACTASDUFHASOIJFIOAJDFIO
        curr = tkns.begin();
    };

    // get next token if possible
    void peek_next_tkn(){ 
        if (curr->get_type() != EoF) {
            curr++;
        }
    };

    // match token type:
    // bool match_tken(Token::token token){
    //     return
    // };
};
