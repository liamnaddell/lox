
#[derive(Debug,PartialEq)]
pub enum TokenType {
  //scream cased to avoid keyword conflicts with cpp
  LeftParen, 
  // Single-character tokens.
  RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  Identifier(String), StringLit(String), Number(f64),

  // Keywords.
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

#[derive(Debug)]
pub struct Token {
    pub tkn_type: TokenType,
    pub locus:usize,
}

pub fn tokenize(_source: &String) -> Vec<Token> {
    use TokenType::*;
    //TODO: please delete my spam tuna.

    let tkn = |t: TokenType| {
        Token {tkn_type:t, locus:0}
    };
    //return vec![tkn(LeftParen),tkn(StringLit("hi".to_string())),tkn(RightParen)]
    return vec![tkn(StringLit("hi".to_string())),tkn(And),tkn(StringLit("hi".to_string()))]
}
