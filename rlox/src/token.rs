
#[derive(Debug)]
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

fn tokenize(_source: &String) -> Vec<Token> {
    //TODO: T>I>N_A
    unimplemented!();
}
