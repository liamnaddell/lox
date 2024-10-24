use std::{collections::HashMap, usize};

#[derive(Debug,Clone,PartialEq)]
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

    Eof,
}

#[derive(Debug)]
struct TokenError {
    locus: usize,
    msg: String,
}

impl TokenError {
    pub fn new(locus: usize, msg: String) -> TokenError {
        return TokenError { locus, msg };
    }
}

#[derive(Debug)]
pub struct Token {
    pub tkn_type: TokenType,
    pub locus: usize,
}

pub fn tokenize(_source: String) -> Vec<Token> {
  let mut tknizer: Tokenizer = Tokenizer::new();

  tknizer.initiate_tokenization(_source);
  return tknizer.tkns;
}

// taking creative liberty thing (i forgot the word rn)

#[derive(Debug)]
pub struct Tokenizer {
    src: Vec<u8>,
    pub tkns: Vec<Token>,
    locus: usize,
    keywords: HashMap<String, TokenType>,
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            src: Vec::new(),
            tkns: Vec::new(),
            locus: 0,
            keywords: vec![
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]
            .into_iter()
            .map(|(key_wrd, t_type)| (key_wrd.to_string(), t_type))
            .collect(),
        }
    }
}

impl Tokenizer {
    fn initiate_tokenization(&mut self, _source: String) {
        self.src = _source.into_bytes();
        while !self.at_end() {
          self.tokenize();
        }
    }

    fn tokenize(&mut self) {
        let char: char = self.advance();
        match char {
            ' ' | '\n' | '\t' => {}
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '"' => self.add_token_string().unwrap(),
            _ => {
                if is_numeric(char) {
                    self.add_token_num().unwrap();
                } else if is_alpha(char) {
                    self.add_token_identifier().unwrap();
                }
            }
        }
    }

    // helpers:
    // move to next unit
    fn advance(&mut self) -> char {
        self.locus += 1;
        return char::from(self.src[self.locus - 1]);
    }

    // see what the next unit is
    fn peek_next(&mut self) -> char {
      return char::from(self.src[self.locus + 1]);
    }

    // see what the next unit is
    fn peek(&mut self) -> char {
      return char::from(self.src[self.locus]);
    }

    // check to see if we reached end
    fn at_end(&mut self) -> bool {
        return (self.locus + 1) == self.src.len();
    }

    // check to see if the next character matches c
    fn match_partner(&mut self, c: char) -> bool {
        if self.peek() == c {
            return true;
        }
        return false;
    }

    // Token Creators:
    fn add_token(&mut self, tkn_type: TokenType) {
        self.tkns.push(Token {
            tkn_type: tkn_type,
            locus: self.locus,
        })
    }

    // for handling strings since we need to find end quote
    fn add_token_string(&mut self) -> Result<(), TokenError> {
        let i: usize = self.locus;

        while !self.at_end() && self.peek() != '"' {
            self.advance();
        }

        if self.at_end() {
            return Err(TokenError::new(
                self.locus,
                "Forgot to terminate your string ffs".to_string(),
            ));
        }

        assert!(self.peek() == '"');

        self.advance();

        let text = self.src[i..(self.locus -1)].to_vec();

        match String::from_utf8(text) {
            Ok(string) => Ok(self.add_token(
              TokenType::StringLit(string))),
            Err(e) => Err(TokenError::new(
                self.locus,
                "what are u trying to sneak in your string?".to_string(),
            )),
        }
    }

    // for handling numbers since we digitS
    fn add_token_num(&mut self) -> Result<(), TokenError> {
        let i: usize = self.locus;

        while is_numeric(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && is_numeric(self.peek_next()) {
            self.advance();
        }

        while is_numeric(self.peek()) {
            self.advance();
        }

        let nums = self.src[i..self.locus].to_vec();

        match String::from_utf8(nums) {
            Ok(string) => Ok(self.add_token(
                TokenType::Number(string.parse::<f64>().unwrap())
            )),
            Err(e) => Err(TokenError::new(
                self.locus,
                "what are u trying to sneak in your number?".to_string(),
            )),
        }
    }

    // for handling identifiers
    fn add_token_identifier(&mut self) -> Result<(), TokenError> {
        let i: usize = self.locus - 1;

        while is_alphanumeric(self.peek_next()) {
            self.advance();
        }

        if self.at_end() {
            return Err(TokenError::new(self.locus, "Forgot something?".to_string()));
        }

        self.advance();

        let val = std::str::from_utf8(&self.src[i..self.locus]);

        match val {
            Ok(string) => {
              let t_type = self.keywords.get(string);
              match t_type {
                Some(ttype) => Ok(self.add_token(ttype.clone())),
                None => Ok(self.add_token(
                  TokenType::Identifier((*string).to_string()),
                )),
              }
            }
            Err(e) => Err(TokenError::new(
                self.locus,
                "what are u trying to sneak in your identifier?".to_string(),
            )),
        }
    }
}

  // self explanatory
  fn is_alpha(c: char) -> bool {
      return c.is_alphabetic();
  }

  // self explanatory
  fn is_numeric(c: char) -> bool {
      return c.is_ascii_digit();
  }

  // self explanatory
  fn is_alphanumeric(c: char) -> bool {
      return is_alpha(c) || is_numeric(c);
  }

