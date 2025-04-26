use std::{collections::HashMap, usize};
use crate::error::*;
use std::str::FromStr;

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
        //TODO: Return a sentinel to indicate tokenization fails!!!
        self.src = _source.into_bytes();
        while !self.at_end() {
          let res = self.tokenize();
          if let Err(e) = res {
              e.emit();
          }
        }
    }

    fn tokenize(&mut self) -> Result<()> {
        let char: char = self.advance();
        let mut extra_inc = false;
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
            '/' => self.add_token(TokenType::Slash),
            '!' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    extra_inc = true;
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    extra_inc = true;
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    extra_inc = true;
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let potential_match: bool = self.match_partner('=');
                self.add_token(if potential_match {
                    extra_inc = true;
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                });
            }
            '"' => { 
                self.add_token_string()?;
            }
            '#' => {
                while self.advance() != '\n' {}
            }
            _ => {
                self.locus -=1;
                if is_numeric(char) {
                    self.add_token_num()?;
                } else if is_alpha(char) || char == '_' {
                    self.add_token_identifier()?;
                } else {
                    return Err(new_err(self.locus+1,"Unknown token in token stream"));
                }
            }
        }

        if extra_inc {
            self.advance();
        }
        Ok(())
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
        return self.locus == self.src.len();
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
    fn add_token_string(&mut self) -> Result<()> {
        let i: usize = self.locus;

        while !self.at_end() && self.peek() != '"' {
            self.advance();
        }

        if self.at_end() {
            return Err(CompileError::from_str(
                self.locus,
                "Forgot to terminate your string ffs",
            ));
        }

        assert!(self.peek() == '"');

        self.advance();

        let text = self.src[i..(self.locus -1)].to_vec();

        match String::from_utf8(text) {
            Ok(string) => Ok(self.add_token(
              TokenType::StringLit(string))),
            Err(_) => Err(CompileError::from_str(
                self.locus,
                "what are u trying to sneak in your string?",
            )),
        }
    }

    // for handling numbers since we digitS
    fn add_token_num(&mut self) -> Result<()> {
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

        let stri = String::from_utf8(nums).map_err(|_| new_err(self.locus, "What a NUMBER!!"))?;


        let num = f64::from_str(&stri).map_err(|_| new_err(self.locus,"NICE NUMBER!!"))?;
        self.add_token(TokenType::Number(num));

        Ok(())
    }

    // for handling identifiers
    fn add_token_identifier(&mut self) -> Result<()> {
        let i: usize = self.locus;

        while is_alphanumeric(self.peek_next()) {
            self.advance();
        }

        if self.at_end() {
            return Err(CompileError::from_str(self.locus, "Forgot something?"));
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
            Err(_) => Err(CompileError::from_str(
                self.locus,
                "what are u trying to sneak in your identifier?",
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
      return is_alpha(c) || is_numeric(c) || c == '_';
  }

