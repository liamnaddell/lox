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
    pub lexeme: usize,
}

fn tokenize(_source: &String) -> Vec<Token> {
    //TODO: T>I>N_A
    unimplemented!();
}

// taking creative liberty thing (i forgot the word rn)

struct Tokenizer {
    // src<usize> causes uncomfortable scenario, look at add_token_string
    src: Vec<u8>,
    tkns: Vec<Token>,
    locus: usize,
}

impl Tokenizer {
    fn initiate_tokenization(&mut self, _source: String) {
        self.src = _source.into_bytes();
        // COMMENT: if we were to make src<usize> this is what we would need 
        // self.src = _source
        //     .chars()
        //     .map(|x| x.to_digit(10).unwrap() as usize)
        //     .collect();
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
            // my ? are giving me errors AAAAAA
            '"' => self.add_token_string()?,
            _ => {
                if Self::is_numeric(char) {
                    self.add_token_num()?;
                } else if Self::is_alpha(char) {
                    self.add_token_identifier()?;
                }
            }
        }
    }

    // helpers:
    // move to next unit
    fn advance(&mut self) -> char {
        self.locus += 1;
        return char::from(self.src[self.locus - 1]);
        // SWITCH FOR src<usize>
        // return char::from_digit(self.src[self.locus - 1] as u32, 10).unwrap();
    }

    // see what the next unit is
    fn peek_next(&mut self) -> char {
      return char::from(self.src[self.locus + 1]);
        // SWITCH FOR src<usize>
        // return char::from_digit(self.src[self.locus + 1] as u32, 10).unwrap();
    }

    // check to see if we reached end
    fn at_end(&mut self) -> bool {
        return self.locus == self.src.len();
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
        return Self::is_alpha(c) || Self::is_numeric(c);
    }

    // check to see if the next character matches c
    fn match_partner(&mut self, c: char) -> bool {
        if self.peek_next() == c {
            return true;
        }
        return false;
    }

    // Token Creators:
    fn add_token(&mut self, tkn_type: TokenType) {
        self.tkns.push(Token {
            tkn_type: tkn_type,
            locus: self.locus,
            // ISSUE WITH U8 NOW::::
            lexeme: self.src[self.locus] as usize,
        })
    }

    // for handling strings since we need to find end quote
    fn add_token_string(&mut self) -> Result<(), TokenError> {
        let i: usize = self.locus;

        while self.peek_next() != '"' && !self.at_end() {
            self.advance();
        }

        if self.at_end() {
            return Err(TokenError::new(
                self.locus,
                "Forgot to terminate your string ffs".to_string(),
            ));
        }

        assert!(self.peek_next() == '"');

        self.advance();

        let text = self.src[i..self.locus].to_vec();
        // ISSUE POTENTIALLY HERE
        // let mut s = text.into_iter().map(|x| x.to_string().chars().nth(0).unwrap()).collect();
        // return self.tkns.push(Token {
        //   tkn_type: TokenType::StringLit(s),
        //   locus: self.locus,
        //   //not sure what lexeme would be here, do we even need lexeme?
        //   lexeme: self.locus,
        // });
        // we can technically make a token here for s but if we get an error it will be unclear why

        match String::from_utf8(text) {
            Ok(string) => Ok(self.tkns.push(Token {
                tkn_type: TokenType::StringLit(string),
                locus: self.locus,
                //not sure what lexeme would be here, do we even need lexeme?
                lexeme: self.locus,
            })),
            Err(e) => Err(TokenError::new(
                self.locus,
                "what are u trying to sneak in?".to_string(),
            )),
        }
    }

    // for handling numbers since we digitS
    fn add_token_num(&mut self) -> Result<(), TokenError> {
        let i: usize = self.locus;

        while Self::is_numeric(self.peek_next()) {
            self.advance();
        }

        if self.peek_next() == '.' && Self::is_numeric(self.peek_next()) {
            self.advance();
        }

        while Self::is_numeric(self.peek_next()) {
            self.advance();
        }

        unimplemented!();
    }

    // for handling identifiers but ???
    fn add_token_identifier(&mut self) -> Result<(), TokenError> {
        unimplemented!();
    }
}
