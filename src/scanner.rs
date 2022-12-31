use std::collections::HashMap;

use crate::token::{Token, TokenType};
use crate::error;

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and",    TokenType::And);
        m.insert("class",  TokenType::Class);
        m.insert("else",   TokenType::Else);
        m.insert("false",  TokenType::False);
        m.insert("for",    TokenType::For);
        m.insert("fun",    TokenType::Fun);
        m.insert("if",     TokenType::If);
        m.insert("nil",    TokenType::Nil);
        m.insert("or",     TokenType::Or);
        m.insert("print",  TokenType::Print);
        m.insert("return", TokenType::Return);
        m.insert("super",  TokenType::Super);
        m.insert("this",   TokenType::This);
        m.insert("true",   TokenType::True);
        m.insert("var",    TokenType::Var);
        m.insert("while",  TokenType::While);
        m
    };
}

impl From<String> for Scanner {
    fn from(source: String) -> Self {
        Scanner { source: source,
                  tokens: vec![],
                  start: 0,
                  current: 0,
                  line: 0
        }
    }
}

impl Scanner {
    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            type_token: TokenType::EOF,
            lexeme: "".to_string(),
            line: self.line
        });

        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
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
                let matches = self.match_char('=');
                self.add_token(if matches { TokenType::BangEqual } else { TokenType::Bang })
            },
            '=' => {
                let matches = self.match_char('=');
                self.add_token(if matches { TokenType::EqualEqual } else { TokenType::Equal })
            },
            '<' => {
                let matches = self.match_char('=');
                self.add_token(if matches { TokenType::LessEqual } else { TokenType::Less })
            },
            '>' => {
                let matches = self.match_char('=');
                self.add_token(if matches { TokenType::GreaterEqual } else { TokenType::Greater })
            },
            '/' => {
                let matches_single = self.match_char('/');
                let matches_multiline = self.match_char('*');
                if matches_single {
                    self.comment()
                } else if matches_multiline {
                    self.multiline_comment()
                } else {
                    self.add_token(TokenType::Slash)
                }
            },
            ' ' => (),
            '\r' => (),
            '\t' => (),
            '\n' => { self.line += 1 },
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if is_alpha_underscore(c) => self.identifier(),
            _   => error(self.line, "Unexpected character".to_string()),
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.current_char() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn current_char(&self) -> char {
        nth_char(&self.source, self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let curr_char = self.current_char();
        self.current += 1;
        curr_char
    }

    fn add_token(&mut self, token: TokenType) {
        let text: String = substring(&self.source, self.start, self.current);
        self.tokens.push(Token { type_token: token,
                                 lexeme: text,
                                 line: self.line
        });
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.current_char()
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            nth_char(&self.source, self.current + 1)
        }
    }

    fn comment(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }
    }

    fn multiline_comment(&mut self) {
        while self.peek() != '*' && self.peek_next() != '/' {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            error(self.line, "Unterminated multiline comment".to_string());
            return;
        }

        self.advance();
        self.advance();
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            error(self.line, "Unterminated string".to_string());
            return;
        }

        self.advance();

        let value: String = substring(&self.source, self.start + 1, self.current - 1);
        self.add_token(TokenType::String(value));
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let lexeme = substring(&self.source, self.start, self.current);
        let value: f64 = lexeme.parse().unwrap();
        self.add_token(TokenType::Number(value));
    }

    fn identifier(&mut self) {
        while is_alpha_underscore(self.peek()) {
            self.advance();
        }

        let text: String = substring(&self.source, self.start, self.current);
        let token_type: TokenType = KEYWORDS.get(&text as &str)
                                            .or(Some(&TokenType::Identifier(text)))
                                            .unwrap().clone();
        self.add_token(token_type);
    }
}

fn substring(s: &String, begin: usize, end: usize) -> String {
    s.chars().skip(begin).take(end - begin).collect()
}

fn nth_char(s: &String, n: usize) -> char {
    s.chars().nth(n).unwrap()
}

fn is_alpha_underscore(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
