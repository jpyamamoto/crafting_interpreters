use std::collections::HashMap;

use ordered_float::OrderedFloat;

use super::error::{Error, Error::Scanner};
use super::token::{InternalValue, Token, TokenType};

struct ScannerState {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and", TokenType::And);
        m.insert("class", TokenType::Class);
        m.insert("else", TokenType::Else);
        m.insert("false", TokenType::False);
        m.insert("for", TokenType::For);
        m.insert("fun", TokenType::Fun);
        m.insert("if", TokenType::If);
        m.insert("nil", TokenType::Nil);
        m.insert("or", TokenType::Or);
        m.insert("print", TokenType::Print);
        m.insert("return", TokenType::Return);
        m.insert("super", TokenType::Super);
        m.insert("this", TokenType::This);
        m.insert("true", TokenType::True);
        m.insert("var", TokenType::Var);
        m.insert("while", TokenType::While);
        m
    };
}

impl ScannerState {
    fn advance(&mut self) -> char {
        let curr_char = self.current_char();
        self.current += 1;
        curr_char
    }

    fn current_char(&self) -> char {
        nth_char(&self.source, self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
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
}

pub fn scan_tokens(source: String) -> Result<Vec<Token>, Error> {
    let mut state = ScannerState {
        source,
        tokens: vec![],
        start: 0,
        current: 0,
        line: 0,
    };

    while !state.is_at_end() {
        state.start = state.current;
        scan_token(&mut state)?;
    }

    state.tokens.push(Token {
        type_token: TokenType::Eof,
        lexeme: "".to_string(),
        line: state.line,
        value: None,
    });

    Ok(state.tokens)
}

fn scan_token(state: &mut ScannerState) -> Result<(), Error> {
    let c = state.advance();

    match c {
        '(' => {
            add_token(state, TokenType::LeftParen, None);
            Ok(())
        }
        ')' => {
            add_token(state, TokenType::RightParen, None);
            Ok(())
        }
        '{' => {
            add_token(state, TokenType::LeftBrace, None);
            Ok(())
        }
        '}' => {
            add_token(state, TokenType::RightBrace, None);
            Ok(())
        }
        ',' => {
            add_token(state, TokenType::Comma, None);
            Ok(())
        }
        '.' => {
            add_token(state, TokenType::Dot, None);
            Ok(())
        }
        '-' => {
            add_token(state, TokenType::Minus, None);
            Ok(())
        }
        '+' => {
            add_token(state, TokenType::Plus, None);
            Ok(())
        }
        ';' => {
            add_token(state, TokenType::Semicolon, None);
            Ok(())
        }
        '*' => {
            add_token(state, TokenType::Star, None);
            Ok(())
        }
        '?' => {
            add_token(state, TokenType::Mark, None);
            Ok(())
        }
        ':' => {
            add_token(state, TokenType::Colon, None);
            Ok(())
        }
        '!' => {
            let matches = match_char(state, '=');
            add_token(
                state,
                if matches {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                },
                None,
            );
            Ok(())
        }
        '=' => {
            let matches = match_char(state, '=');
            add_token(
                state,
                if matches {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                },
                None,
            );
            Ok(())
        }
        '<' => {
            let matches = match_char(state, '=');
            add_token(
                state,
                if matches {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                },
                None,
            );
            Ok(())
        }
        '>' => {
            let matches = match_char(state, '=');
            add_token(
                state,
                if matches {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                },
                None,
            );
            Ok(())
        }
        '/' => {
            let matches_single = match_char(state, '/');
            let matches_multiline = match_char(state, '*');
            if matches_single {
                comment(state)
            } else if matches_multiline {
                let _ = multiline_comment(state);
            } else {
                add_token(state, TokenType::Slash, None)
            };

            Ok(())
        }
        ' ' => Ok(()),
        '\r' => Ok(()),
        '\t' => Ok(()),
        '\n' => {
            state.line += 1;
            Ok(())
        }
        '"' => Ok(string(state)?),
        c if c.is_ascii_digit() => {
            number(state);
            Ok(())
        }
        c if is_alpha_underscore(c) => {
            identifier(state);
            Ok(())
        }
        _ => Err(Scanner {
            line: state.line,
            message: "Unexpected character".to_string(),
        }),
    }
}

fn match_char(state: &mut ScannerState, expected: char) -> bool {
    if state.is_at_end() {
        return false;
    }

    if state.current_char() != expected {
        return false;
    }

    state.current += 1;
    true
}

fn add_token(state: &mut ScannerState, token: TokenType, value: Option<InternalValue>) {
    let text: String = substring(&state.source, state.start, state.current);

    let token = Token {
        type_token: token,
        lexeme: text,
        line: state.line,
        value,
    };

    state.tokens.push(token);
}

fn comment(state: &mut ScannerState) {
    while state.peek() != '\n' && !state.is_at_end() {
        state.advance();
    }
}

fn multiline_comment(state: &mut ScannerState) -> Result<(), Error> {
    while state.peek() != '*' && state.peek_next() != '/' {
        if state.peek() == '\n' {
            state.line += 1;
        }

        state.advance();
    }

    if state.is_at_end() {
        return Err(Scanner {
            line: state.line,
            message: "Unterminated multiline comment".to_string(),
        });
    }

    state.advance();
    state.advance();

    Ok(())
}

fn string(state: &mut ScannerState) -> Result<(), Error> {
    while state.peek() != '"' && !state.is_at_end() {
        if state.peek() == '\n' {
            state.line += 1;
        }

        state.advance();
    }

    if state.is_at_end() {
        return Err(Scanner {
            line: state.line,
            message: "Unterminated string".to_string(),
        });
    }

    state.advance();

    let value: String = substring(&state.source, state.start + 1, state.current - 1);
    add_token(state, TokenType::String, Some(InternalValue::Text(value)));

    Ok(())
}

fn number(state: &mut ScannerState) {
    while state.peek().is_ascii_digit() {
        state.advance();
    }

    if state.peek() == '.' && state.peek_next().is_ascii_digit() {
        state.advance();
        while state.peek().is_ascii_digit() {
            state.advance();
        }
    }

    let lexeme = substring(&state.source, state.start, state.current);
    let value: f64 = lexeme.parse().unwrap();
    add_token(
        state,
        TokenType::Number,
        Some(InternalValue::Number(OrderedFloat(value))),
    );
}

fn identifier(state: &mut ScannerState) {
    while is_alpha_underscore(state.peek()) {
        state.advance();
    }

    let text: String = substring(&state.source, state.start, state.current);
    let token_type: TokenType = KEYWORDS
        .get(&text as &str)
        .unwrap_or(&TokenType::Identifier)
        .clone();
    add_token(state, token_type, Some(InternalValue::Text(text)));
}

fn substring(s: &str, begin: usize, end: usize) -> String {
    s.chars().skip(begin).take(end - begin).collect()
}

fn nth_char(s: &str, n: usize) -> char {
    s.chars().nth(n).unwrap()
}

fn is_alpha_underscore(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
