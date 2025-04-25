use std::hash::Hash;

use ordered_float::OrderedFloat;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum TokenType {
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, String, Number,

    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Mark, Colon,

    Eof
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InternalValue {
    Text(String),
    Number(OrderedFloat<f64>),
}

#[derive(Clone, Debug)]
pub struct Token {
    pub type_token: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub value: Option<InternalValue>,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.type_token == other.type_token
            && self.lexeme == other.lexeme
            && self.value == other.value
    }
}

impl Eq for Token {}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_token.hash(state);
        self.lexeme.hash(state);
        self.value.hash(state);
    }
}
