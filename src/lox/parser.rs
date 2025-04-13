use super::error::Error;
use super::expr::{BinaryOp, Expr, UnaryOp};
use super::stmt::Stmt;
use super::token::{Token, TokenType};

struct ParserState {
    tokens: Vec<Token>,
    current: usize,
}

impl ParserState {
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().type_token == *token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_token == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

/*
 * Grammar rules:
 *
 * expression     → ternary (, ternary)* ;
 * ternary        → equality ? ternary : ternary | equality ;
 * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
 * term           → factor ( ( "-" | "+" ) factor )* ;
 * factor         → unary ( ( "/" | "*" ) unary )* ;
 * unary          → ( "!" | "-" ) unary | primary ;
 * primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
 *
 */

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, Error> {
    let mut statements: Vec<Stmt> = vec![];
    let mut state = ParserState { tokens, current: 0 };

    while !state.is_at_end() {
        let decl = declaration(&mut state)?;

        if let Some(declaration) = decl {
            statements.push(declaration)
        }
    }

    Ok(statements)
}

fn declaration(state: &mut ParserState) -> Result<Option<Stmt>, Error> {
    if match_any_token(state, &[TokenType::Var]) {
        var_declaration(state).map(Stmt::into)
    } else {
        statement(state).map(Stmt::into)
    }
    .or_else(|err| match err {
        Error::Parse { .. } => {
            synchronize(state);
            Ok(None)
        }
        _ => Err(err),
    })
}

fn var_declaration(state: &mut ParserState) -> Result<Stmt, Error> {
    let name: Token = consume(
        state,
        &TokenType::Identifier("".to_string()),
        "Expect variable name.",
    )?;

    let initializer = if match_any_token(state, &[TokenType::Equal]) {
        let expr = expression(state)?;
        Some(Box::new(expr))
    } else {
        None
    };

    consume(
        state,
        &TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    )?;

    Ok(Stmt::Var(name, initializer))
}

fn statement(state: &mut ParserState) -> Result<Stmt, Error> {
    if match_any_token(state, &[TokenType::Print]) {
        print_statement(state)
    } else if match_any_token(state, &[TokenType::LeftBrace]) {
        let block = block(state)?;
        Ok(Stmt::Block(block))
    } else {
        expression_statement(state)
    }
}

fn block(state: &mut ParserState) -> Result<Vec<Stmt>, Error> {
    let mut statements: Vec<Stmt> = vec![];

    while !state.check(&TokenType::RightBrace) && !state.is_at_end() {
        let decl = declaration(state)?;

        if let Some(valid_decl) = decl {
            statements.push(valid_decl);
        }
    }

    consume(state, &TokenType::RightBrace, "Expect '}' after block.")?;
    Ok(statements)
}

fn print_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    let value: Expr = expression(state)?;

    consume(state, &TokenType::Semicolon, "Expect ';' after value.")?;

    Ok(Stmt::Print(Box::new(value)))
}

fn expression_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    let expr: Expr = expression(state)?;

    consume(state, &TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Stmt::Expr(Box::new(expr)))
}

fn expression(state: &mut ParserState) -> Result<Expr, Error> {
    comma(state)
}

fn comma(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = assignment(state)?;

    while match_any_token(state, &[TokenType::Comma]) {
        let operator: BinaryOp = state.previous().try_into()?;
        let right = ternary(state)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn assignment(state: &mut ParserState) -> Result<Expr, Error> {
    let expr = ternary(state)?;

    if match_any_token(state, &[TokenType::Equal]) {
        let equals = state.previous().to_owned();
        let value = assignment(state)?;

        if let Expr::Variable(token) = expr {
            return Ok(Expr::Assign(token, Box::new(value)));
        }

        Err(Error::Assignment {
            token: equals,
            message: "Invalid assignment target.".to_string(),
        })
    } else {
        Ok(expr)
    }
}

fn ternary(state: &mut ParserState) -> Result<Expr, Error> {
    let expr = equality(state)?;

    if match_any_token(state, &[TokenType::Mark]) {
        let then_expr = ternary(state)?;
        let _ = consume(
            state,
            &TokenType::Colon,
            "Expected ':' in ternary expression.",
        );
        let else_expr = ternary(state)?;
        Ok(Expr::Ternary(
            Box::new(expr),
            Box::new(then_expr),
            Box::new(else_expr),
        ))
    } else {
        Ok(expr)
    }
}

fn equality(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = comparison(state)?;

    while match_any_token(state, &[TokenType::BangEqual, TokenType::EqualEqual]) {
        let operator: BinaryOp = state.previous().try_into()?;
        let right = comparison(state)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn comparison(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = term(state)?;

    while match_any_token(
        state,
        &[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ],
    ) {
        let operator: BinaryOp = state.previous().try_into()?;
        let right = term(state)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn term(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = factor(state)?;

    while match_any_token(state, &[TokenType::Minus, TokenType::Plus]) {
        let operator: BinaryOp = state.previous().try_into()?;
        let right = factor(state)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn factor(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = unary(state)?;

    while match_any_token(state, &[TokenType::Slash, TokenType::Star]) {
        let operator: BinaryOp = state.previous().try_into()?;
        let right = unary(state)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn unary(state: &mut ParserState) -> Result<Expr, Error> {
    if match_any_token(state, &[TokenType::Bang, TokenType::Minus]) {
        let operator: UnaryOp = state.previous().try_into()?;
        let right = unary(state)?;
        Ok(Expr::Unary(operator, Box::new(right)))
    } else {
        primary(state)
    }
}

fn primary(state: &mut ParserState) -> Result<Expr, Error> {
    if match_any_token(
        state,
        &[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::Number(0.0),
            TokenType::String(String::new()),
        ],
    ) {
        return Ok(Expr::Atomic(state.previous().try_into()?));
    }

    if match_any_token(state, &[TokenType::Identifier("".to_string())]) {
        let name = state.previous().to_owned();
        return Ok(Expr::Variable(name));
    }

    if match_any_token(state, &[TokenType::LeftParen]) {
        let expr = expression(state)?;
        consume(
            state,
            &TokenType::RightParen,
            "Expect ')' after expression.",
        )?;
        return Ok(Expr::Grouping(Box::new(expr)));
    }

    if match_any_token(
        state,
        &[
            TokenType::And,
            TokenType::Or,
            TokenType::Comma,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Slash,
            TokenType::Star,
            TokenType::BangEqual,
            TokenType::Equal,
            TokenType::EqualEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ],
    ) {
        let error = Error::Parse {
            token: state.previous().clone(),
            message: "Binary operator not preceded by expression".to_string(),
        };
        return Err(error);
    }

    let error = Error::Parse {
        token: state.peek().clone(),
        message: "Expected expression".to_string(),
    };
    Err(error)
}

fn match_any_token(state: &mut ParserState, types: &[TokenType]) -> bool {
    for token_type in types {
        if state.check(token_type) {
            state.advance();
            return true;
        }
    }

    false
}

fn consume(state: &mut ParserState, token_type: &TokenType, message: &str) -> Result<Token, Error> {
    if state.check(token_type) {
        return Ok(state.advance().clone());
    }

    Err(Error::Parse {
        token: state.peek().clone(),
        message: message.to_string(),
    })
}

fn synchronize(state: &mut ParserState) {
    state.advance();

    while !state.is_at_end() {
        if state.previous().type_token == TokenType::Semicolon {
            return;
        }

        match state.peek().type_token {
            TokenType::Class
            | TokenType::Fun
            | TokenType::Var
            | TokenType::For
            | TokenType::If
            | TokenType::While
            | TokenType::Print
            | TokenType::Return => {
                return;
            }
            _ => {
                state.advance();
            }
        }
    }
}
