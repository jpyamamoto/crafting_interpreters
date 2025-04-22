use super::error::Error;
use super::expr::{BinaryOp, Expr, Literal, UnaryOp};
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
    if match_any_token(state, &[TokenType::Fun]) {
        function(state, "function").map(Stmt::into)
    } else if match_any_token(state, &[TokenType::Var]) {
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

fn function(state: &mut ParserState, kind: &str) -> Result<Stmt, Error> {
    let name = consume(
        state,
        &TokenType::Identifier,
        &format!("Expect {} name.", kind),
    )?;
    consume(
        state,
        &TokenType::LeftParen,
        &format!("Expect '(' after {} name.", kind),
    )?;

    let mut parameters: Vec<Token> = vec![];

    if !state.check(&TokenType::RightParen) {
        loop {
            if parameters.len() >= 255 {
                let err = Error::Report {
                    token: state.peek().clone(),
                    message: "Can't have more than 255 parameters.".to_string(),
                };

                return Err(err);
            }

            let param = consume(state, &TokenType::Identifier, "Expect parameter name.")?;
            parameters.push(param);

            if !match_any_token(state, &[TokenType::Comma]) {
                break;
            }
        }
    }

    consume(
        state,
        &TokenType::RightParen,
        "Expect ')' after parameters.",
    )?;
    consume(
        state,
        &TokenType::LeftBrace,
        &format!("Expect '{{' before {} body.", kind),
    )?;
    let body: Vec<Stmt> = block(state)?;

    Ok(Stmt::Function(name, parameters, body))
}

fn var_declaration(state: &mut ParserState) -> Result<Stmt, Error> {
    let name: Token = consume(state, &TokenType::Identifier, "Expect variable name.")?;

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
    if match_any_token(state, &[TokenType::For]) {
        for_statement(state)
    } else if match_any_token(state, &[TokenType::If]) {
        if_statement(state)
    } else if match_any_token(state, &[TokenType::Print]) {
        print_statement(state)
    } else if match_any_token(state, &[TokenType::Return]) {
        return_statement(state)
    } else if match_any_token(state, &[TokenType::While]) {
        while_statement(state)
    } else if match_any_token(state, &[TokenType::LeftBrace]) {
        let block = block(state)?;
        Ok(Stmt::Block(block))
    } else {
        expression_statement(state)
    }
}

fn while_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    consume(state, &TokenType::LeftParen, "Expect '(' after while.")?;
    let condition = expression(state)?;

    consume(state, &TokenType::RightParen, "Expect ')' after condition.")?;
    let body = statement(state)?;

    Ok(Stmt::While(Box::new(condition), Box::new(body)))
}

fn for_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    consume(state, &TokenType::LeftParen, "Expect '(' after 'for'.")?;

    let initializer = if match_any_token(state, &[TokenType::Semicolon]) {
        None
    } else if match_any_token(state, &[TokenType::Var]) {
        Some(var_declaration(state)?)
    } else {
        Some(expression_statement(state)?)
    };

    let condition = if !state.check(&TokenType::Semicolon) {
        expression(state)?
    } else {
        Expr::Atomic(Literal::True)
    };

    consume(
        state,
        &TokenType::Semicolon,
        "Expect ';' after loop condition.",
    )?;

    let increment = if !state.check(&TokenType::RightParen) {
        Some(expression(state)?)
    } else {
        None
    };

    consume(
        state,
        &TokenType::RightParen,
        "Expect ')' after for clauses.",
    )?;

    let mut body = statement(state)?;

    if let Some(increment_expr) = increment {
        body = Stmt::Block(vec![body, Stmt::Expr(Box::new(increment_expr))]);
    }

    body = Stmt::While(Box::new(condition), Box::new(body));

    if let Some(initializer_stmt) = initializer {
        body = Stmt::Block(vec![initializer_stmt, body]);
    }

    Ok(body)
}

fn if_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    consume(state, &TokenType::LeftParen, "Expect '(' after 'if'.")?;
    let condition = expression(state)?;
    consume(
        state,
        &TokenType::RightParen,
        "Expect ')' after 'if' condition.",
    )?;

    let then_branch = statement(state)?;
    let else_branch = if match_any_token(state, &[TokenType::Else]) {
        let stmt = statement(state)?;
        Some(Box::new(stmt))
    } else {
        None
    };

    Ok(Stmt::If(
        Box::new(condition),
        Box::new(then_branch),
        else_branch,
    ))
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

fn return_statement(state: &mut ParserState) -> Result<Stmt, Error> {
    let keyword = state.previous().to_owned();

    let value = if !state.check(&TokenType::Semicolon) {
        expression(state)?
    } else {
        Expr::Atomic(Literal::Nil)
    };

    consume(
        state,
        &TokenType::Semicolon,
        "Expect ';' after return value.",
    )?;

    Ok(Stmt::Return(keyword, Box::new(value)))
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

        Err(Error::Report {
            token: equals,
            message: "Invalid assignment target.".to_string(),
        })
    } else {
        Ok(expr)
    }
}

fn ternary(state: &mut ParserState) -> Result<Expr, Error> {
    let expr = or(state)?;

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

fn or(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = and(state)?;

    while match_any_token(state, &[TokenType::Or]) {
        let operator = state.previous().try_into()?;
        let right = and(state)?;
        expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn and(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = equality(state)?;

    while match_any_token(state, &[TokenType::And]) {
        let operator = state.previous().try_into()?;
        let right = equality(state)?;
        expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
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
        call(state)
    }
}

fn call(state: &mut ParserState) -> Result<Expr, Error> {
    let mut expr = primary(state)?;

    loop {
        if match_any_token(state, &[TokenType::LeftParen]) {
            expr = finish_call(state, expr)?;
        } else {
            return Ok(expr);
        }
    }
}

fn finish_call(state: &mut ParserState, callee: Expr) -> Result<Expr, Error> {
    let mut arguments: Vec<Expr> = vec![];

    if !state.check(&TokenType::RightParen) {
        loop {
            if arguments.len() >= 255 {
                let err = Error::Report {
                    token: state.peek().clone(),
                    message: "Can't have more than 255 arguments.".to_string(),
                };

                return Err(err);
            }

            // The expression is an assignment to avoid arguments being read as a comma expr.
            let expr_arg = assignment(state)?;
            arguments.push(expr_arg);

            if !match_any_token(state, &[TokenType::Comma]) {
                break;
            }
        }
    };

    let paren = consume(state, &TokenType::RightParen, "Expect ')' after arguments.")?;
    Ok(Expr::Call(Box::new(callee), arguments, paren))
}

fn primary(state: &mut ParserState) -> Result<Expr, Error> {
    if match_any_token(
        state,
        &[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::Number,
            TokenType::String,
        ],
    ) {
        return Ok(Expr::Atomic(state.previous().try_into()?));
    }

    if match_any_token(state, &[TokenType::Identifier]) {
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
