use super::*;

impl<I: Iterator<Item = SpannedToken>> Parser<I>{
    /// Entry point for expression parsing — handles the lowest-precedence
    /// ternary `if/else`, `lambda`, and `yield`.
    pub fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();

        // lambda
        if self.peek_token() == Token::Lambda {
            return self.parse_lambda();
        }

        // yield / yield from
        if self.peek_token() == Token::Yield {
            return self.parse_yield_expr();
        }

        let expr = self.parse_or()?;

        // ternary: `body if test else orelse`
        if self.peek_token() == Token::If {
            self.advance(); // consume `if`
            let test = self.parse_or()?;
            self.expect(Token::Else)?;
            let orelse = self.parse_expr()?; // right-associative
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::IfExpr {
                    test: Box::new(test),
                    body: Box::new(expr),
                    orelse: Box::new(orelse),
                },
                start.start..end,
            ));
        }

        // walrus: `name := expr`
        if self.peek_token() == Token::Walrus {
            self.advance();
            let value = self.parse_expr()?;
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::NamedExpr {
                    target: Box::new(expr),
                    value: Box::new(value),
                },
                start.start..end,
            ));
        }

        Ok(expr)
    }

    fn parse_lambda(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        self.advance(); // consume `lambda`
        let params = if self.peek_token() != Token::Colon {
            self.parse_params()?
        } else {
            Vec::new()
        };
        self.expect(Token::Colon)?;
        let body = self.parse_expr()?;
        let end = self.current_span.end;
        Ok(Spanned::new(
            Expr::Lambda {
                params,
                body: Box::new(body),
            },
            start.start..end,
        ))
    }

    fn parse_yield_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        self.advance(); // consume `yield`
        if self.peek_token() == Token::From {
            self.advance(); // consume `from`
            let value = self.parse_expr()?;
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::YieldFrom(Box::new(value)),
                start.start..end,
            ));
        }
        if self.peek_token() == Token::Newline
            || self.peek_token() == Token::Eof
            || self.peek_token() == Token::RParen
        {
            let end = self.current_span.end;
            return Ok(Spanned::new(Expr::Yield(None), start.start..end));
        }
        let value = self.parse_expr()?;
        let end = self.current_span.end;
        Ok(Spanned::new(
            Expr::Yield(Some(Box::new(value))),
            start.start..end,
        ))
    }

    fn parse_or(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_and()?;
        while self.peek_is_name("or") {
            self.advance();
            let right = self.parse_and()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::Or,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // and
    fn parse_and(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_not()?;
        while self.peek_is_name("and") {
            self.advance();
            let right = self.parse_not()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::And,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    fn parse_not(&mut self) -> ParseResult<Spanned<Expr>> {
        if self.peek_is_name("not") {
            let start = self.peek_span();
            self.advance();
            let operand = self.parse_not()?;
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::UnaryOp {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                start.start..end,
            ));
        }
        self.parse_comparison()
    }

    /// Check if the next token is a Name whose text equals `word`.
    pub(crate) fn peek_is_name(&mut self, word: &str) -> bool {
        if self.peek_token() != Token::Name {
            return false;
        }
        let span = self.peek_span();
        self.text(&span) == word
    }

    pub(crate) fn parse_await_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        if self.peek_token() == Token::Await {
            let start = self.peek_span();
            self.advance();
            let operand = self.parse_postfix()?;
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::Await(Box::new(operand)),
                start.start..end,
            ));
        }
        self.parse_postfix()
    }

    pub(crate) fn parse_primary(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let tok = self.peek_token();

        match tok {
            Token::Name => {
                self.advance();
                let name = self.last_text().to_string();
                Ok(Spanned::new(Expr::Name(name), start))
            }
            Token::Number => {
                self.advance();
                let text = self.last_text().to_string();
                Ok(Spanned::new(Expr::Number(text), start))
            }
            Token::String => {
                self.advance();
                let text = self.last_text().to_string();
                Ok(Spanned::new(Expr::StringLit(text), start))
            }
            Token::True => {
                self.advance();
                Ok(Spanned::new(Expr::Bool(true), start))
            }
            Token::False => {
                self.advance();
                Ok(Spanned::new(Expr::Bool(false), start))
            }
            Token::None_ => {
                self.advance();
                Ok(Spanned::new(Expr::NoneLit, start))
            }
            Token::LParen => {
                self.advance(); // consume `(`
                // Empty tuple
                if self.peek_token() == Token::RParen {
                    self.advance();
                    let end = self.current_span.end;
                    return Ok(Spanned::new(Expr::Tuple(Vec::new()), start.start..end));
                }
                let expr = self.parse_expr()?;
                // Check for tuple: `(a, b, ...)`
                if self.eat(Token::Comma) {
                    let mut elems = vec![expr];
                    if self.peek_token() != Token::RParen {
                        loop {
                            elems.push(self.parse_expr()?);
                            if !self.eat(Token::Comma) {
                                break;
                            }
                            if self.peek_token() == Token::RParen {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    let end = self.current_span.end;
                    return Ok(Spanned::new(Expr::Tuple(elems), start.start..end));
                }
                // Parenthesized expression
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::LBracket => {
                self.advance(); // consume `[`
                let mut elems = Vec::new();
                if self.peek_token() != Token::RBracket {
                    loop {
                        elems.push(self.parse_expr()?);
                        if !self.eat(Token::Comma) {
                            break;
                        }
                        if self.peek_token() == Token::RBracket {
                            break;
                        }
                    }
                }
                self.expect(Token::RBracket)?;
                let end = self.current_span.end;
                Ok(Spanned::new(Expr::List(elems), start.start..end))
            }
            Token::LBrace => {
                self.advance(); // consume `{`
                // Empty dict
                if self.peek_token() == Token::RBrace {
                    self.advance();
                    let end = self.current_span.end;
                    return Ok(Spanned::new(
                        Expr::Dict {
                            keys: Vec::new(),
                            values: Vec::new(),
                        },
                        start.start..end,
                    ));
                }
                let first = self.parse_expr()?;
                if self.eat(Token::Colon) {
                    // Dict: {key: value, ...}
                    let first_val = self.parse_expr()?;
                    let mut keys = vec![first];
                    let mut values = vec![first_val];
                    while self.eat(Token::Comma) {
                        if self.peek_token() == Token::RBrace {
                            break;
                        }
                        keys.push(self.parse_expr()?);
                        self.expect(Token::Colon)?;
                        values.push(self.parse_expr()?);
                    }
                    self.expect(Token::RBrace)?;
                    let end = self.current_span.end;
                    Ok(Spanned::new(Expr::Dict { keys, values }, start.start..end))
                } else {
                    // Set: {a, b, ...}
                    let mut elems = vec![first];
                    while self.eat(Token::Comma) {
                        if self.peek_token() == Token::RBrace {
                            break;
                        }
                        elems.push(self.parse_expr()?);
                    }
                    self.expect(Token::RBrace)?;
                    let end = self.current_span.end;
                    Ok(Spanned::new(Expr::Set(elems), start.start..end))
                }
            }
            Token::Star => {
                self.advance(); // consume `*`
                let inner = self.parse_expr()?;
                let end = self.current_span.end;
                Ok(Spanned::new(
                    Expr::Starred(Box::new(inner)),
                    start.start..end,
                ))
            }
            _ => Err(ParseError {
                message: format!("unexpected token {:?}", tok),
                span: start,
            }),
        }
    }
    
    pub(crate) fn try_aug_op(&mut self) -> Option<AugOp> {
        // TODO: Add augmented assignment tokens (+=, -=, etc.) to the lexer
        // so this can work with a single-token lookahead.
        // Currently, Logos tokenizes `+=` as `+` then `=` (two tokens).
        // We would need 2-token lookahead to detect this pattern.
        None
    }
}