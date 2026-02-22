use super::*;

impl<I: Iterator<Item = SpannedToken>> Parser<I> {
    pub fn parse_postfix(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek_token() {
                Token::LParen => {
                    self.advance(); // consume `(`
                    let (args, kwargs) = self.parse_call_args()?;
                    self.expect(Token::RParen)?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Call {
                            func: Box::new(expr),
                            args,
                            kwargs,
                        },
                        start.start..end,
                    );
                }
                Token::Dot => {
                    self.advance(); // consume `.`
                    let attr = self.expect_name()?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Attribute {
                            value: Box::new(expr),
                            attr,
                        },
                        start.start..end,
                    );
                }
                Token::LBracket => {
                    self.advance(); // consume `[`
                    let index = self.parse_expr()?;
                    self.expect(Token::RBracket)?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Subscript {
                            value: Box::new(expr),
                            index: Box::new(index),
                        },
                        start.start..end,
                    );
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// Parse call arguments: positional and keyword.
    fn parse_call_args(
        &mut self,
    ) -> ParseResult<(Vec<Spanned<Expr>>, Vec<(String, Spanned<Expr>)>)> {
        let mut args = Vec::new();
        let mut kwargs = Vec::new();
        if self.peek_token() == Token::RParen {
            return Ok((args, kwargs));
        }
        loop {
            // Check for `name=value` keyword argument by peeking two tokens
            if self.peek_token() == Token::Name {
                let span = self.peek_span();
                let name_text = self.text(&span).to_string();
                // Consume the Name, then check if `=` follows
                self.advance();
                if self.eat(Token::Assign) {
                    let value = self.parse_expr()?;
                    kwargs.push((name_text, value));
                } else {
                    // It was a positional arg starting with a Name.
                    // We already consumed the name; build the expr and continue postfix.
                    let name_expr = Spanned::new(Expr::Name(name_text), span.clone());
                    let full_expr = self.continue_postfix(name_expr, &span)?;
                    let full_expr = self.continue_binop(full_expr)?;
                    args.push(full_expr);
                }
            } else if self.peek_token() == Token::Star {
                // *args (starred)
                let start = self.peek_span();
                self.advance();
                let inner = self.parse_expr()?;
                let end = self.current_span.end;
                args.push(Spanned::new(
                    Expr::Starred(Box::new(inner)),
                    start.start..end,
                ));
            } else {
                args.push(self.parse_expr()?);
            }
            if !self.eat(Token::Comma) {
                break;
            }
            if self.peek_token() == Token::RParen {
                break;
            }
        }
        Ok((args, kwargs))
    }

    /// Continue parsing postfix operations on an already-parsed expr.
    fn continue_postfix(
        &mut self,
        mut expr: Spanned<Expr>,
        start: &Span,
    ) -> ParseResult<Spanned<Expr>> {
        loop {
            match self.peek_token() {
                Token::LParen => {
                    self.advance();
                    let (args, kwargs) = self.parse_call_args()?;
                    self.expect(Token::RParen)?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Call {
                            func: Box::new(expr),
                            args,
                            kwargs,
                        },
                        start.start..end,
                    );
                }
                Token::Dot => {
                    self.advance();
                    let attr = self.expect_name()?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Attribute {
                            value: Box::new(expr),
                            attr,
                        },
                        start.start..end,
                    );
                }
                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(Token::RBracket)?;
                    let end = self.current_span.end;
                    expr = Spanned::new(
                        Expr::Subscript {
                            value: Box::new(expr),
                            index: Box::new(index),
                        },
                        start.start..end,
                    );
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// After parsing a primary + postfix inside call args, continue with
    /// binary operators. This handles the common arithmetic operators.
    fn continue_binop(&mut self, left: Spanned<Expr>) -> ParseResult<Spanned<Expr>> {
        let start = left.span.clone();
        let mut result = left;
        loop {
            let op = match self.peek_token() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::FloorDiv => BinOp::FloorDiv,
                Token::Percent => BinOp::Mod,
                Token::Pow => BinOp::Pow,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            let end = self.current_span.end;
            result = Spanned::new(
                Expr::BinOp {
                    left: Box::new(result),
                    op,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(result)
    }
}