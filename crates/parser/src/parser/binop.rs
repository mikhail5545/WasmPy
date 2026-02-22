use super::*;

impl<I: Iterator<Item = SpannedToken>> Parser<I> {
    // Comparison: chained, e.g. `1 < x < 10`
    pub fn parse_comparison(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let left = self.parse_bitor()?;
        let mut ops = Vec::new();
        let mut comparators = Vec::new();

        loop {
            let cmp = match self.peek_token() {
                Token::Eq => Some(CmpOp::Eq),
                Token::NotEq => Some(CmpOp::NotEq),
                Token::Lt => Some(CmpOp::Lt),
                Token::Gt => Some(CmpOp::Gt),
                Token::Le => Some(CmpOp::LtE),
                Token::Ge => Some(CmpOp::GtE),
                Token::Name => {
                    let span = self.peek_span();
                    let word = self.text(&span);
                    match word {
                        "in" => Some(CmpOp::In),
                        "is" => Some(CmpOp::Is),
                        _ => None,
                    }
                }
                _ => None,
            };

            if let Some(op) = cmp {
                self.advance();

                // `is not`
                if op == CmpOp::Is && self.peek_is_name("not") {
                    self.advance();
                    ops.push(CmpOp::IsNot);
                } else {
                    ops.push(op);
                }

                comparators.push(self.parse_bitor()?);
            } else if self.peek_is_name("not") {
                // `not in`
                self.advance(); // consume `not`
                let span = self.peek_span();
                let word = self.text(&span);
                if word == "in" {
                    self.advance();
                    ops.push(CmpOp::NotIn);
                    comparators.push(self.parse_bitor()?);
                } else {
                    return Err(ParseError {
                        message: "expected 'in' after 'not' in comparison".into(),
                        span,
                    });
                }
            } else {
                break;
            }
        }

        if ops.is_empty() {
            Ok(left)
        } else {
            let end = self.current_span.end;
            Ok(Spanned::new(
                Expr::Compare {
                    left: Box::new(left),
                    ops,
                    comparators,
                },
                start.start..end,
            ))
        }
    }

    // |
    fn parse_bitor(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_bitxor()?;
        while self.peek_token() == Token::Pipe {
            self.advance();
            let right = self.parse_bitxor()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::BitOr,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // ^
    fn parse_bitxor(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_bitand()?;
        while self.peek_token() == Token::Caret {
            self.advance();
            let right = self.parse_bitand()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::BitXor,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // &
    fn parse_bitand(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_shift()?;
        while self.peek_token() == Token::Amp {
            self.advance();
            let right = self.parse_shift()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::BitAnd,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // << >>
    fn parse_shift(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_arith()?;
        loop {
            let op = match self.peek_token() {
                Token::ShiftLeft => BinOp::LShift,
                Token::ShiftRight => BinOp::RShift,
                _ => break,
            };
            self.advance();
            let right = self.parse_arith()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // + -
    fn parse_arith(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_term()?;
        loop {
            let op = match self.peek_token() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_term()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // * / // % @
    fn parse_term(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let mut left = self.parse_factor()?;
        loop {
            let op = match self.peek_token() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::FloorDiv => BinOp::FloorDiv,
                Token::Percent => BinOp::Mod,
                Token::At => BinOp::MatMul,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            let end = self.current_span.end;
            left = Spanned::new(
                Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                start.start..end,
            );
        }
        Ok(left)
    }

    // Unary: +x, -x, ~x
    pub(crate) fn parse_factor(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let op = match self.peek_token() {
            Token::Plus => Some(UnaryOp::Pos),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Tilde => Some(UnaryOp::Invert),
            _ => None,
        };
        if let Some(op) = op {
            self.advance();
            let operand = self.parse_factor()?; // right-associative
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::UnaryOp {
                    op,
                    operand: Box::new(operand),
                },
                start.start..end,
            ));
        }
        self.parse_power()
    }

    // ** (right-associative)
    fn parse_power(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.peek_span();
        let base = self.parse_await_expr()?;
        if self.peek_token() == Token::Pow {
            self.advance();
            let exp = self.parse_factor()?; // right-assoc: recurse into factor
            let end = self.current_span.end;
            return Ok(Spanned::new(
                Expr::BinOp {
                    left: Box::new(base),
                    op: BinOp::Pow,
                    right: Box::new(exp),
                },
                start.start..end,
            ));
        }
        Ok(base)
    }
}