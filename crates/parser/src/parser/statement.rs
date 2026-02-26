use super::*;

impl<I: Iterator<Item = SpannedToken>> Parser<I> {
    pub(crate) fn parse_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.peek_span();
        let stmt = match self.peek_token() {
            Token::Def => self.parse_func_def(Vec::new())?,
            Token::Class => self.parse_class_def(Vec::new())?,
            Token::If => self.parse_if()?,
            Token::While => self.parse_while()?,
            Token::For => self.parse_for()?,
            Token::Return => self.parse_return()?,
            Token::Pass => {
                self.advance();
                self.eat(Token::Newline);
                Stmt::Pass
            }
            Token::Break => {
                self.advance();
                self.eat(Token::Newline);
                Stmt::Break
            }
            Token::Continue => {
                self.advance();
                self.eat(Token::Newline);
                Stmt::Continue
            }
            Token::Del => self.parse_del()?,
            Token::Assert => self.parse_assert()?,
            Token::Raise => self.parse_raise()?,
            Token::Global => self.parse_global()?,
            Token::Nonlocal => self.parse_nonlocal()?,
            Token::Import => self.parse_import()?,
            Token::From => self.parse_from_import()?,
            Token::Try => self.parse_try()?,
            Token::With => self.parse_with()?,
            Token::At => self.parse_decorated()?,
            _ => self.parse_expr_or_assign_stmt()?,
        };
        let end = self.current_span.end;
        Ok(Spanned::new(stmt, start.start..end))
    }

    // --- Simple statements ---

    fn parse_return(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `return`
        let value = if self.peek_token() != Token::Newline && self.peek_token() != Token::Eof {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.eat(Token::Newline);
        Ok(Stmt::Return(value))
    }

    fn parse_del(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `del`
        let target = self.parse_expr()?;
        self.eat(Token::Newline);
        Ok(Stmt::Del(target))
    }

    fn parse_assert(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `assert`
        let test = self.parse_expr()?;
        let msg = if self.eat(Token::Comma) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.eat(Token::Newline);
        Ok(Stmt::Assert { test, msg })
    }

    fn parse_raise(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `raise`
        if self.peek_token() == Token::Newline || self.peek_token() == Token::Eof {
            self.eat(Token::Newline);
            return Ok(Stmt::Raise {
                exc: None,
                cause: None,
            });
        }
        let exc = self.parse_expr()?;
        let cause = if self.peek_token() == Token::From {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.eat(Token::Newline);
        Ok(Stmt::Raise {
            exc: Some(exc),
            cause,
        })
    }

    fn parse_global(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `global`
        let mut names = vec![self.expect_name()?];
        while self.eat(Token::Comma) {
            names.push(self.expect_name()?);
        }
        self.eat(Token::Newline);
        Ok(Stmt::Global(names))
    }

    fn parse_nonlocal(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `nonlocal`
        let mut names = vec![self.expect_name()?];
        while self.eat(Token::Comma) {
            names.push(self.expect_name()?);
        }
        self.eat(Token::Newline);
        Ok(Stmt::Nonlocal(names))
    }

    fn parse_import(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `import`
        let module = self.parse_dotted_name()?;
        let alias = if self.eat(Token::As) {
            Some(self.expect_name()?)
        } else {
            None
        };
        self.eat(Token::Newline);
        Ok(Stmt::Import { module, alias })
    }

    fn parse_from_import(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `from`
        let module = self.parse_dotted_name()?;
        self.expect(Token::Import)?;
        let mut names = Vec::new();
        loop {
            let name = self.expect_name()?;
            let alias = if self.eat(Token::As) {
                Some(self.expect_name()?)
            } else {
                None
            };
            names.push((name, alias));
            if !self.eat(Token::Comma) {
                break;
            }
        }
        self.eat(Token::Newline);
        Ok(Stmt::FromImport { module, names })
    }

    fn parse_dotted_name(&mut self) -> ParseResult<Vec<String>> {
        let mut parts = vec![self.expect_name()?];
        while self.eat(Token::Dot) {
            parts.push(self.expect_name()?);
        }
        Ok(parts)
    }

    fn parse_expr_or_assign_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;

        // Check for augmented assignment: +=, -=, etc.
        if let Some(aug_op) = self.try_aug_op() {
            self.advance(); // consume the operator
            self.advance(); // consume the '='
            let value = self.parse_expr()?;
            self.eat(Token::Newline);
            return Ok(Stmt::AugAssign {
                target: expr,
                op: aug_op,
                value,
                type_hint: None,
            });
        }

        // Check for type annotation: `expr: type` or `expr: type = value`
        if self.peek_token() == Token::Colon {
            self.advance(); // consume `:`
            let type_hint_spanned = self.parse_type_hint()?;

            if self.eat(Token::Assign) {
                // Annotated assignment: `x: int = 5`
                let value = self.parse_expr()?;
                self.eat(Token::Newline);
                return Ok(Stmt::Assign {
                    targets: vec![expr],
                    value,
                    type_hint: Some(type_hint_spanned),
                });
            } else {
                // Annotation-only statement: `x: int`
                // Produce an Assign with NoneLit as the value placeholder
                let none_span = self.current_span.clone();
                let value = Spanned::new(Expr::NoneLit, none_span);
                self.eat(Token::Newline);
                return Ok(Stmt::Assign {
                    targets: vec![expr],
                    value,
                    type_hint: Some(type_hint_spanned),
                });
            }
        }

        // Check for plain assignment: `expr = value`
        if self.eat(Token::Assign) {
            let value = self.parse_expr()?;
            // Support chained assignment: `a = b = c`
            let mut targets = vec![expr];
            let mut final_value = value;
            while self.eat(Token::Assign) {
                targets.push(final_value);
                final_value = self.parse_expr()?;
            }
            self.eat(Token::Newline);
            return Ok(Stmt::Assign {
                targets,
                value: final_value,
                type_hint: None,
            });
        }

        self.eat(Token::Newline);
        Ok(Stmt::Expr(expr))
    }

    fn parse_if(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `if`
        let test = self.parse_expr()?;
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;

        let mut elif_clauses = Vec::new();
        while self.peek_token() == Token::Elif {
            self.advance(); // consume `elif`
            let elif_test = self.parse_expr()?;
            self.expect(Token::Colon)?;
            let elif_body = self.parse_block()?;
            elif_clauses.push((elif_test, elif_body));
        }

        let else_body = if self.peek_token() == Token::Else {
            self.advance(); // consume `else`
            self.expect(Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Stmt::If {
            test,
            body,
            elif_clauses,
            else_body,
        })
    }

    fn parse_while(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `while`
        let test = self.parse_expr()?;
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;
        let else_body = if self.peek_token() == Token::Else {
            self.advance();
            self.expect(Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::While {
            test,
            body,
            else_body,
        })
    }

    fn parse_for(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `for`
        let target = self.parse_primary()?;
        // Handle `in` keyword — it's lexed as a Name token
        let tok = self.peek_token();
        let span = self.peek_span();
        if tok == Token::Name && self.text(&span) == "in" {
            self.advance();
        } else {
            return Err(ParseError {
                message: "expected 'in' in for loop".into(),
                span,
            });
        }
        let iter = self.parse_expr()?;
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;
        let else_body = if self.peek_token() == Token::Else {
            self.advance();
            self.expect(Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::For {
            target,
            iter,
            body,
            else_body,
        })
    }

    fn parse_func_def(&mut self, decorators: Vec<Spanned<Expr>>) -> ParseResult<Stmt> {
        self.advance(); // consume `def`
        let name = self.expect_name()?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;
        let (return_type, return_type_hint) = if self.eat(Token::Arrow) {
            let rt = self.parse_expr()?;
            let hint = Self::convert_expr_to_type_hint(&rt);
            (Some(rt), hint)
        } else {
            (None, None)
        };
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;
        Ok(Stmt::FuncDef {
            name,
            params,
            return_type,
            return_type_hint,
            body,
            decorators,
        })
    }

    pub(crate) fn parse_params(&mut self) -> ParseResult<Vec<Param>> {
        let mut params = Vec::new();
        if self.peek_token() == Token::RParen {
            return Ok(params);
        }
        loop {
            let name = self.expect_name()?;
            let (annotation, type_hint) = if self.eat(Token::Colon) {
                let ann = self.parse_expr()?;
                let hint = Self::convert_expr_to_type_hint(&ann);
                (Some(ann), hint)
            } else {
                (None, None)
            };
            let default = if self.eat(Token::Assign) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            params.push(Param {
                name,
                annotation,
                default,
                type_hint,
            });
            if !self.eat(Token::Comma) {
                break;
            }
            // Allow trailing comma before `)`
            if self.peek_token() == Token::RParen {
                break;
            }
        }
        Ok(params)
    }

    fn parse_class_def(&mut self, decorators: Vec<Spanned<Expr>>) -> ParseResult<Stmt> {
        self.advance(); // consume `class`
        let name = self.expect_name()?;
        let bases = if self.eat(Token::LParen) {
            let mut bases = Vec::new();
            if self.peek_token() != Token::RParen {
                loop {
                    bases.push(self.parse_expr()?);
                    if !self.eat(Token::Comma) {
                        break;
                    }
                    if self.peek_token() == Token::RParen {
                        break;
                    }
                }
            }
            self.expect(Token::RParen)?;
            bases
        } else {
            Vec::new()
        };
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;
        Ok(Stmt::ClassDef {
            name,
            bases,
            body,
            decorators,
        })
    }

    fn parse_try(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `try`
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;

        let mut handlers = Vec::new();
        while self.peek_token() == Token::Except {
            self.advance();
            let (exc_type, handler_name) = if self.peek_token() != Token::Colon {
                let t = Some(self.parse_expr()?);
                let n = if self.eat(Token::As) {
                    Some(self.expect_name()?)
                } else {
                    None
                };
                (t, n)
            } else {
                (None, None)
            };
            self.expect(Token::Colon)?;
            let handler_body = self.parse_block()?;
            handlers.push(ExceptHandler {
                exc_type,
                name: handler_name,
                body: handler_body,
            });
        }

        let else_body = if self.peek_token() == Token::Else {
            self.advance();
            self.expect(Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        let finally_body = if self.peek_token() == Token::Finally {
            self.advance();
            self.expect(Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Stmt::Try {
            body,
            handlers,
            else_body,
            finally_body,
        })
    }

    fn parse_with(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume `with`
        let mut items = Vec::new();
        loop {
            let context = self.parse_expr()?;
            let optional_var = if self.eat(Token::As) {
                Some(self.parse_primary()?)
            } else {
                None
            };
            items.push((context, optional_var));
            if !self.eat(Token::Comma) {
                break;
            }
        }
        self.expect(Token::Colon)?;
        let body = self.parse_block()?;
        Ok(Stmt::With { items, body })
    }

    fn parse_decorated(&mut self) -> ParseResult<Stmt> {
        let mut decorators = Vec::new();
        while self.peek_token() == Token::At {
            self.advance(); // consume `@`
            let deco = self.parse_expr()?;
            decorators.push(deco);
            self.expect(Token::Newline)?;
            self.skip_newlines();
        }
        match self.peek_token() {
            Token::Def => self.parse_func_def(decorators),
            Token::Class => self.parse_class_def(decorators),
            _ => Err(ParseError {
                message: "expected 'def' or 'class' after decorator".into(),
                span: self.peek_span(),
            }),
        }
    }
}