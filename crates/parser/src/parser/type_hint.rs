/*
 * Copyright (c) 2026. Mikhail Kulik.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use super::*;
use ast::types::TypeHint;

impl<I: Iterator<Item = SpannedToken>> Parser<I> {
    /// Parse a type hint from the current token stream.
    /// Handles: `int`, `float`, `str`, `bool`, `None`,
    ///          `List[X]`, `Set[X]`, `Dict[K, V]`, `Tuple[A, B, ...]`,
    ///          `Optional[X]`, and custom type names.
    pub(crate) fn parse_type_hint(&mut self) -> ParseResult<Spanned<TypeHint>> {
        let start = self.peek_span();

        // Handle `None` keyword as type hint
        if self.peek_token() == Token::None_ {
            self.advance();
            let end = self.current_span.end;
            return Ok(Spanned::new(TypeHint::None, start.start..end));
        }

        let name = self.expect_name()?;
        let name_span_end = self.current_span.end;

        // Check for generic type with brackets: `List[int]`, `Dict[str, int]`, etc.
        if self.peek_token() == Token::LBracket {
            self.advance(); // consume `[`
            let hint = match name.as_str() {
                "List" | "list" => {
                    let inner = self.parse_type_hint()?;
                    self.expect(Token::RBracket)?;
                    TypeHint::List(Box::new(inner.node))
                }
                "Set" | "set" => {
                    let inner = self.parse_type_hint()?;
                    self.expect(Token::RBracket)?;
                    TypeHint::Set(Box::new(inner.node))
                }
                "Dict" | "dict" => {
                    let key = self.parse_type_hint()?;
                    self.expect(Token::Comma)?;
                    let value = self.parse_type_hint()?;
                    self.expect(Token::RBracket)?;
                    TypeHint::Dict(Box::new(key.node), Box::new(value.node))
                }
                "Tuple" | "tuple" => {
                    let mut elems = Vec::new();
                    if self.peek_token() != Token::RBracket {
                        elems.push(self.parse_type_hint()?.node);
                        while self.eat(Token::Comma) {
                            if self.peek_token() == Token::RBracket {
                                break;
                            }
                            elems.push(self.parse_type_hint()?.node);
                        }
                    }
                    self.expect(Token::RBracket)?;
                    TypeHint::Tuple(elems)
                }
                "Optional" => {
                    let inner = self.parse_type_hint()?;
                    self.expect(Token::RBracket)?;
                    TypeHint::Optional(Box::new(inner.node))
                }
                other => {
                    // Unknown generic — treat as Custom for now, skip bracket contents
                    // This is a simplification; real Python has more complex generic support.
                    let mut depth = 1;
                    while depth > 0 {
                        match self.peek_token() {
                            Token::LBracket => { self.advance(); depth += 1; }
                            Token::RBracket => { self.advance(); depth -= 1; }
                            Token::Eof => {
                                return Err(ParseError {
                                    message: "unexpected EOF in type hint".into(),
                                    span: self.peek_span(),
                                });
                            }
                            _ => { self.advance(); }
                        }
                    }
                    TypeHint::Custom(other.to_string())
                }
            };
            let end = self.current_span.end;
            return Ok(Spanned::new(hint, start.start..end));
        }

        // Simple (non-generic) type names
        let hint = match name.as_str() {
            "int" => TypeHint::Int,
            "float" => TypeHint::Float,
            "str" => TypeHint::Str,
            "bool" => TypeHint::Bool,
            other => TypeHint::Custom(other.to_string()),
        };
        Ok(Spanned::new(hint, start.start..name_span_end))
    }

    /// Convert an already-parsed annotation `Expr` into a `TypeHint`.
    /// Returns `None` if the expression cannot be recognized as a known type hint.
    pub(crate) fn convert_expr_to_type_hint(expr: &Spanned<Expr>) -> Option<Spanned<TypeHint>> {
        let hint = Self::convert_expr_node_to_type_hint(&expr.node)?;
        Some(Spanned::new(hint, expr.span.clone()))
    }

    fn convert_expr_node_to_type_hint(expr: &Expr) -> Option<TypeHint> {
        match expr {
            Expr::Name(name) => {
                Some(match name.as_str() {
                    "int" => TypeHint::Int,
                    "float" => TypeHint::Float,
                    "str" => TypeHint::Str,
                    "bool" => TypeHint::Bool,
                    "None" => TypeHint::None,
                    other => TypeHint::Custom(other.to_string()),
                })
            }
            Expr::NoneLit => Some(TypeHint::None),
            Expr::Subscript { value, index } => {
                if let Expr::Name(container) = &value.node {
                    match container.as_str() {
                        "List" | "list" => {
                            let inner = Self::convert_expr_node_to_type_hint(&index.node)?;
                            Some(TypeHint::List(Box::new(inner)))
                        }
                        "Set" | "set" => {
                            let inner = Self::convert_expr_node_to_type_hint(&index.node)?;
                            Some(TypeHint::Set(Box::new(inner)))
                        }
                        "Optional" => {
                            let inner = Self::convert_expr_node_to_type_hint(&index.node)?;
                            Some(TypeHint::Optional(Box::new(inner)))
                        }
                        "Dict" | "dict" => {
                            // Dict[K, V] parses as Subscript { value: Dict, index: Tuple(K, V) }
                            if let Expr::Tuple(elems) = &index.node {
                                if elems.len() == 2 {
                                    let key = Self::convert_expr_node_to_type_hint(&elems[0].node)?;
                                    let val = Self::convert_expr_node_to_type_hint(&elems[1].node)?;
                                    Some(TypeHint::Dict(Box::new(key), Box::new(val)))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        "Tuple" | "tuple" => {
                            if let Expr::Tuple(elems) = &index.node {
                                let hints: Option<Vec<TypeHint>> = elems.iter()
                                    .map(|e| Self::convert_expr_node_to_type_hint(&e.node))
                                    .collect();
                                Some(TypeHint::Tuple(hints?))
                            } else {
                                // Single-element tuple: Tuple[int]
                                let inner = Self::convert_expr_node_to_type_hint(&index.node)?;
                                Some(TypeHint::Tuple(vec![inner]))
                            }
                        }
                        _ => Some(TypeHint::Custom(container.clone())),
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

