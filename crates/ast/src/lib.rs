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

pub mod stmt;
pub mod expr;
pub mod op;
pub mod types;

use logos::Span;
use stmt::Stmt;
use expr::Expr;
use crate::types::TypeHint;

/// Every AST node carries a source span for error reporting.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// Top-level module representation, containing a list of statements.
#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub annotation: Option<Spanned<Expr>>,
    pub default: Option<Spanned<Expr>>,
    pub type_hint: Option<Spanned<TypeHint>>,
}

#[derive(Debug, Clone)]
pub struct ExceptHandler {
    pub exc_type: Option<Spanned<Expr>>,
    pub name: Option<String>,
    pub body: Vec<Spanned<Stmt>>,
}
