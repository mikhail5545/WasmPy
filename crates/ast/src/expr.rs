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

use crate::{Param, Spanned};
use crate::op::*;

#[derive(Debug, Clone)]
pub enum Expr {
    /// An identifier/name reference: `foo`
    Name(String),

    /// Integer or float literal: `42`, `3.14`
    Number(String),

    /// String literal: `"hello"`
    StringLit(String),

    /// Boolean literal
    Bool(bool),

    /// `None`
    NoneLit,

    /// Unary operation: `-x`, `not x`, `~x`
    UnaryOp {
        op: UnaryOp,
        operand: Box<Spanned<Expr>>,
    },

    /// Binary operation: `x + y`, `x and y`, etc.
    BinOp {
        left: Box<Spanned<Expr>>,
        op: BinOp,
        right: Box<Spanned<Expr>>,
    },

    /// Comparison: `x < y`, `x == y`, `x is y`, `x in y`, etc.
    /// Supports chained comparisons: `1 < x < 10`
    Compare {
        left: Box<Spanned<Expr>>,
        ops: Vec<CmpOp>,
        comparators: Vec<Spanned<Expr>>,
    },

    /// `a if cond else b`
    IfExpr {
        test: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
        orelse: Box<Spanned<Expr>>,
    },

    /// `lambda params: body`
    Lambda {
        params: Vec<Param>,
        body: Box<Spanned<Expr>>,
    },

    /// Function call: `f(a, b, key=val)`
    Call {
        func: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
        kwargs: Vec<(String, Spanned<Expr>)>,
    },

    /// Attribute access: `obj.attr`
    Attribute {
        value: Box<Spanned<Expr>>,
        attr: String,
    },

    /// Subscript: `obj[index]`
    Subscript {
        value: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },

    /// Tuple literal: `(a, b, c)` or `a, b, c`
    Tuple(Vec<Spanned<Expr>>),

    /// List literal: `[a, b, c]`
    List(Vec<Spanned<Expr>>),

    /// Dict literal: `{k: v, ...}`
    Dict {
        keys: Vec<Spanned<Expr>>,
        values: Vec<Spanned<Expr>>,
    },

    /// Set literal: `{a, b, c}`
    Set(Vec<Spanned<Expr>>),

    /// `*expr` (star expression, e.g. in assignments)
    Starred(Box<Spanned<Expr>>),

    /// `await expr`
    Await(Box<Spanned<Expr>>),

    /// `yield expr` / `yield from expr`
    Yield(Option<Box<Spanned<Expr>>>),

    /// `yield from expr`
    YieldFrom(Box<Spanned<Expr>>),

    /// Walrus operator: `name := expr`
    NamedExpr {
        target: Box<Spanned<Expr>>,
        value: Box<Spanned<Expr>>,
    },
}