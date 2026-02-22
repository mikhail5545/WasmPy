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

use logos::Span;

// --- Span-wrapped node ---

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

// --- Top-level module ---

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Spanned<Stmt>>,
}

// --- Statements ---

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Expression used as a statement (e.g. function call, string literal)
    Expr(Spanned<Expr>),

    /// `x = expr` or `x: type = expr`
    Assign {
        targets: Vec<Spanned<Expr>>,
        value: Spanned<Expr>,
    },

    /// Augmented assignment: `x += 1`, `x -= 1`, etc.
    AugAssign {
        target: Spanned<Expr>,
        op: AugOp,
        value: Spanned<Expr>,
    },

    /// `return` / `return expr`
    Return(Option<Spanned<Expr>>),

    /// `pass`
    Pass,

    /// `break`
    Break,

    /// `continue`
    Continue,

    /// `del target`
    Del(Spanned<Expr>),

    /// `assert expr [, msg]`
    Assert {
        test: Spanned<Expr>,
        msg: Option<Spanned<Expr>>,
    },

    /// `raise [expr [from expr]]`
    Raise {
        exc: Option<Spanned<Expr>>,
        cause: Option<Spanned<Expr>>,
    },

    /// `global x, y`
    Global(Vec<String>),

    /// `nonlocal x, y`
    Nonlocal(Vec<String>),

    /// `import module [as alias]`
    Import {
        module: Vec<String>,
        alias: Option<String>,
    },

    /// `from module import name [as alias], ...`
    FromImport {
        module: Vec<String>,
        names: Vec<(String, Option<String>)>,
    },

    /// ```python
    /// if test:
    ///     body
    /// elif test2:
    ///     body2
    /// else:
    ///     orelse
    /// ```
    If {
        test: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>,
        elif_clauses: Vec<(Spanned<Expr>, Vec<Spanned<Stmt>>)>,
        else_body: Option<Vec<Spanned<Stmt>>>,
    },

    /// `while test: body [else: orelse]`
    While {
        test: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>,
        else_body: Option<Vec<Spanned<Stmt>>>,
    },

    /// `for target in iter: body [else: orelse]`
    For {
        target: Spanned<Expr>,
        iter: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>,
        else_body: Option<Vec<Spanned<Stmt>>>,
    },

    /// ```python
    /// def name(params) -> ret:
    ///     body
    /// ```
    FuncDef {
        name: String,
        params: Vec<Param>,
        return_type: Option<Spanned<Expr>>,
        body: Vec<Spanned<Stmt>>,
        decorators: Vec<Spanned<Expr>>,
    },

    /// ```python
    /// class Name(bases):
    ///     body
    /// ```
    ClassDef {
        name: String,
        bases: Vec<Spanned<Expr>>,
        body: Vec<Spanned<Stmt>>,
        decorators: Vec<Spanned<Expr>>,
    },

    /// `try / except / else / finally`
    Try {
        body: Vec<Spanned<Stmt>>,
        handlers: Vec<ExceptHandler>,
        else_body: Option<Vec<Spanned<Stmt>>>,
        finally_body: Option<Vec<Spanned<Stmt>>>,
    },

    /// `with expr as target: body`
    With {
        items: Vec<(Spanned<Expr>, Option<Spanned<Expr>>)>,
        body: Vec<Spanned<Stmt>>,
    },
}

// ---Expressions ---

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

// --- Operators ---

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    MatMul,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Pos,
    Not,
    Invert,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AugOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
    MatMul,
}

// --- Helpers ---

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub annotation: Option<Spanned<Expr>>,
    pub default: Option<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct ExceptHandler {
    pub exc_type: Option<Spanned<Expr>>,
    pub name: Option<String>,
    pub body: Vec<Spanned<Stmt>>,
}

