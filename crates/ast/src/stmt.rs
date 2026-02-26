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

use crate::{ExceptHandler, Expr, Param, Spanned};
use crate::op::AugOp;
use crate::types::TypeHint;

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Expression used as a statement (e.g. function call, string literal)
    Expr(Spanned<Expr>),

    /// `x = expr` or `x: type = expr`
    Assign {
        targets: Vec<Spanned<Expr>>,
        value: Spanned<Expr>,
        type_hint: Option<Spanned<TypeHint>>,
    },

    /// Augmented assignment: `x += 1`, `x -= 1`, etc.
    AugAssign {
        target: Spanned<Expr>,
        op: AugOp,
        value: Spanned<Expr>,
        type_hint: Option<Spanned<TypeHint>>,
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
        return_type_hint: Option<Spanned<TypeHint>>,
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