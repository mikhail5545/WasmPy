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

//! Static type checker for the WASM compilation backend.
//!
//! This module walks the AST and enforces:
//! - All variables must have type hints on first assignment
//! - No dynamic type changes (reassigning to a different type is an error)
//! - All function parameters and return types must be annotated
//! - Expression types are inferred and checked for compatibility

use ast::expr::Expr;
use ast::op::*;
use ast::stmt::Stmt;
use ast::types::TypeHint;
use ast::{Module, Param, Spanned};
use logos::Span;
use std::collections::HashMap;
use std::fmt;

use crate::types::WasmType;

// ─── Error types ─────────────────────────────────────────────────────────────

/// A type error with a source span for precise error reporting.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl TypeError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Type error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

impl std::error::Error for TypeError {}

// ─── Function signature ──────────────────────────────────────────────────────

/// The resolved signature of a function (after type-checking the definition).
#[derive(Debug, Clone)]
pub struct FuncSig {
    pub param_types: Vec<WasmType>,
    pub return_type: WasmType,
}

// ─── Type environment (scoped symbol table) ──────────────────────────────────

/// A scoped symbol table that tracks variable types and function signatures.
/// Supports nesting: a function body creates a child `TypeEnv` with its own locals.
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Variable name → resolved WASM type
    variables: HashMap<String, WasmType>,
    /// Function name → signature
    functions: HashMap<String, FuncSig>,
    /// Parent scope (for nested lookups)
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: None,
        }
    }

    pub fn child(&self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    pub fn declare_var(&mut self, name: String, ty: WasmType) {
        self.variables.insert(name, ty);
    }

    pub fn lookup_var(&self, name: &str) -> Option<WasmType> {
        if let Some(ty) = self.variables.get(name) {
            Some(*ty)
        } else if let Some(parent) = &self.parent {
            parent.lookup_var(name)
        } else {
            None
        }
    }

    pub fn declare_func(&mut self, name: String, sig: FuncSig) {
        self.functions.insert(name, sig);
    }

    pub fn lookup_func(&self, name: &str) -> Option<&FuncSig> {
        if let Some(sig) = self.functions.get(name) {
            Some(sig)
        } else if let Some(parent) = &self.parent {
            parent.lookup_func(name)
        } else {
            None
        }
    }
}

// ─── Type checker ────────────────────────────────────────────────────────────

/// The static type checker. Walks the AST, enforces type annotations, infers
/// expression types, and rejects programs with missing hints or type mismatches.
pub struct TypeChecker {
    env: TypeEnv,
    errors: Vec<TypeError>,
    /// The expected return type of the current function (None when at module level).
    current_return_type: Option<WasmType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            errors: Vec::new(),
            current_return_type: None,
        }
    }

    /// Main entry point: type-check an entire module.
    /// Returns `Ok(TypeEnv)` with the resolved types if successful,
    /// or `Err(Vec<TypeError>)` with all collected errors.
    pub fn check_module(mut self, module: &Module) -> Result<TypeEnv, Vec<TypeError>> {
        // First pass: register all function signatures so forward references work
        for stmt in &module.body {
            if let Stmt::FuncDef {
                name,
                params,
                return_type_hint,
                ..
            } = &stmt.node
            {
                self.register_func_sig(name, params, return_type_hint, stmt.span.clone());
            }
        }

        // Second pass: type-check all statements
        for stmt in &module.body {
            self.check_stmt(stmt);
        }

        if self.errors.is_empty() {
            Ok(self.env)
        } else {
            Err(self.errors)
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError::new(message, span));
    }

    // ─── Function signature registration ─────────────────────────────────

    fn register_func_sig(
        &mut self,
        name: &str,
        params: &[Param],
        return_type_hint: &Option<Spanned<TypeHint>>,
        span: Span,
    ) {
        let mut param_types = Vec::new();
        let mut has_errors = false;

        for param in params {
            match &param.type_hint {
                Some(hint) => match WasmType::from_type_hint(&hint.node) {
                    Some(wt) => param_types.push(wt),
                    None => {
                        self.error(
                            format!(
                                "unsupported type '{}' for parameter '{}' in static compilation",
                                crate::types::type_hint_display(&hint.node),
                                param.name
                            ),
                            hint.span.clone(),
                        );
                        has_errors = true;
                    }
                },
                None => {
                    self.error(
                        format!(
                            "parameter '{}' of function '{}' must have a type annotation for static compilation",
                            param.name, name
                        ),
                        span.clone(),
                    );
                    has_errors = true;
                }
            }
        }

        let return_type = match return_type_hint {
            Some(hint) => match WasmType::from_type_hint(&hint.node) {
                Some(wt) => wt,
                None => {
                    self.error(
                        format!(
                            "unsupported return type '{}' for function '{}' in static compilation",
                            crate::types::type_hint_display(&hint.node),
                            name
                        ),
                        hint.span.clone(),
                    );
                    return;
                }
            },
            None => {
                self.error(
                    format!(
                        "function '{}' must have a return type annotation for static compilation",
                        name
                    ),
                    span,
                );
                return;
            }
        };

        if !has_errors {
            self.env.declare_func(
                name.to_string(),
                FuncSig {
                    param_types,
                    return_type,
                },
            );
        }
    }

    // ─── Statement checking ──────────────────────────────────────────────

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::Expr(expr) => {
                let _ = self.infer_expr(expr);
            }

            Stmt::Assign {
                targets,
                value,
                type_hint,
            } => {
                self.check_assign(targets, value, type_hint, stmt.span.clone());
            }

            Stmt::AugAssign {
                target, op, value, ..
            } => {
                self.check_aug_assign(target, op, value, stmt.span.clone());
            }

            Stmt::Return(maybe_expr) => {
                self.check_return(maybe_expr, stmt.span.clone());
            }

            Stmt::If {
                test,
                body,
                elif_clauses,
                else_body,
            } => {
                self.check_if(test, body, elif_clauses, else_body);
            }

            Stmt::While {
                test,
                body,
                else_body,
            } => {
                self.check_while(test, body, else_body);
            }

            Stmt::FuncDef {
                name,
                params,
                return_type_hint,
                body,
                ..
            } => {
                self.check_func_def(name, params, return_type_hint, body, stmt.span.clone());
            }

            Stmt::Pass => {}

            Stmt::Break | Stmt::Continue => {
                // Control flow — no type implications
            }

            Stmt::Global(_) => {
                // No type implications at this stage
            }

            _ => {
                self.error(
                    format!("statement {:?} is not supported in static compilation", stmt.node),
                    stmt.span.clone(),
                );
            }
        }
    }

    fn check_assign(
        &mut self,
        targets: &[Spanned<Expr>],
        value: &Spanned<Expr>,
        type_hint: &Option<Spanned<TypeHint>>,
        _span: Span,
    ) {
        let inferred = match self.infer_expr(value) {
            Some(t) => t,
            None => return, // error already reported
        };

        for target in targets {
            if let Expr::Name(name) = &target.node {
                if let Some(hint) = type_hint {
                    // Annotated assignment: `x: int = expr`
                    match WasmType::from_type_hint(&hint.node) {
                        Some(declared) => {
                            if !self.types_compatible(declared, inferred) {
                                self.error(
                                    format!(
                                        "cannot assign {} value to variable '{}' declared as {}",
                                        inferred, name, declared
                                    ),
                                    value.span.clone(),
                                );
                            }
                            // Check for re-declaration with a different type
                            if let Some(existing) = self.env.lookup_var(name) {
                                if existing != declared {
                                    self.error(
                                        format!(
                                            "cannot re-declare variable '{}' as {} (previously declared as {})",
                                            name, declared, existing
                                        ),
                                        hint.span.clone(),
                                    );
                                }
                            }
                            self.env.declare_var(name.clone(), declared);
                        }
                        None => {
                            self.error(
                                format!(
                                    "unsupported type '{}' for variable '{}' in static compilation",
                                    crate::types::type_hint_display(&hint.node),
                                    name
                                ),
                                hint.span.clone(),
                            );
                        }
                    }
                } else {
                    // No type hint: check if variable was previously declared
                    if let Some(existing) = self.env.lookup_var(name) {
                        // Re-assignment: inferred type must match
                        if !self.types_compatible(existing, inferred) {
                            self.error(
                                format!(
                                    "cannot assign {} value to variable '{}' (declared as {}): dynamic type change not allowed",
                                    inferred, name, existing
                                ),
                                value.span.clone(),
                            );
                        }
                    } else {
                        // First assignment without a type hint — error
                        self.error(
                            format!(
                                "variable '{}' must have a type annotation on first assignment for static compilation",
                                name
                            ),
                            target.span.clone(),
                        );
                    }
                }
            } else {
                self.error(
                    "only simple name targets are supported in static compilation",
                    target.span.clone(),
                );
            }
        }
    }

    fn check_aug_assign(
        &mut self,
        target: &Spanned<Expr>,
        _op: &AugOp,
        value: &Spanned<Expr>,
        span: Span,
    ) {
        let val_type = match self.infer_expr(value) {
            Some(t) => t,
            None => return,
        };

        if let Expr::Name(name) = &target.node {
            match self.env.lookup_var(name) {
                Some(existing) => {
                    if !self.types_compatible(existing, val_type) {
                        self.error(
                            format!(
                                "augmented assignment would change type of '{}' from {} to {}",
                                name, existing, val_type
                            ),
                            span,
                        );
                    }
                }
                None => {
                    self.error(
                        format!("variable '{}' used before declaration", name),
                        target.span.clone(),
                    );
                }
            }
        } else {
            self.error(
                "only simple name targets are supported in static compilation",
                target.span.clone(),
            );
        }
    }

    fn check_return(&mut self, maybe_expr: &Option<Spanned<Expr>>, span: Span) {
        let ret_type = match maybe_expr {
            Some(expr) => match self.infer_expr(expr) {
                Some(t) => t,
                None => return,
            },
            None => WasmType::Void,
        };

        if let Some(expected) = self.current_return_type {
            if !self.types_compatible(expected, ret_type) {
                self.error(
                    format!(
                        "return type {} does not match declared return type {}",
                        ret_type, expected
                    ),
                    span,
                );
            }
        }
        // If no current_return_type is set, we're at module level — return is still valid but not checked
    }

    fn check_if(
        &mut self,
        test: &Spanned<Expr>,
        body: &[Spanned<Stmt>],
        elif_clauses: &[(Spanned<Expr>, Vec<Spanned<Stmt>>)],
        else_body: &Option<Vec<Spanned<Stmt>>>,
    ) {
        if let Some(test_ty) = self.infer_expr(test) {
            if test_ty != WasmType::I32 {
                self.error(
                    format!("if condition must be bool, got {}", test_ty),
                    test.span.clone(),
                );
            }
        }

        for stmt in body {
            self.check_stmt(stmt);
        }

        for (elif_test, elif_body) in elif_clauses {
            if let Some(test_ty) = self.infer_expr(elif_test) {
                if test_ty != WasmType::I32 {
                    self.error(
                        format!("elif condition must be bool, got {}", test_ty),
                        elif_test.span.clone(),
                    );
                }
            }
            for stmt in elif_body {
                self.check_stmt(stmt);
            }
        }

        if let Some(else_body) = else_body {
            for stmt in else_body {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_while(
        &mut self,
        test: &Spanned<Expr>,
        body: &[Spanned<Stmt>],
        else_body: &Option<Vec<Spanned<Stmt>>>,
    ) {
        if let Some(test_ty) = self.infer_expr(test) {
            if test_ty != WasmType::I32 {
                self.error(
                    format!("while condition must be bool, got {}", test_ty),
                    test.span.clone(),
                );
            }
        }

        for stmt in body {
            self.check_stmt(stmt);
        }

        if let Some(else_body) = else_body {
            for stmt in else_body {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_func_def(
        &mut self,
        name: &str,
        params: &[Param],
        _return_type_hint: &Option<Spanned<TypeHint>>,
        body: &[Spanned<Stmt>],
        _span: Span,
    ) {
        // Look up the function signature (registered in first pass)
        let sig = match self.env.lookup_func(name) {
            Some(sig) => sig.clone(),
            None => return, // signature registration failed, errors already reported
        };

        // Save parent state
        let parent_env = self.env.clone();
        let parent_return_type = self.current_return_type.take();

        // Create child scope
        self.env = self.env.child();
        self.current_return_type = Some(sig.return_type);

        // Register parameters as local variables
        for (param, &wasm_ty) in params.iter().zip(sig.param_types.iter()) {
            self.env.declare_var(param.name.clone(), wasm_ty);
        }

        // Check body
        for stmt in body {
            self.check_stmt(stmt);
        }

        // Restore parent state
        self.env = parent_env;
        self.current_return_type = parent_return_type;

        // Declare the function name as a variable in the enclosing scope
        // (so it can be referenced as a callable)
    }

    // ─── Expression type inference ───────────────────────────────────────

    /// Infer the WasmType of an expression. Returns `None` if the type cannot be determined
    /// (errors are recorded internally).
    fn infer_expr(&mut self, expr: &Spanned<Expr>) -> Option<WasmType> {
        match &expr.node {
            Expr::Number(text) => {
                if text.contains('.') || text.contains('e') || text.contains('E') {
                    Some(WasmType::F64)
                } else {
                    Some(WasmType::I64)
                }
            }

            Expr::Bool(_) => Some(WasmType::I32),

            Expr::NoneLit => Some(WasmType::Void),

            Expr::StringLit(_) => {
                self.error(
                    "string literals are not supported in static compilation (no WASM string type)",
                    expr.span.clone(),
                );
                None
            }

            Expr::Name(name) => {
                if let Some(ty) = self.env.lookup_var(name) {
                    Some(ty)
                } else {
                    self.error(
                        format!("undefined variable '{}'", name),
                        expr.span.clone(),
                    );
                    None
                }
            }

            Expr::BinOp { left, op, right } => {
                self.infer_binop(left, op, right, expr.span.clone())
            }

            Expr::UnaryOp { op, operand } => {
                self.infer_unaryop(op, operand, expr.span.clone())
            }

            Expr::Compare {
                left,
                ops,
                comparators,
            } => self.infer_compare(left, ops, comparators, expr.span.clone()),

            Expr::IfExpr { test, body, orelse } => {
                self.infer_if_expr(test, body, orelse, expr.span.clone())
            }

            Expr::Call { func, args, .. } => {
                self.infer_call(func, args, expr.span.clone())
            }

            _ => {
                self.error(
                    format!("expression {:?} is not supported in static compilation", expr.node),
                    expr.span.clone(),
                );
                None
            }
        }
    }

    fn infer_binop(
        &mut self,
        left: &Spanned<Expr>,
        op: &BinOp,
        right: &Spanned<Expr>,
        span: Span,
    ) -> Option<WasmType> {
        let lt = self.infer_expr(left)?;
        let rt = self.infer_expr(right)?;

        match op {
            // Logical operators: require bool, produce bool
            BinOp::And | BinOp::Or => {
                if lt != WasmType::I32 {
                    self.error(
                        format!("left operand of '{:?}' must be bool, got {}", op, lt),
                        left.span.clone(),
                    );
                    return None;
                }
                if rt != WasmType::I32 {
                    self.error(
                        format!("right operand of '{:?}' must be bool, got {}", op, rt),
                        right.span.clone(),
                    );
                    return None;
                }
                Some(WasmType::I32)
            }

            // Bitwise operators: require integer types
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::LShift | BinOp::RShift => {
                if !lt.is_integer() {
                    self.error(
                        format!("bitwise operation requires integer type, got {}", lt),
                        left.span.clone(),
                    );
                    return None;
                }
                if !rt.is_integer() {
                    self.error(
                        format!("bitwise operation requires integer type, got {}", rt),
                        right.span.clone(),
                    );
                    return None;
                }
                Some(WasmType::I64)
            }

            // Arithmetic operators: numeric promotion
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod | BinOp::Pow | BinOp::FloorDiv => {
                match lt.numeric_promote(rt) {
                    Some(result) => Some(result),
                    None => {
                        self.error(
                            format!(
                                "cannot apply {:?} to {} and {}",
                                op, lt, rt
                            ),
                            span,
                        );
                        None
                    }
                }
            }

            // Division always produces float
            BinOp::Div => {
                if !lt.is_numeric() && lt != WasmType::I32 {
                    self.error(
                        format!("division requires numeric type, got {}", lt),
                        left.span.clone(),
                    );
                    return None;
                }
                if !rt.is_numeric() && rt != WasmType::I32 {
                    self.error(
                        format!("division requires numeric type, got {}", rt),
                        right.span.clone(),
                    );
                    return None;
                }
                Some(WasmType::F64)
            }

            BinOp::MatMul => {
                self.error(
                    "matrix multiplication is not supported in static compilation",
                    span,
                );
                None
            }
        }
    }

    fn infer_unaryop(
        &mut self,
        op: &UnaryOp,
        operand: &Spanned<Expr>,
        span: Span,
    ) -> Option<WasmType> {
        let ot = self.infer_expr(operand)?;

        match op {
            UnaryOp::Neg | UnaryOp::Pos => {
                if ot.is_numeric() || ot == WasmType::I32 {
                    // -bool or +bool promotes to int
                    if ot == WasmType::I32 {
                        Some(WasmType::I64)
                    } else {
                        Some(ot)
                    }
                } else {
                    self.error(
                        format!("unary {:?} requires numeric type, got {}", op, ot),
                        span,
                    );
                    None
                }
            }
            UnaryOp::Not => {
                if ot != WasmType::I32 {
                    self.error(
                        format!("'not' requires bool, got {}", ot),
                        span,
                    );
                    return None;
                }
                Some(WasmType::I32)
            }
            UnaryOp::Invert => {
                if !ot.is_integer() {
                    self.error(
                        format!("bitwise invert requires integer type, got {}", ot),
                        span,
                    );
                    return None;
                }
                Some(WasmType::I64)
            }
        }
    }

    fn infer_compare(
        &mut self,
        left: &Spanned<Expr>,
        ops: &[CmpOp],
        comparators: &[Spanned<Expr>],
        span: Span,
    ) -> Option<WasmType> {
        let mut lt = self.infer_expr(left)?;

        for (op, comp) in ops.iter().zip(comparators.iter()) {
            let rt = self.infer_expr(comp)?;

            match op {
                CmpOp::Eq | CmpOp::NotEq => {
                    // Allow comparing same types or numeric types
                    if lt != rt && lt.numeric_promote(rt).is_none() {
                        self.error(
                            format!("cannot compare {} and {}", lt, rt),
                            span.clone(),
                        );
                        return None;
                    }
                }
                CmpOp::Lt | CmpOp::LtE | CmpOp::Gt | CmpOp::GtE => {
                    if !lt.is_numeric() && !lt.is_integer() {
                        self.error(
                            format!("ordering comparison requires numeric type, got {}", lt),
                            span.clone(),
                        );
                        return None;
                    }
                    if !rt.is_numeric() && !rt.is_integer() {
                        self.error(
                            format!("ordering comparison requires numeric type, got {}", rt),
                            span.clone(),
                        );
                        return None;
                    }
                }
                CmpOp::Is | CmpOp::IsNot => {
                    // Identity comparison — allowed for any types
                }
                CmpOp::In | CmpOp::NotIn => {
                    self.error(
                        "'in' / 'not in' are not supported in static compilation",
                        span.clone(),
                    );
                    return None;
                }
            }

            lt = rt;
        }

        Some(WasmType::I32) // comparisons always produce bool
    }

    fn infer_if_expr(
        &mut self,
        test: &Spanned<Expr>,
        body: &Spanned<Expr>,
        orelse: &Spanned<Expr>,
        span: Span,
    ) -> Option<WasmType> {
        if let Some(test_ty) = self.infer_expr(test) {
            if test_ty != WasmType::I32 {
                self.error(
                    format!("if expression condition must be bool, got {}", test_ty),
                    test.span.clone(),
                );
            }
        }

        let body_ty = self.infer_expr(body)?;
        let else_ty = self.infer_expr(orelse)?;

        if body_ty != else_ty {
            self.error(
                format!(
                    "if expression branches have different types: {} and {}",
                    body_ty, else_ty
                ),
                span,
            );
            None
        } else {
            Some(body_ty)
        }
    }

    fn infer_call(
        &mut self,
        func: &Spanned<Expr>,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Option<WasmType> {
        let func_name = match &func.node {
            Expr::Name(name) => name.clone(),
            _ => {
                self.error(
                    "only direct function calls are supported in static compilation",
                    func.span.clone(),
                );
                return None;
            }
        };

        let sig = match self.env.lookup_func(&func_name) {
            Some(sig) => sig.clone(),
            None => {
                self.error(
                    format!("undefined function '{}'", func_name),
                    func.span.clone(),
                );
                return None;
            }
        };

        // Check argument count
        if args.len() != sig.param_types.len() {
            self.error(
                format!(
                    "function '{}' expects {} arguments, got {}",
                    func_name,
                    sig.param_types.len(),
                    args.len()
                ),
                span.clone(),
            );
            return None;
        }

        // Check argument types
        for (i, (arg, expected)) in args.iter().zip(sig.param_types.iter()).enumerate() {
            if let Some(arg_ty) = self.infer_expr(arg) {
                if !self.types_compatible(*expected, arg_ty) {
                    self.error(
                        format!(
                            "argument {} of '{}': expected {}, got {}",
                            i + 1,
                            func_name,
                            expected,
                            arg_ty
                        ),
                        arg.span.clone(),
                    );
                }
            }
        }

        Some(sig.return_type)
    }

    // ─── Helpers ─────────────────────────────────────────────────────────

    /// Checks if `actual` is compatible with `expected`.
    /// bool (I32) is compatible with int (I64) for promotion, but not vice-versa.
    fn types_compatible(&self, expected: WasmType, actual: WasmType) -> bool {
        if expected == actual {
            return true;
        }
        // Allow bool → int promotion
        if expected == WasmType::I64 && actual == WasmType::I32 {
            return true;
        }
        // Allow int → float promotion
        if expected == WasmType::F64 && (actual == WasmType::I64 || actual == WasmType::I32) {
            return true;
        }
        false
    }
}

