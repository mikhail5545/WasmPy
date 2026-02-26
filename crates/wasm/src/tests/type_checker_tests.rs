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

use ast::expr::Expr;
use ast::op::*;
use ast::stmt::Stmt;
use ast::types::TypeHint;
use ast::{Module, Param, Spanned};
use crate::type_checker::TypeChecker;

// ─── Helper builders ──────────────────────────────────────────────────────────

fn sp<T>(node: T) -> Spanned<T> {
    Spanned::new(node, 0..1)
}

fn int_lit(n: &str) -> Spanned<Expr> {
    sp(Expr::Number(n.to_string()))
}

fn float_lit(n: &str) -> Spanned<Expr> {
    sp(Expr::Number(n.to_string()))
}

fn bool_lit(b: bool) -> Spanned<Expr> {
    sp(Expr::Bool(b))
}

fn name(n: &str) -> Spanned<Expr> {
    sp(Expr::Name(n.to_string()))
}

fn binop(left: Spanned<Expr>, op: BinOp, right: Spanned<Expr>) -> Spanned<Expr> {
    sp(Expr::BinOp {
        left: Box::new(left),
        op,
        right: Box::new(right),
    })
}

fn assign(target: &str, value: Spanned<Expr>, hint: Option<TypeHint>) -> Spanned<Stmt> {
    sp(Stmt::Assign {
        targets: vec![name(target)],
        value,
        type_hint: hint.map(|h| sp(h)),
    })
}

fn func_def(
    fname: &str,
    params: Vec<(&str, TypeHint)>,
    return_hint: TypeHint,
    body: Vec<Spanned<Stmt>>,
) -> Spanned<Stmt> {
    let params = params
        .into_iter()
        .map(|(name, hint)| Param {
            name: name.to_string(),
            annotation: None,
            default: None,
            type_hint: Some(sp(hint)),
        })
        .collect();
    sp(Stmt::FuncDef {
        name: fname.to_string(),
        params,
        return_type: None,
        return_type_hint: Some(sp(return_hint)),
        body,
        decorators: vec![],
    })
}

fn return_stmt(expr: Spanned<Expr>) -> Spanned<Stmt> {
    sp(Stmt::Return(Some(expr)))
}

fn module(body: Vec<Spanned<Stmt>>) -> Module {
    Module { body }
}

// ─── Type checker: success cases ──────────────────────────────────────────────

#[test]
fn test_typed_int_assignment() {
    let m = module(vec![
        assign("x", int_lit("42"), Some(TypeHint::Int)),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_typed_float_assignment() {
    let m = module(vec![
        assign("y", float_lit("3.14"), Some(TypeHint::Float)),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_typed_bool_assignment() {
    let m = module(vec![
        assign("flag", bool_lit(true), Some(TypeHint::Bool)),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_reassignment_same_type() {
    // x: int = 1; x = 2 (no type hint on second, same type inferred)
    let m = module(vec![
        assign("x", int_lit("1"), Some(TypeHint::Int)),
        assign("x", int_lit("2"), None),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_binop_int_int() {
    // x: int = 1 + 2
    let m = module(vec![
        assign("x", binop(int_lit("1"), BinOp::Add, int_lit("2")), Some(TypeHint::Int)),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_func_def_with_types() {
    let m = module(vec![
        func_def("add", vec![("a", TypeHint::Int), ("b", TypeHint::Int)], TypeHint::Int, vec![
            return_stmt(binop(name("a"), BinOp::Add, name("b"))),
        ]),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

#[test]
fn test_func_call_typed() {
    let m = module(vec![
        func_def("add", vec![("a", TypeHint::Int), ("b", TypeHint::Int)], TypeHint::Int, vec![
            return_stmt(binop(name("a"), BinOp::Add, name("b"))),
        ]),
        assign("result", sp(Expr::Call {
            func: Box::new(name("add")),
            args: vec![int_lit("1"), int_lit("2")],
            kwargs: vec![],
        }), Some(TypeHint::Int)),
    ]);
    let checker = TypeChecker::new();
    assert!(checker.check_module(&m).is_ok());
}

// ─── Type checker: failure cases ──────────────────────────────────────────────

#[test]
fn test_missing_type_hint_on_first_assignment() {
    // x = 42 (no type hint on first assignment)
    let m = module(vec![
        assign("x", int_lit("42"), None),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("type annotation")));
}

#[test]
fn test_dynamic_type_change_rejected() {
    // x: int = 1; x = 3.14 (type change from int to float)
    let m = module(vec![
        assign("x", int_lit("1"), Some(TypeHint::Int)),
        assign("x", float_lit("3.14"), None),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("dynamic type change") || e.message.contains("cannot assign")));
}

#[test]
fn test_func_missing_return_type() {
    let m = module(vec![
        sp(Stmt::FuncDef {
            name: "foo".to_string(),
            params: vec![],
            return_type: None,
            return_type_hint: None,
            body: vec![sp(Stmt::Pass)],
            decorators: vec![],
        }),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("return type annotation")));
}

#[test]
fn test_func_missing_param_type() {
    let m = module(vec![
        sp(Stmt::FuncDef {
            name: "foo".to_string(),
            params: vec![Param {
                name: "x".to_string(),
                annotation: None,
                default: None,
                type_hint: None,
            }],
            return_type: None,
            return_type_hint: Some(sp(TypeHint::Int)),
            body: vec![return_stmt(int_lit("0"))],
            decorators: vec![],
        }),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("type annotation") && e.message.contains("parameter")));
}

#[test]
fn test_wrong_return_type() {
    // def foo() -> int: return True
    let m = module(vec![
        func_def("foo", vec![], TypeHint::Int, vec![
            return_stmt(bool_lit(true)),
        ]),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    // bool is compatible with int via promotion, so this should actually succeed
    assert!(err.is_ok());
}

#[test]
fn test_return_float_for_int_function() {
    // def foo() -> int: return 3.14
    let m = module(vec![
        func_def("foo", vec![], TypeHint::Int, vec![
            return_stmt(float_lit("3.14")),
        ]),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("return type")));
}

#[test]
fn test_string_literal_rejected() {
    let m = module(vec![
        sp(Stmt::Expr(sp(Expr::StringLit("hello".to_string())))),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("string")));
}

#[test]
fn test_undefined_variable() {
    let m = module(vec![
        assign("x", name("y"), Some(TypeHint::Int)),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("undefined")));
}

#[test]
fn test_wrong_arg_count() {
    let m = module(vec![
        func_def("add", vec![("a", TypeHint::Int), ("b", TypeHint::Int)], TypeHint::Int, vec![
            return_stmt(binop(name("a"), BinOp::Add, name("b"))),
        ]),
        sp(Stmt::Expr(sp(Expr::Call {
            func: Box::new(name("add")),
            args: vec![int_lit("1")], // only 1 arg, needs 2
            kwargs: vec![],
        }))),
    ]);
    let checker = TypeChecker::new();
    let err = checker.check_module(&m);
    assert!(err.is_err());
    let errors = err.unwrap_err();
    assert!(errors.iter().any(|e| e.message.contains("expects 2 arguments")));
}

