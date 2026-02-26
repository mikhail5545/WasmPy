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
use wasmparser::Validator;

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

/// Compile a module and validate the resulting WASM bytes.
fn compile_and_validate(m: &Module) -> Vec<u8> {
    let bytes = crate::compile(m).expect("compilation should succeed");
    let mut validator = Validator::new();
    validator
        .validate_all(&bytes)
        .expect("WASM validation should succeed");
    bytes
}

// ─── Compiler output tests ────────────────────────────────────────────────────

#[test]
fn test_compile_simple_add_function() {
    // def add(a: int, b: int) -> int: return a + b
    let m = module(vec![func_def(
        "add",
        vec![("a", TypeHint::Int), ("b", TypeHint::Int)],
        TypeHint::Int,
        vec![return_stmt(binop(name("a"), BinOp::Add, name("b")))],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
    // WASM magic number
    assert_eq!(&bytes[0..4], b"\0asm");
}

#[test]
fn test_compile_float_function() {
    // def mul(a: float, b: float) -> float: return a * b
    let m = module(vec![func_def(
        "mul",
        vec![("a", TypeHint::Float), ("b", TypeHint::Float)],
        TypeHint::Float,
        vec![return_stmt(binop(name("a"), BinOp::Mul, name("b")))],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_bool_function() {
    // def negate(x: bool) -> bool: return not x
    let m = module(vec![func_def(
        "negate",
        vec![("x", TypeHint::Bool)],
        TypeHint::Bool,
        vec![return_stmt(sp(Expr::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(name("x")),
        }))],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_void_function() {
    // def noop() -> None: pass
    let m = module(vec![func_def(
        "noop",
        vec![],
        TypeHint::None,
        vec![sp(Stmt::Pass)],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_function_with_local_var() {
    // def square(x: int) -> int:
    //     result: int = x * x
    //     return result
    let m = module(vec![func_def(
        "square",
        vec![("x", TypeHint::Int)],
        TypeHint::Int,
        vec![
            assign(
                "result",
                binop(name("x"), BinOp::Mul, name("x")),
                Some(TypeHint::Int),
            ),
            return_stmt(name("result")),
        ],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_multiple_functions() {
    let m = module(vec![
        func_def(
            "add",
            vec![("a", TypeHint::Int), ("b", TypeHint::Int)],
            TypeHint::Int,
            vec![return_stmt(binop(name("a"), BinOp::Add, name("b")))],
        ),
        func_def(
            "sub",
            vec![("a", TypeHint::Int), ("b", TypeHint::Int)],
            TypeHint::Int,
            vec![return_stmt(binop(name("a"), BinOp::Sub, name("b")))],
        ),
    ]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_comparison() {
    // def is_positive(x: int) -> bool: return x > 0
    let m = module(vec![func_def(
        "is_positive",
        vec![("x", TypeHint::Int)],
        TypeHint::Bool,
        vec![return_stmt(sp(Expr::Compare {
            left: Box::new(name("x")),
            ops: vec![CmpOp::Gt],
            comparators: vec![int_lit("0")],
        }))],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_if_statement() {
    // def abs(x: int) -> int:
    //     result: int = x
    //     if x < 0:
    //         result = -x
    //     return result
    let m = module(vec![func_def(
        "abs_val",
        vec![("x", TypeHint::Int)],
        TypeHint::Int,
        vec![
            assign("result", name("x"), Some(TypeHint::Int)),
            sp(Stmt::If {
                test: sp(Expr::Compare {
                    left: Box::new(name("x")),
                    ops: vec![CmpOp::Lt],
                    comparators: vec![int_lit("0")],
                }),
                body: vec![assign(
                    "result",
                    sp(Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        operand: Box::new(name("x")),
                    }),
                    None,
                )],
                elif_clauses: vec![],
                else_body: None,
            }),
            return_stmt(name("result")),
        ],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_while_loop() {
    // def count_down(n: int) -> int:
    //     total: int = 0
    //     i: int = n
    //     while i > 0:
    //         total = total + i
    //         i = i - 1
    //     return total
    let m = module(vec![func_def(
        "count_down",
        vec![("n", TypeHint::Int)],
        TypeHint::Int,
        vec![
            assign("total", int_lit("0"), Some(TypeHint::Int)),
            assign("i", name("n"), Some(TypeHint::Int)),
            sp(Stmt::While {
                test: sp(Expr::Compare {
                    left: Box::new(name("i")),
                    ops: vec![CmpOp::Gt],
                    comparators: vec![int_lit("0")],
                }),
                body: vec![
                    assign("total", binop(name("total"), BinOp::Add, name("i")), None),
                    assign("i", binop(name("i"), BinOp::Sub, int_lit("1")), None),
                ],
                else_body: None,
            }),
            return_stmt(name("total")),
        ],
    )]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_function_call() {
    // def double(x: int) -> int: return x + x
    // def quad(x: int) -> int: return double(double(x))
    let m = module(vec![
        func_def(
            "double",
            vec![("x", TypeHint::Int)],
            TypeHint::Int,
            vec![return_stmt(binop(name("x"), BinOp::Add, name("x")))],
        ),
        func_def(
            "quad",
            vec![("x", TypeHint::Int)],
            TypeHint::Int,
            vec![return_stmt(sp(Expr::Call {
                func: Box::new(name("double")),
                args: vec![sp(Expr::Call {
                    func: Box::new(name("double")),
                    args: vec![name("x")],
                    kwargs: vec![],
                })],
                kwargs: vec![],
            }))],
        ),
    ]);
    let bytes = compile_and_validate(&m);
    assert!(!bytes.is_empty());
}

#[test]
fn test_wasm_magic_and_version() {
    let m = module(vec![func_def(
        "id",
        vec![("x", TypeHint::Int)],
        TypeHint::Int,
        vec![return_stmt(name("x"))],
    )]);
    let bytes = compile_and_validate(&m);
    // WASM magic: \0asm
    assert_eq!(&bytes[0..4], b"\0asm");
    // WASM version 1
    assert_eq!(&bytes[4..8], &[1, 0, 0, 0]);
}

