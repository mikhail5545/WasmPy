use crate::ast::{BinOp, Expr, Module, Stmt};
use crate::lexer::IndentLexer;
use crate::parser::Parser;


fn parse(source: &str) -> Module {
    let lexer = IndentLexer::new(source);
    let mut parser = Parser::new(source, lexer);
    parser.parse_module().expect("parse failed")
}

#[test]
fn test_simple_assignment() {
    let m = parse("x = 42\n");
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::Assign { targets, value } => {
            assert_eq!(targets.len(), 1);
            match &targets[0].node {
                Expr::Name(n) => assert_eq!(n, "x"),
                other => panic!("expected Name, got {:?}", other),
            }
            match &value.node {
                Expr::Number(n) => assert_eq!(n, "42"),
                other => panic!("expected Number, got {:?}", other),
            }
        }
        other => panic!("expected Assign, got {:?}", other),
    }
}

#[test]
fn test_function_def() {
    let m = parse("def foo(x, y):\n    return x + y\n");
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::FuncDef {
            name, params, body, ..
        } => {
            assert_eq!(name, "foo");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "x");
            assert_eq!(params[1].name, "y");
            assert_eq!(body.len(), 1);
            match &body[0].node {
                Stmt::Return(Some(expr)) => match &expr.node {
                    Expr::BinOp { op, .. } => assert_eq!(*op, BinOp::Add),
                    other => panic!("expected BinOp, got {:?}", other),
                },
                other => panic!("expected Return, got {:?}", other),
            }
        }
        other => panic!("expected FuncDef, got {:?}", other),
    }
}

#[test]
fn test_if_elif_else() {
    let src = "\
if x > 0:
    pass
elif x < 0:
    pass
else:
    pass
";
    let m = parse(src);
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::If {
            elif_clauses,
            else_body,
            ..
        } => {
            assert_eq!(elif_clauses.len(), 1);
            assert!(else_body.is_some());
        }
        other => panic!("expected If, got {:?}", other),
    }
}

#[test]
fn test_while_loop() {
    let src = "\
while True:
    break
";
    let m = parse(src);
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::While { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(&body[0].node, Stmt::Break));
        }
        other => panic!("expected While, got {:?}", other),
    }
}

#[test]
fn test_for_loop() {
    let src = "\
for i in items:
    pass
";
    let m = parse(src);
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::For { target, .. } => match &target.node {
            Expr::Name(n) => assert_eq!(n, "i"),
            other => panic!("expected Name, got {:?}", other),
        },
        other => panic!("expected For, got {:?}", other),
    }
}

#[test]
fn test_class_def() {
    let src = "\
class Foo(Base):
    pass
";
    let m = parse(src);
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::ClassDef { name, bases, .. } => {
            assert_eq!(name, "Foo");
            assert_eq!(bases.len(), 1);
        }
        other => panic!("expected ClassDef, got {:?}", other),
    }
}

#[test]
fn test_binary_precedence() {
    // 1 + 2 * 3  should parse as 1 + (2 * 3)
    let m = parse("1 + 2 * 3\n");
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::Expr(expr) => match &expr.node {
            Expr::BinOp { op, right, .. } => {
                assert_eq!(*op, BinOp::Add);
                match &right.node {
                    Expr::BinOp { op, .. } => assert_eq!(*op, BinOp::Mul),
                    other => panic!("expected BinOp(Mul), got {:?}", other),
                }
            }
            other => panic!("expected BinOp, got {:?}", other),
        },
        other => panic!("expected Expr, got {:?}", other),
    }
}

#[test]
fn test_call_expression() {
    let m = parse("print(1, 2, sep=\",\")\n");
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::Expr(expr) => match &expr.node {
            Expr::Call { args, kwargs, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(kwargs.len(), 1);
                assert_eq!(kwargs[0].0, "sep");
            }
            other => panic!("expected Call, got {:?}", other),
        },
        other => panic!("expected Expr, got {:?}", other),
    }
}

#[test]
fn test_list_dict_set() {
    let m = parse("[1, 2, 3]\n");
    match &m.body[0].node {
        Stmt::Expr(e) => assert!(matches!(&e.node, Expr::List(_))),
        other => panic!("expected Expr(List), got {:?}", other),
    }

    let m = parse("{\"a\": 1}\n");
    match &m.body[0].node {
        Stmt::Expr(e) => assert!(matches!(&e.node, Expr::Dict { .. })),
        other => panic!("expected Expr(Dict), got {:?}", other),
    }

    let m = parse("{1, 2}\n");
    match &m.body[0].node {
        Stmt::Expr(e) => assert!(matches!(&e.node, Expr::Set(_))),
        other => panic!("expected Expr(Set), got {:?}", other),
    }
}

#[test]
fn test_nested_indent() {
    let src = "\
def foo():
    if True:
        return 1
    return 0
";
    let m = parse(src);
    assert_eq!(m.body.len(), 1);
    match &m.body[0].node {
        Stmt::FuncDef { body, .. } => {
            assert_eq!(body.len(), 2); // if + return
        }
        other => panic!("expected FuncDef, got {:?}", other),
    }
}

#[test]
fn test_import() {
    let m = parse("import os.path as p\n");
    match &m.body[0].node {
        Stmt::Import { module, alias } => {
            assert_eq!(module, &["os", "path"]);
            assert_eq!(alias.as_deref(), Some("p"));
        }
        other => panic!("expected Import, got {:?}", other),
    }
}

#[test]
fn test_from_import() {
    let m = parse("from os import path, getcwd as cwd\n");
    match &m.body[0].node {
        Stmt::FromImport { module, names } => {
            assert_eq!(module, &["os"]);
            assert_eq!(names.len(), 2);
            assert_eq!(names[0], ("path".into(), None));
            assert_eq!(names[1], ("getcwd".into(), Some("cwd".into())));
        }
        other => panic!("expected FromImport, got {:?}", other),
    }
}