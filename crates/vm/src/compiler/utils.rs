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

impl Compiler {
    pub(super) fn assign_targets(targets: &Vec<Spanned<Expr>>, locals: &mut HashSet<String>, explicit_globals: &mut HashSet<String>) {
        for target in targets {
            if let Expr::Name(name) = &target.node {
                if !explicit_globals.contains(name) {
                    locals.insert(name.clone());
                }
            }
        }
    }
    
    /// Recursively scan statements to find all local variable names and explicitly declared global names.
    /// Should be called as pre-pass during function compilation to determine variable scopes.
    pub(super) fn scan_scope(statements: &[Spanned<Stmt>], locals: &mut HashSet<String>, explicit_globals: &mut HashSet<String>) {
        for stmt in statements {
            match &stmt.node{
                Stmt::Assign { targets, ..} => {
                    Self::assign_targets(targets, locals, explicit_globals);
                }
                Stmt::Global(names) => {
                    for name in names {
                        explicit_globals.insert(name.clone());
                        // Self-correcting: if a name is declared global, it cannot be local, so remove it from locals set
                        locals.remove(name);
                    }
                }
                Stmt::If { body, elif_clauses, else_body, .. } => {
                    Self::scan_scope(body, locals, explicit_globals);
                    for (_, elif_body) in elif_clauses {
                        Self::scan_scope(elif_body, locals, explicit_globals);
                    }
                    if let Some(else_body) = else_body {
                        Self::scan_scope(else_body, locals, explicit_globals);
                    }
                }
                Stmt::While { body, else_body, ..} => {
                    Self::scan_scope(body, locals, explicit_globals);
                    if let Some(else_body) = else_body {
                        Self::scan_scope(else_body, locals, explicit_globals);
                    }
                }
                Stmt::FuncDef { name, .. } => {
                    // Function name is local to the enclosing scope
                    if !explicit_globals.contains(name) {
                        locals.insert(name.clone());
                    }
                }
                _ => {}
            }
        }
    }
    
}