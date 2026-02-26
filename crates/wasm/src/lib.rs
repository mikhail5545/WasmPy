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

pub mod compiler;
pub mod functions;
pub mod type_checker;
pub mod types;

#[cfg(test)]
mod tests;

use ast::Module;
use type_checker::TypeError;

/// Compile a parsed AST module to WASM bytecode.
///
/// This is the main entry point for the static compilation pipeline:
/// 1. Runs the type checker to enforce type annotations and reject dynamic typing
/// 2. Compiles the type-checked AST to WASM binary
///
/// Returns the WASM binary bytes on success, or a list of type errors on failure.
pub fn compile(module: &Module) -> Result<Vec<u8>, Vec<TypeError>> {
    // Phase 1: Type check
    let checker = type_checker::TypeChecker::new();
    let _type_env = checker.check_module(module)?;

    // Phase 2: Code generation
    let mut wasm_compiler = compiler::Compiler::new();
    wasm_compiler.compile_module(module);
    Ok(wasm_compiler.finish())
}
