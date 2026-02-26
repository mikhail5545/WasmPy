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

use crate::builtin::string::*;
use crate::instruction::*;
use crate::error::RuntimeError;

#[test]
fn test_string_len_logic() {
    if let Value::NativeFunc(func) = len("hello".to_string()) {
        let result = (func.func)(&[]).unwrap();
        assert_eq!(result, Value::Int(5));
    } else {
        panic!("Expected a native function");
    }
}

#[test]
fn test_string_concat_logic() {
    if let Value::NativeFunc(func) = concat("hello".to_string()) {
        let result = (func.func)(&[Value::Str(" world".to_string())]).unwrap();
        assert_eq!(result, Value::Str("hello world".to_string()));
    } else {
        panic!("Expected a native function");
    }

    if let Value::NativeFunc(func) = concat("hello".to_string()) {
        match (func.func)(&[Value::Int(123)]) {
            Ok(_) => panic!("Expected a type error"),
            Err(e) => assert!(matches!(e, RuntimeError::TypeError(_))),
        }
    }

    if let Value::NativeFunc(func) = concat("hello".to_string()) {
        match (func.func)(&[]) {
            Ok(_) => panic!("Expected a value error"),
            Err(e) => assert!(matches!(e, RuntimeError::ValueError(_))),
        }
    }
}
