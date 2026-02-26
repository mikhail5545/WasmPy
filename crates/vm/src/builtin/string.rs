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
use std::rc::Rc;
use crate::error::RuntimeError;
use crate::instruction::{Value, NativeFn};

pub fn get_string_attribute(target: &str, attr_name: &str) -> Option<Value> {
    match attr_name {
        "len" => Some(len(target.to_string())),
        "concat" => Some(concat(target.to_string())),
        "format" => Some(format(target.to_string())),
        _ => None,
    }
}

pub fn len(s: String) -> Value {
    Value::NativeFunc(Rc::new(NativeFn{
        func: Box::new(move |_args| {
            Ok(Value::Int(s.len() as i64))
        })
    }))
}

pub fn concat(s: String) -> Value {
    Value::NativeFunc(Rc::new(NativeFn{
        func: Box::new(move |args| {
            match args.get(0) {
                Some(Value::Str(other)) => Ok(Value::Str(s.clone() + other)),
                Some(other) => Err(RuntimeError::TypeError(format!("TypeError: can only concat str (not {:?}) to str", other))),
                None => Err(RuntimeError::ValueError("ValueError: concat expects 1 argument".to_string())),
            }
        })
    }))
}

pub fn format(s: String) -> Value{
    Value::NativeFunc(Rc::new(NativeFn{
        func: Box::new(move |args| {
            let mut result = s.clone();
            for (i, arg) in args.iter().enumerate() {
                let placeholder = format!("{{{}}}", i);
                let replacement = match arg {
                    Value::Int(n) => n.to_string(),
                    Value::Str(s) => s.clone(),
                    other => format!("{:?}", other),
                };
                result = result.replace(&placeholder, &replacement);
            }
            Ok(Value::Str(result))
        })
    }))
}
