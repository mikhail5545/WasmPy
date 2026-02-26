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
use crate::instruction::Value;
use std::collections::HashMap;

pub fn register_globals(globals: &mut HashMap<String, Value>) {
    globals.insert("print".to_string(), print());
}

pub fn print() -> Value {
   Value::NativeFunc(std::rc::Rc::new(crate::instruction::NativeFn{
        func: Box::new(|args| {
            for arg in args {
                match arg {
                    Value::Int(i) => print!("{}", i),
                    Value::Str(s) => print!("{}", s),
                    Value::None => print!("None"),
                    _ => print!("{:?}", arg),
                }
            }
            println!();
            Ok(Value::None)
        })
    }))
}