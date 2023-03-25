use std::{
    cmp::Ordering,
    fs::File,
    io::{BufRead, BufReader, Lines, Result},
    path::Path,
    sync::Mutex,
};

use super::types::Object::{self, Error, Integer, Null};

pub type BuiltinFunction = fn(&[Object]) -> Object;

static BUILTINS: phf::Map<&'static str, BuiltinFunction> = phf::phf_map! {
    "defined" => defined_builtin,
    "fileLines" => file_lines_builtin,
    "first" => first_builtin,
    "hasKey" => has_key_builtin,
    "last" => last_builtin,
    "len" => len_builtin,
    "push" => push_builtin,
    "puts" => puts_builtin,
    "rest" => rest_builtin,
    "sort" => sort_builtin,
    "quote" => quote_builtin,
    "toInt" => to_int_builtin,
};

pub fn has_builtin(name: &str) -> bool {
    BUILTINS.contains_key(name)
}

pub fn get_builtin(name: &str) -> &BuiltinFunction {
    BUILTINS.get(name).unwrap()
}

static OUTPUT: Mutex<Vec<String>> = Mutex::new(vec![]);

fn do_print(output: String) {
    OUTPUT.lock().unwrap().push(output);
}

pub fn get_output() -> Vec<String> {
    return OUTPUT.lock().unwrap().drain(..).collect();
}

fn defined_builtin(_objects: &[Object]) -> Object {
    // Dummy implementation to support identifying calls to `defined`
    Null
}

fn file_lines_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `fileLines`. got:{}, want:1",
            objects.len()
        ));
    }
    match &objects[0] {
        Object::String(value) => {
            if let Ok(lines_iter) = read_lines(value) {
                return Object::Array(
                    lines_iter
                        .map(|lines| Object::String(lines.ok().unwrap_or_default()))
                        .collect(),
                );
            }
            Error(format!(
                "Argument to `fileLines` cannot be opened: {value:?}"
            ))
        }
        x => Error(format!("Argument to `fileLines` not supported: {x:?}")),
    }
}

fn read_lines<P>(filename: P) -> Result<Lines<BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(BufReader::new(file).lines())
}

fn first_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `first`. got:{}, want:1",
            objects.len()
        ));
    }
    return match &objects[0] {
        Object::String(value) => {
            if value.is_empty() {
                return Error(format!(
                    "Argument to `first` has no first element: {:?}",
                    &objects[0]
                ));
            }
            Object::String(String::from(value.chars().next().unwrap()))
        }
        Object::Array(elements) => {
            if elements.is_empty() {
                return Error(format!(
                    "Argument to `first` has no first element: {:?}",
                    &objects[0]
                ));
            }
            elements.first().unwrap().clone()
        }
        x => Error(format!("Argument to `first` not supported: {:?}", x)),
    };
}

fn has_key_builtin(objects: &[Object]) -> Object {
    if objects.len() != 2 {
        return Error(format!(
            "Wrong number of arguments for `hasKey`. got:{}, want:2",
            objects.len()
        ));
    }
    let key = objects.get(1).unwrap();
    match &objects[0] {
        Object::Hash(map) => {
            if map.contains_key(key) {
                return Object::Boolean(true);
            }
            Object::Boolean(false)
        }
        x => Error(format!("Argument to `hasKey` not supported: {:?}", x)),
    }
}

fn last_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `last`. got:{}, want:1",
            objects.len()
        ));
    }
    return match &objects[0] {
        Object::String(value) => {
            if value.is_empty() {
                return Error(format!(
                    "Argument to `last` has no last element: {:?}",
                    &objects[0]
                ));
            }
            Object::String(String::from(value.chars().nth(value.len() - 1).unwrap()))
        }
        Object::Array(elements) => {
            if elements.is_empty() {
                return Error(format!(
                    "Argument to `last` has no last element: {:?}",
                    &objects[0]
                ));
            }
            elements.last().unwrap().clone()
        }
        x => Error(format!("Argument to `last` not supported: {:?}", x)),
    };
}

fn len_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `len`. got:{}, want:1",
            objects.len()
        ));
    }
    return match &objects[0] {
        Object::String(value) => Integer(value.chars().count().try_into().unwrap()),
        Object::Array(elements) => Integer(elements.len() as i64),
        x => Error(format!("Argument to `len` not supported: {:?}", x)),
    };
}

fn push_builtin(objects: &[Object]) -> Object {
    if objects.len() != 2 {
        return Error(format!(
            "Wrong number of arguments for `push`. got:{}, want:2",
            objects.len()
        ));
    }
    match (&objects[0], &objects[1]) {
        (Object::Array(elements), item) => {
            let mut new_elements = elements.clone();
            new_elements.push(item.clone());
            Object::Array(new_elements)
        }
        (array, _) => Error(format!("Argument to `push` not supported: {:?}", array)),
    }
}

fn puts_builtin(objects: &[Object]) -> Object {
    for object in objects.iter() {
        match object {
            Object::String(value) => do_print(value.to_string()),
            o => do_print(format!("{o:#?}")),
        };
    }
    Null
}

fn rest_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `rest`. got:{}, want:1",
            objects.len()
        ));
    }
    return match &objects[0] {
        Object::String(value) => {
            if value.is_empty() {
                return Error(format!(
                    "Argument to `rest` has no elements: {:?}",
                    &objects[0]
                ));
            }
            Object::String(value.chars().skip(1).collect())
        }
        Object::Array(elements) => {
            if elements.is_empty() {
                return Error(format!(
                    "Argument to `rest` has no elements: {:?}",
                    &objects[0]
                ));
            }
            Object::Array(elements.iter().skip(1).cloned().collect())
        }
        x => Error(format!("Argument to `rest` not supported: {:?}", x)),
    };
}

fn sort_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `sort`. got:{}, want:1",
            objects.len()
        ));
    }
    let compare_integer_objects = |a: &Object, b: &Object| -> Ordering {
        match (a, b) {
            (Object::Integer(l), Object::Integer(r)) => {
                if l == r {
                    Ordering::Equal
                } else if l < r {
                    return Ordering::Less;
                } else {
                    return Ordering::Greater;
                }
            }
            _ => todo!("Not implemented for objects: {a:?}, {b:?}"),
        }
    };
    match &objects[0] {
        Object::Array(elements) => {
            if elements.is_empty() {
                return Error(format!(
                    "Argument to `sort` has no elements: {:?}",
                    &objects[0]
                ));
            }
            let mut sorted_elements = elements.clone();
            sorted_elements.sort_by(compare_integer_objects);
            Object::Array(sorted_elements)
        }
        x => Error(format!("Argument to `sort` not supported: {:?}", x)),
    }
}

fn quote_builtin(_objects: &[Object]) -> Object {
    // Dummy implementation to support identifying calls to `quote`
    Null
}

fn to_int_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `rest`. got:{}, want:1",
            objects.len()
        ));
    }
    match &objects[0] {
        Object::String(value) => {
            if let Ok(int) = value.parse::<i64>() {
                Object::Integer(int)
            } else {
                Error(format!(
                    "Argument to `toInt` could not be parsed to integer: {value:?}"
                ))
            }
        }
        Object::Integer(int) => {
            Object::Integer(int.to_owned())
        }
        x => Error(format!("Argument to `toInt` not supported: {:?}", x)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{
            evaluator::eval,
            types::{
                Environment,
                Object::{self, Array, Error, Integer},
            },
        },
        parse::parser::Parser,
        token::lexer::Lexer,
    };
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc};

    fn assert_object_scenario(scenario: &str, expected: Object) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        assert_eq!(Vec::<String>::new(), parser.errors);

        let evaluated = eval(program, env);
        assert_eq!(
            expected, evaluated,
            "Failure on scenario {}, expected: {:#?}, actual: {:#?}",
            scenario, expected, evaluated
        );
    }

    #[test]
    fn len() {
        let scenarios = vec![
            (r#"len("")"#, Integer(0)),
            (r#"len("four")"#, Integer(4)),
            (r#"len("hello world")"#, Integer(11)),
            (
                r#"len(1)"#,
                Error(String::from("Argument to `len` not supported: Integer(1)")),
            ),
            (
                r#"len("one", "two")"#,
                Error(String::from(
                    "Wrong number of arguments for `len`. got:2, want:1",
                )),
            ),
            (r#"len(["hello", "world"])"#, Integer(2)),
            (r#"len([])"#, Integer(0)),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn first() {
        let scenarios = vec![
            (r#"first("hello world")"#, Object::String(String::from("h"))),
            (
                r#"first(["hello", "world"])"#,
                Object::String(String::from("hello")),
            ),
            (
                r#"first([])"#,
                Error(String::from(
                    "Argument to `first` has no first element: Array([])",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn has_key() {
        let scenarios = vec![
            ("let a = {1: 2, 3: 4}; hasKey(a, 1)", Object::Boolean(true)),
            ("let a = {1: 2, 3: 4}; hasKey(a, 9)", Object::Boolean(false)),
            (
                "let a = {true: 2, false: 4}; hasKey(a, false)",
                Object::Boolean(true),
            ),
            (
                "let a = {true: 2}; hasKey(a, false)",
                Object::Boolean(false),
            ),
            (
                "let a = {\"first\": 1}; hasKey(a, \"second\")",
                Object::Boolean(false),
            ),
            ("let a = {}; hasKey(a, 1)", Object::Boolean(false)),
            (
                "let a = [1, 2]; hasKey(a, 0)",
                Object::Error(String::from(
                    "Argument to `hasKey` not supported: Array([Integer(1), Integer(2)])",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn last() {
        let scenarios = vec![
            (r#"last("hello world")"#, Object::String(String::from("d"))),
            (
                r#"last(["hello", "world"])"#,
                Object::String(String::from("world")),
            ),
            (
                r#"last([])"#,
                Error(String::from(
                    "Argument to `last` has no last element: Array([])",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn push() {
        let scenarios = vec![
            (
                r#"push(["hello"], "world")"#,
                Object::Array(vec![
                    Object::String(String::from("hello")),
                    Object::String(String::from("world")),
                ]),
            ),
            (
                r#"push([], "hello")"#,
                Object::Array(vec![Object::String(String::from("hello"))]),
            ),
            (
                r#"push("hello", "world")"#,
                Error(String::from(
                    "Argument to `push` not supported: String(\"hello\")",
                )),
            ),
            (
                r#"push(["hello"], ["hello"], "world")"#,
                Error(String::from(
                    "Wrong number of arguments for `push`. got:3, want:2",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn rest() {
        let scenarios = vec![
            (
                r#"rest("hello world")"#,
                Object::String(String::from("ello world")),
            ),
            (
                r#"rest(["hello", "world"])"#,
                Array(vec![Object::String(String::from("world"))]),
            ),
            (
                r#"rest("")"#,
                Error(String::from(
                    "Argument to `rest` has no elements: String(\"\")",
                )),
            ),
            (
                r#"rest([])"#,
                Error(String::from(
                    "Argument to `rest` has no elements: Array([])",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn sort() {
        let scenarios = vec![
            (
                r#"sort([3, 4, 2, 1])"#,
                Array(vec![Integer(1), Integer(2), Integer(3), Integer(4)]),
            ),
            (r#"sort([1])"#, Array(vec![Integer(1)])),
            (
                r#"sort([])"#,
                Error(String::from(
                    "Argument to `sort` has no elements: Array([])",
                )),
            ),
            (
                r#"sort([], [])"#,
                Error(String::from(
                    "Wrong number of arguments for `sort`. got:2, want:1",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn to_int() {
        let scenarios = vec![
            (r#"toInt("123")"#, Object::Integer(123)),
            (r#"toInt("-123")"#, Object::Integer(-123)),
            (r#"toInt(123)"#, Object::Integer(123)),
            (r#"toInt(-123)"#, Object::Integer(-123)),
            (
                r#"toInt("not a number")"#,
                Object::Error(String::from(
                    "Argument to `toInt` could not be parsed to integer: \"not a number\"",
                )),
            ),
            (
                r#"toInt([1, 2, 3])"#,
                Object::Error(String::from(
                    "Argument to `toInt` not supported: Array([Integer(1), Integer(2), Integer(3)])",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }
}
