use super::types::Object::{self, Error, Integer};

pub type BuiltinFunction = fn(&[Object]) -> Object;

static BUILTINS: phf::Map<&'static str, BuiltinFunction> = phf::phf_map! {
    "first" => first_builtin,
    "last" => last_builtin,
    "len" => len_builtin,
    "push" => push_builtin,
    "rest" => rest_builtin,
};

pub fn has_builtin(name: &str) -> bool {
    BUILTINS.contains_key(name)
}

pub fn get_builtin(name: &str) -> &BuiltinFunction {
    BUILTINS.get(name).unwrap()
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
            if value.len() == 0 {
                return Error(format!(
                    "Argument to `first` has no first element: {:?}",
                    &objects[0]
                ));
            }
            Object::String(String::from(value.chars().nth(0).unwrap()))
        }
        Object::Array(elements) => {
            if elements.len() == 0 {
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

fn last_builtin(objects: &[Object]) -> Object {
    if objects.len() != 1 {
        return Error(format!(
            "Wrong number of arguments for `last`. got:{}, want:1",
            objects.len()
        ));
    }
    return match &objects[0] {
        Object::String(value) => {
            if value.len() == 0 {
                return Error(format!(
                    "Argument to `last` has no last element: {:?}",
                    &objects[0]
                ));
            }
            Object::String(String::from(value.chars().nth(value.len() - 1).unwrap()))
        }
        Object::Array(elements) => {
            if elements.len() == 0 {
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
    return match (&objects[0], &objects[1]) {
        (Object::Array(elements), item) => {
            let mut new_elements = elements.clone();
            new_elements.push(item.clone());
            return Object::Array(new_elements);
        }
        (array, _) => Error(format!("Argument to `push` not supported: {:?}", array)),
    };
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
            if value.len() == 0 {
                return Error(format!(
                    "Argument to `rest` has no elements: {:?}",
                    &objects[0]
                ));
            }
            Object::String(value.chars().skip(1).collect())
        }
        Object::Array(elements) => {
            if elements.len() == 0 {
                return Error(format!(
                    "Argument to `rest` has no elements: {:?}",
                    &objects[0]
                ));
            }
            Object::Array(elements.iter().skip(1).map(|e| e.clone()).collect())
        }
        x => Error(format!("Argument to `rest` not supported: {:?}", x)),
    };
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
}
