use super::types::Object::{self, Error, Integer};

pub type BuiltinFunction = fn(&[Object]) -> Object;

static BUILTINS: phf::Map<&'static str, BuiltinFunction> = phf::phf_map! {
    "first" => first_builtin,
    "last" => last_builtin,
    "len" => len_builtin,
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
