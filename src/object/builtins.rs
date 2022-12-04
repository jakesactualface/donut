use super::types::Object::{self, Error, Integer};

pub type BuiltinFunction = fn(&[Object]) -> Object;

static BUILTINS: phf::Map<&'static str, BuiltinFunction> = phf::phf_map! {
    "len" => len_builtin,
};

pub fn has_builtin(name: &str) -> bool {
    BUILTINS.contains_key(name)
}

pub fn get_builtin(name: &str) -> &BuiltinFunction {
    BUILTINS.get(name).unwrap()
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
        x => Error(format!("Argument to `len` not supported: {:?}", x)),
    };
}
