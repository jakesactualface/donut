use std::{cell::RefCell, collections::HashMap, iter::zip, mem::discriminant, rc::Rc};

use crate::{
    parse::ast::{Expression, Node, Statement, ToNode},
    token::types::Token,
};

use super::{
    builtins::{get_builtin, has_builtin},
    modify::{Modifiable, ModifierFunction},
    types::{
        Environment,
        Object::{
            self, Array, Boolean, Builtin, Error, Function, Hash, Integer, Null, Quote, Return,
        },
    },
};

const NULL: Object = Null;
const TRUE: Object = Boolean(true);
const FALSE: Object = Boolean(false);

pub fn eval(node: impl ToNode, env: Rc<RefCell<Environment>>) -> Object {
    match node.to_node() {
        Node::Program(statements) => {
            let mut evaluated = NULL;
            for statement in statements.into_iter() {
                evaluated = eval(statement, env.clone());
                match evaluated {
                    // Statement was a "return" statement, return unwrapped
                    Return(object) => {
                        return *object;
                    }
                    // Statement was an "error" statement, propagate
                    Error(_) => {
                        return evaluated;
                    }
                    _ => continue,
                };
            }
            evaluated
        }
        Node::Statement(s) => eval_statement(s, env),
        Node::Expression(e) => eval_expression(e, env),
    }
}

fn eval_statement(statement: Statement, env: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Let { name, value } => {
            let evaluated = eval(value, env.clone());
            return match evaluated {
                Error(_) => evaluated,
                _ => env.borrow_mut().assign(name, evaluated),
            };
        }
        Statement::Return { value } => {
            let evaluated = eval(value, env);
            match evaluated {
                Error(_) => evaluated,
                _ => Return(Box::new(evaluated)),
            }
        }
        Statement::Expression { value } => eval_expression(value, env),
        Statement::Block { statements } => {
            let mut evaluated = NULL;
            for statement in statements.into_iter() {
                evaluated = eval_statement(statement, env.clone());
                match evaluated {
                    // Statement was a "return" statement, propagate
                    Return(_) => {
                        return evaluated;
                    }
                    // Statement was an "error" statement, propagate
                    Error(_) => {
                        return evaluated;
                    }
                    _ => continue,
                };
            }
            evaluated
        }
    }
}

fn eval_expression(expression: Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::Identifier { name } => eval_identifier(name, env),
        Expression::Integer { value } => Integer(value),
        Expression::String { value } => Object::String(value),
        Expression::Boolean { value } => native_bool_to_boolean(value),
        Expression::PrefixExpression { operator, value } => {
            let evaluated = eval(*value, env.clone());
            match evaluated {
                Error(_) => evaluated,
                _ => eval_prefix_expression(operator, evaluated, env),
            }
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => eval_infix_expression(*left, operator, *right, env),
        Expression::ShortCircuitExpression {
            left,
            operator,
            right,
        } => eval_short_circuit_expression(*left, operator, *right, env),
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, *consequence, alternative, env),
        Expression::WhileExpression { condition, body } => {
            eval_while_expression(*condition, body, env)
        }
        Expression::Array { elements } => {
            let mut evaluated_elements: Vec<Object> = vec![];
            for element in elements.into_iter() {
                let evaluated_element = eval(element, env.clone());
                if let Error(_) = evaluated_element {
                    // Error encountered in element evaluation, propagate
                    return evaluated_element;
                }
                evaluated_elements.push(evaluated_element);
            }
            Array(evaluated_elements)
        }
        Expression::Hash { pairs } => eval_hash_expression(pairs, env),
        Expression::Index { value, index } => eval_index_expression(*value, *index, env),
        Expression::Function { parameters, body } => Function {
            parameters,
            body: *body,
            env,
        },
        Expression::Call {
            function,
            arguments,
        } => eval_call_expression(*function, arguments, env),
        Expression::Mutation { target, value } => eval_mutation_expression(*target, *value, env),
        Expression::Macro {
            parameters: _parameters,
            body: _body,
        } => Error(String::from("Cannot evaluate unexpanded macro!")),
    }
}

fn eval_defined(arguments: Vec<Expression>, env: Rc<RefCell<Environment>>) -> Object {
    if arguments.len() != 1 {
        return Error(String::from("Expected single argument for `defined`!"));
    }
    let argument = eval_expression(arguments.first().unwrap().clone(), env);
    match argument {
        Return(x) => Return(x),
        Error(_) => Boolean(false),
        Null => Boolean(false),
        _ => Boolean(true),
    }
}

fn eval_quote(arguments: Vec<Expression>, env: Rc<RefCell<Environment>>) -> Object {
    if arguments.len() != 1 {
        return Error(String::from("Expected single argument for `quote`!"));
    }
    return Quote(eval_unquoted(arguments.get(0).unwrap().clone(), env));
}

fn eval_unquoted<T: ToNode + Modifiable>(quoted: T, env: Rc<RefCell<Environment>>) -> T {
    let modifier: ModifierFunction = |n, e| -> Node {
        match n.clone() {
            Node::Expression(Expression::Call {
                function,
                arguments,
            }) => {
                if let Expression::Identifier { name } = *function {
                    if name == "unquote" {
                        if arguments.len() != 1 {
                            return Error(String::from("Expected single argument for Unquote!"))
                                .to_node();
                        }
                        let arg = arguments.get(0).unwrap().clone();
                        return eval(arg, e).to_node();
                    }
                }
                n
            }
            Node::Expression(Expression::Identifier { name }) => {
                if name == "unquote" {
                    return eval(n, Rc::new(RefCell::new(Environment::new()))).to_node();
                }
                n
            }
            _ => n,
        }
    };
    quoted.modify(modifier, env)
}

fn eval_index_expression(
    value: Expression,
    index: Expression,
    env: Rc<RefCell<Environment>>,
) -> Object {
    match (eval(value, env.clone()), eval(index, env)) {
        (Error(x), _) => Error(x),
        (_, Error(index)) => Error(index),
        (Array(array), Integer(index)) => {
            if let Some(object) = array.get(index as usize) {
                return object.clone();
            }
            Error(format!(
                "index out of bounds! Arrays are zero-indexed. Given: {}, Length: {}",
                index,
                array.len()
            ))
        }
        (Hash(map), key) => {
            if let Some(value) = map.get(&key) {
                return value.clone();
            }
            NULL
        }
        (Object::String(string), Integer(index)) => {
            if let Some(object) = string.chars().nth(index as usize) {
                return Object::String(String::from(object));
            }
            Error(format!(
                "index out of bounds! Arrays are zero-indexed. Given: {}, Length: {}",
                index,
                string.len()
            ))
        }
        (collection, Integer(_)) => Error(format!(
            "index operator not implemented for: {:?}",
            collection
        )),
        (_, index) => Error(format!(
            "expected integer for index value, received: {:?}",
            index
        )),
    }
}

fn native_bool_to_boolean(input: bool) -> Object {
    if input {
        return TRUE;
    }
    FALSE
}

fn eval_identifier(name: String, env: Rc<RefCell<Environment>>) -> Object {
    if let Some(object) = env.borrow().get(&name) {
        return Rc::<Object>::try_unwrap(object).unwrap();
    }
    if has_builtin(&name) {
        return Builtin(name);
    }
    Error(format!("identifier not found: {name}"))
}

fn eval_prefix_expression(
    operator: Token,
    value: Object,
    _env: Rc<RefCell<Environment>>,
) -> Object {
    match operator {
        Token::Bang => match value {
            TRUE => FALSE,
            FALSE => TRUE,
            Integer(0) => TRUE,
            NULL => TRUE,
            _ => FALSE,
        },
        Token::Minus => match value {
            Integer(i) => Integer(-i),
            value => Error(format!("unknown operator: {:?}{:?}", operator, value)),
        },
        operator => Error(format!("unknown operator: {:?}{:?}", operator, value)),
    }
}

fn eval_infix_expression(
    left: Expression,
    operator: Token,
    right: Expression,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let evaluated_left: Object;
    let evaluated_right: Object;
    match (eval(left, env.clone()), eval(right, env)) {
        (Error(left), _) => {
            return Error(left);
        }
        (_, Error(right)) => {
            return Error(right);
        }
        (left, right) => {
            evaluated_left = left;
            evaluated_right = right;
        }
    };

    match (operator, evaluated_left, evaluated_right) {
        (operator, Integer(l), Integer(r)) => match operator {
            Token::Plus => Integer(l + r),
            Token::Minus => Integer(l - r),
            Token::Asterisk => Integer(l * r),
            Token::Slash => Integer(l / r),
            Token::LT => native_bool_to_boolean(l < r),
            Token::GT => native_bool_to_boolean(l > r),
            Token::Equal => native_bool_to_boolean(l == r),
            Token::NotEqual => native_bool_to_boolean(l != r),
            operator => Error(format!("unknown operator: {:?} {:?} {:?}", l, operator, r)),
        },
        (Token::Plus, Object::String(l), Object::String(r)) => Object::String(l + &r),
        (Token::Plus, Object::String(l), r) => Object::String(format!("{l}{r:?}")),
        (Token::Plus, l, Object::String(r)) => Object::String(format!("{l:?}{r}")),
        (Token::And, Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l && r),
        (Token::Or, Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l || r),
        (Token::Equal, l, r) => native_bool_to_boolean(l == r),
        (Token::NotEqual, l, r) => native_bool_to_boolean(l != r),
        (operator, l, r) if discriminant(&l) != discriminant(&r) => {
            Error(format!("type mismatch: {:?} {:?} {:?}", l, operator, r))
        }
        (operator, l, r) => Error(format!("unknown operator: {:?} {:?} {:?}", l, operator, r)),
    }
}

fn eval_short_circuit_expression(
    left: Expression,
    operator: Token,
    right: Expression,
    env: Rc<RefCell<Environment>>,
) -> Object {
    match operator {
        Token::And => {
            let evaluated_left = eval(left, env.clone());
            match evaluated_left {
                Error(_) => evaluated_left,
                Boolean(false) => evaluated_left,
                Boolean(true) => eval(right, env),
                _ => Error(String::from("Expected boolean expression!")),
            }
        }
        Token::Or => {
            let evaluated_left = eval(left, env.clone());
            match evaluated_left {
                Error(_) => evaluated_left,
                Boolean(true) => evaluated_left,
                Boolean(false) => eval(right, env),
                _ => Error(String::from("Expected boolean expression!")),
            }
        }
        _ => Error(String::from("Expected boolean operand!")),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Statement,
    alternative: Option<Box<Statement>>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let truthy = is_truthy(condition, env.clone());

    if truthy {
        return eval(consequence, env);
    } else if alternative.is_some() {
        return eval(*alternative.unwrap(), env);
    }
    NULL
}

fn eval_while_expression(
    condition: Expression,
    body: Box<Statement>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    while is_truthy(condition.clone(), env.clone()) {
        let evaluated = eval(*body.clone(), env.clone());
        match evaluated {
            // Statement was a "return" statement, propagate
            Return(_) => {
                return evaluated;
            }
            // Statement was an "error" statement, propagate
            Error(_) => {
                return evaluated;
            }
            _ => (),
        };
    }
    NULL
}

fn is_truthy(expression: Expression, env: Rc<RefCell<Environment>>) -> bool {
    match eval(expression, env) {
        Integer(0) => false,
        Integer(1) => true,
        TRUE => true,
        FALSE => false,
        NULL => false,
        _ => true,
    }
}

fn eval_call_expression(
    function: Expression,
    arguments: Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let evaluated = eval(function, env.clone());
    if let Error(_) = evaluated {
        return evaluated;
    }

    // Some builtin functions must be handled separately to access enviornment
    if let Builtin(builtin_name) = &evaluated {
        match builtin_name.as_str() {
            "defined" => {
                return eval_defined(arguments, env);
            }
            "quote" => {
                return eval_quote(arguments, env);
            }
            _ => (),
        }
    }

    let mut evaluated_arguments: Vec<Object> = vec![];
    for argument in arguments.into_iter() {
        let evaluated_argument = eval(argument, env.clone());
        if let Error(_) = evaluated_argument {
            // Error encountered in argument evaluation, propagate
            return evaluated_argument;
        }
        evaluated_arguments.push(evaluated_argument);
    }

    if let Builtin(name) = &evaluated {
        let function = get_builtin(name);
        return function(evaluated_arguments.as_slice());
    }

    if let Function {
        parameters: inner_params,
        body: inner_body,
        env: inner_env,
    } = evaluated
    {
        let mut extended_env = Environment::new_enclosure(inner_env);
        // Populate each parameter value in the new environment
        for (arg, param) in zip(evaluated_arguments, inner_params) {
            match param {
                Expression::Identifier { name } => extended_env.set(name, arg),
                _ => return Error(String::from("Expected identifier!")),
            };
        }
        match eval(inner_body, Rc::new(RefCell::new(extended_env))) {
            Return(value) => *value,
            value => value,
        }
    } else {
        Error(format!("not a function: {evaluated:?}"))
    }
}

fn eval_hash_expression(
    pairs: Vec<(Expression, Expression)>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let mut hash: HashMap<Object, Object> = HashMap::new();

    for (key_expression, value_expression) in pairs.into_iter() {
        let key = eval(key_expression, env.clone());
        if let Error(_) = key {
            // Error encountered in key evaluation, propagate
            return key;
        }

        let value = eval(value_expression, env.clone());
        if let Error(_) = value {
            // Error encountered in value evaluation, propagate
            return value;
        }

        hash.insert(key, value);
    }

    Hash(hash)
}

fn eval_mutation_expression(
    target: Expression,
    value: Expression,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let evaluated = eval(value, env.clone());
    if let Error(_) = evaluated {
        return evaluated;
    }

    match target {
        Expression::Identifier { name } => {
            return env.borrow_mut().reassign(name, evaluated.clone());
        }
        Expression::Index { value, index } => {
            let value_name: String;
            if let Expression::Identifier { name } = *value {
                value_name = name;
            } else {
                return Error(String::from("Cannot mutate literal!"));
            }

            let evaluated_index = eval(*index, env.clone());

            let mut borrowed_env = env.borrow_mut();
            let retrieved_value = borrowed_env.store.get_mut(&value_name);
            if retrieved_value.is_none() {
                return Error(String::from("Mutation target is not defined!"));
            }

            return match (retrieved_value.unwrap(), evaluated_index) {
                (Error(x), _) => Error(x.clone()),
                (_, Error(index)) => Error(index),
                (Array(array), Integer(index)) => {
                    if index as usize >= array.len() {
                        return Error(format!(
                            "index out of bounds! Arrays are zero-indexed. Given: {}, Length: {}",
                            index,
                            array.len()
                        ));
                    }
                    array.push(evaluated.clone());
                    array.swap_remove(index as usize);
                    return evaluated;
                }
                (Hash(map), key) => {
                    map.insert(key, evaluated.clone());
                    return evaluated;
                }
                (Object::String(string), Integer(index)) => {
                    if let Object::String(mut new_letter) = evaluated {
                        new_letter.truncate(1);
                        string
                            .replace_range(index as usize..index as usize + 1, new_letter.as_str());
                        return Object::String(string.to_string());
                    } else {
                        return Error(format!(
                            "Cannot insert non-string value into string: {evaluated:?}"
                        ));
                    }
                }
                (t, i) => Error(format!(
                    "Target {t:?} does not support mutation with index {i:?}"
                )),
            };
        }
        t => Error(format!("Mutation target not supported: {t:?}")),
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

    use crate::{
        object::{
            evaluator::eval,
            types::{
                Environment,
                Object::{self, Array, Boolean, Error, Function, Hash, Integer, Null, Quote},
            },
        },
        parse::{
            ast::{
                Expression::{self, Identifier, InfixExpression},
                Statement,
            },
            parser::Parser,
        },
        token::{lexer::Lexer, types::Token},
    };
    use pretty_assertions::assert_eq;

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

    fn assert_multiple_objects_scenario(scenario: &str, expected_objects: Vec<Object>) {
        let lexer = Lexer::new(scenario);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        assert_eq!(Vec::<String>::new(), parser.errors);

        for (i, statement) in program.statements.into_iter().enumerate() {
            let expected_object = expected_objects.get(i).unwrap().clone();
            let evaluated = eval(statement, env.clone());
            assert_eq!(
                expected_object, evaluated,
                "Failure on scenario {}, expected: {:#?}, actual: {:#?}",
                scenario, expected_object, evaluated
            );
        }
    }

    #[test]
    fn integer_expressions() {
        let scenarios = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
        }
    }

    #[test]
    fn string_expressions() {
        let scenarios = vec![
            ("\"Hello World!\"", "Hello World!"),
            ("\"foo\\\"bar\"", "foo\"bar"),
            ("\"Hello\" + \" \" + \"World!\"", "Hello World!"),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Object::String(String::from(expected)));
        }
    }

    #[test]
    fn boolean_expressions() {
        let scenarios = vec![
            ("true", true),
            ("false", false),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("\"Hello\" == \"Hello\"", true),
            ("\"Hello\" != \"Hello\"", false),
            ("\"Hello\" == \"World\"", false),
            ("\"Hello\" != \"World\"", true),
            ("true && true", true),
            ("true && false", false),
            ("false && false", false),
            ("(1 < 2) && true", true),
            ("(1 > 2) && true", false),
            ("true || true", true),
            ("true || false", true),
            ("false || false", false),
            ("(1 < 2) || true", true),
            ("(1 > 2) || true", true),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Boolean(expected));
        }
    }

    #[test]
    fn short_circuit_boolean_expressions() {
        let scenarios = vec![
            (
                "let a = 1; true && ( if (true) { mut a = 2; true; } ) a;",
                Integer(2),
            ),
            (
                "let a = 1; false && ( if (true) { mut a = 2; true; } ) a;",
                Integer(1),
            ),
            (
                "let a = 1; true || ( if (true) { mut a = 2; true; } ) a;",
                Integer(1),
            ),
            (
                "let a = 1; false || ( if (true) { mut a = 2; true; } ) a;",
                Integer(2),
            ),
            (
                "let a = 1; false || ( if (true) { mut a = 2; true; } ) a;",
                Integer(2),
            ),
            (
                "let a = 1; false || ( if (false) { mut a = 2; true; } ) a;",
                Integer(1),
            ),
            (
                "let a = 1; (a < 2) && ( if (true) { mut a = 2; true; } ) a;",
                Integer(2),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn if_else_expressions() {
        let scenarios = vec![
            ("if (true) { 10 }", Integer(10)),
            ("if (false) { 10 }", Null),
            ("if (1) { 10 }", Integer(10)),
            ("if (1 < 2) { 10 }", Integer(10)),
            ("if (1 > 2) { 10 }", Null),
            ("if (1 > 2) { 10 } else { 20 }", Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Integer(10)),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn while_expressions() {
        let scenarios = vec![
            ("while (false) { 10 }", Null),
            (
                "let a = 1; while (a < 5) { mut a = a + 1; }; a;",
                Integer(5),
            ),
            (
                "let a = 10; while (a < 5) { mut a = a + 1; }; a;",
                Integer(10),
            ),
            (
                "let a = 1; while (a < 10000) { mut a = a + 1; }; a;",
                Integer(10000),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn return_statements() {
        let scenarios = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "
                    if (10 > 1) {
                        if (10 > 1) {
                            return 10;
                        }
                        return 1;
                    }
                ",
                10,
            ),
            (
                "
                    let f = fn(x) {
                        return x;
                        x + 10;
                    };
                    f(10);
                ",
                10,
            ),
            (
                "
                    let f = fn(x) {
                        let result = x + 10;
                        return result;
                        return 10;
                    };
                    f(10);
                ",
                20,
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
        }
    }

    #[test]
    fn error_handling() {
        let scenarios = vec![
            ("5 + true;", "type mismatch: Integer(5) Plus Boolean(true)"),
            (
                "5 + true; 5;",
                "type mismatch: Integer(5) Plus Boolean(true)",
            ),
            ("-true", "unknown operator: MinusBoolean(true)"),
            (
                "true + false;",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "if ( 10 > 1) { true + false; }",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            (
                "
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }
                ",
                "unknown operator: Boolean(true) Plus Boolean(false)",
            ),
            ("foobar", "identifier not found: foobar"),
            (
                "\"Hello\" - \"World\"",
                "unknown operator: String(\"Hello\") Minus String(\"World\")",
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Error(String::from(expected)));
        }
    }

    #[test]
    fn let_statements() {
        let scenarios = vec![
            ("let a = 5; a;", Integer(5)),
            ("let a = 5 * 5; a;", Integer(25)),
            ("let a = 5; let b = a; b;", Integer(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", Integer(15)),
            (
                "let a = 1; let a = 2;",
                Error(String::from(
                    "Declaration already exists in current context for identifier: a",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn mutations() {
        let scenarios = vec![
            ("let a = 5; mut a = 10; a;", Integer(10)),
            ("let a = 5; mut a = a * 5; a;", Integer(25)),
            ("let a = 5; let b = a; mut a = 10; b;", Integer(5)),
            ("let a = 5; let b = a; mut b = 10; b;", Integer(10)),
            ("let a = 5; let b = 10; mut a = b; b;", Integer(10)),
            (
                "let a = 5; let update = fn(x) { mut a = x + 10; }; update(1); a;",
                Integer(11),
            ),
            (
                "let a = 5; let update = fn(x) { mut a = x + 10; }; update(1);",
                Integer(11),
            ),
            (
                "mut a = 5;",
                Error(String::from("No previous declaration for identifier: a")),
            ),
            (
                "let a = [1, 2, 3]; mut a[1] = 4; a;",
                Array(vec![Integer(1), Integer(4), Integer(3)]),
            ),
            (
                "let a = {\"a\": 1, \"b\": 2}; mut a[\"a\"] = 4; a;",
                Hash(HashMap::from([
                    (Object::String(String::from("a")), Integer(4)),
                    (Object::String(String::from("b")), Integer(2)),
                ])),
            ),
            (
                "let a = \"Hello\"; mut a[0] = \"M\"; a;",
                Object::String(String::from("Mello")),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn function_objects() {
        let scenario = (
            "fn(x) { x + 2; };",
            Function {
                parameters: vec![Identifier {
                    name: String::from("x"),
                }],
                body: Statement::Block {
                    statements: vec![Statement::Expression {
                        value: InfixExpression {
                            left: Box::new(Identifier {
                                name: String::from("x"),
                            }),
                            operator: Token::Plus,
                            right: Box::new(Expression::Integer { value: 2 }),
                        },
                    }],
                },
                env: Rc::new(RefCell::new(Environment::new())),
            },
        );
        assert_object_scenario(scenario.0, scenario.1);
    }

    #[test]
    fn function_application() {
        let scenarios = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
        }
    }

    #[test]
    fn closures() {
        let scenarios = vec![
            (
                "
                    let newAdder = fn(x) {
                        fn(y) { x + y };
                    };
                    let addTwo = newAdder(2);
                    addTwo(2);
                ",
                Integer(4),
            ),
            (
                "
                    let counter = fn(x) {
                        if (x > 100) {
                            return true;
                        } else {
                            let foobar = 9999;
                            counter(x + 1);
                        }
                    };
                    counter(0);
                ",
                Boolean(true),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn array_expressions() {
        let scenarios = vec![
            (
                "[1, 2* 2, 3 + 3]",
                Array(vec![Integer(1), Integer(4), Integer(6)]),
            ),
            ("[]", Array(vec![])),
            (
                "[unknown]",
                Error(String::from("identifier not found: unknown")),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn hash_expressions() {
        let scenarios = vec![
            (
                "
                let two = \"two\";
                {
                    \"one\": 10 - 9,
                    two: 1 + 1,
                    \"thr\" + \"ee\": 6 / 2,
                    4: 4,
                    true: 5,
                    false: 6,
                    [1, 2]: [3, 4]
                }
            ",
                Hash(HashMap::from([
                    (Object::String(String::from("one")), Integer(1)),
                    (Object::String(String::from("two")), Integer(2)),
                    (Object::String(String::from("three")), Integer(3)),
                    (Integer(4), Integer(4)),
                    (Boolean(true), Integer(5)),
                    (Boolean(false), Integer(6)),
                    (
                        Array(vec![Integer(1), Integer(2)]),
                        Array(vec![Integer(3), Integer(4)]),
                    ),
                ])),
            ),
            (
                "
                {
                    unknown: true
                }
            ",
                Error(String::from("identifier not found: unknown")),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn index_expressions() {
        let scenarios = vec![
            ("[1, 2, 3][0]", Integer(1)),
            ("[1, 2, 3][1]", Integer(2)),
            ("[1, 2, 3][2]", Integer(3)),
            ("let i = 0; [1][i]", Integer(1)),
            ("[1, 2, 3][1 + 1]", Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
                Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Integer(2),
            ),
            (
                "[1, 2, 3][3]",
                Error(String::from(
                    "index out of bounds! Arrays are zero-indexed. Given: 3, Length: 3",
                )),
            ),
            (
                "[1, 2, 3][-1]",
                Error(String::from(
                    "index out of bounds! Arrays are zero-indexed. Given: -1, Length: 3",
                )),
            ),
            (
                "[1, 2, 3][\"test\"]",
                Error(String::from(
                    "expected integer for index value, received: String(\"test\")",
                )),
            ),
            (
                "123[0]",
                Error(String::from(
                    "index operator not implemented for: Integer(123)",
                )),
            ),
            (r#"{"foo": 5}["foo"]"#, Integer(5)),
            (r#"{"foo": 5}["bar"]"#, Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Integer(5)),
            (r#"{}["foo"]"#, Null),
            (r#"{5: 5}[5]"#, Integer(5)),
            (r#"{true: 5}[true]"#, Integer(5)),
            (r#"{false: 5}[false]"#, Integer(5)),
            (r#"{"name": "Monkey"}[fn(x) { x }];"#, Null),
            (
                r#"{fn(x) { x }: "Monkey"}[fn(x) { x }];"#,
                Object::String(String::from("Monkey")),
            ),
            (
                "let myString = \"Hello World!\"; myString[4]",
                Object::String(String::from("o")),
            ),
            ("\"Hello World!\"[0]", Object::String(String::from("H"))),
            (
                "\"Hello World!\"[20]",
                Error(String::from(
                    "index out of bounds! Arrays are zero-indexed. Given: 20, Length: 12",
                )),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn builtin_functions() {
        let scenarios = vec![(r#"len("")"#, Integer(0))];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn defined() {
        let scenarios = vec![
            ("let a = 1; defined(a)", Boolean(true)),
            ("let a = 1; defined(b)", Boolean(false)),
            ("let a = [1, 2, 3]; defined(a[0])", Boolean(true)),
            ("let a = [1, 2, 3]; defined(a[5])", Boolean(false)),
            (
                "let a = {1: true, 2: 99, 3: \"test\"}; defined(a[3])",
                Boolean(true),
            ),
            (
                "let a = {1: true, 2: 99, 3: \"test\"}; defined(a[5])",
                Boolean(false),
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn quote_unquote() {
        let scenarios = vec![
            ("quote(5)", vec![Quote(Expression::Integer { value: 5 })]),
            (
                "quote(5 + 8)",
                vec![Quote(Expression::InfixExpression {
                    left: Box::new(Expression::Integer { value: 5 }),
                    operator: Token::Plus,
                    right: Box::new(Expression::Integer { value: 8 }),
                })],
            ),
            (
                "quote(foobar)",
                vec![Quote(Expression::Identifier {
                    name: String::from("foobar"),
                })],
            ),
            (
                "quote(foobar + barfoo)",
                vec![Quote(Expression::InfixExpression {
                    left: Box::new(Expression::Identifier {
                        name: String::from("foobar"),
                    }),
                    operator: Token::Plus,
                    right: Box::new(Expression::Identifier {
                        name: String::from("barfoo"),
                    }),
                })],
            ),
            (
                "quote(unquote(4))",
                vec![Quote(Expression::Integer { value: 4 })],
            ),
            (
                "quote(unquote(4 + 4))",
                vec![Quote(Expression::Integer { value: 8 })],
            ),
            (
                "quote(8 + unquote(4 + 4))",
                vec![Quote(Expression::InfixExpression {
                    left: Box::new(Expression::Integer { value: 8 }),
                    operator: Token::Plus,
                    right: Box::new(Expression::Integer { value: 8 }),
                })],
            ),
            (
                "quote(unquote(4 + 4) + 8)",
                vec![Quote(Expression::InfixExpression {
                    left: Box::new(Expression::Integer { value: 8 }),
                    operator: Token::Plus,
                    right: Box::new(Expression::Integer { value: 8 }),
                })],
            ),
            (
                "
                    let foobar = 8;
                    quote(foobar)
                    foobar
                ",
                vec![
                    Integer(8),
                    Quote(Expression::Identifier {
                        name: String::from("foobar"),
                    }),
                    Integer(8),
                ],
            ),
            (
                "
                    let foobar = 8;
                    quote(unquote(foobar))
                    foobar
                ",
                vec![
                    Integer(8),
                    Quote(Expression::Integer { value: 8 }),
                    Integer(8),
                ],
            ),
            (
                "quote(unquote(true))",
                vec![Quote(Expression::Boolean { value: true })],
            ),
            (
                "quote(unquote(false))",
                vec![Quote(Expression::Boolean { value: false })],
            ),
            (
                "quote(unquote([1, 2, 3]))",
                vec![Quote(Expression::Array {
                    elements: vec![
                        Expression::Integer { value: 1 },
                        Expression::Integer { value: 2 },
                        Expression::Integer { value: 3 },
                    ],
                })],
            ),
            (
                "quote(unquote(quote(4 + 4)))",
                vec![Quote(Expression::InfixExpression {
                    left: Box::new(Expression::Integer { value: 4 }),
                    operator: Token::Plus,
                    right: Box::new(Expression::Integer { value: 4 }),
                })],
            ),
            (
                "
                    let quotedInfixExpression = quote(4 + 4);
                    quote(unquote(4 + 4) + unquote(quotedInfixExpression))
                ",
                vec![
                    Quote(Expression::InfixExpression {
                        left: Box::new(Expression::Integer { value: 4 }),
                        operator: Token::Plus,
                        right: Box::new(Expression::Integer { value: 4 }),
                    }),
                    Quote(Expression::InfixExpression {
                        left: Box::new(Expression::Integer { value: 8 }),
                        operator: Token::Plus,
                        right: Box::new(Expression::InfixExpression {
                            left: Box::new(Expression::Integer { value: 4 }),
                            operator: Token::Plus,
                            right: Box::new(Expression::Integer { value: 4 }),
                        }),
                    }),
                ],
            ),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_multiple_objects_scenario(scenario, expected);
        }
    }

    #[test]
    fn sprinkle() {
        let mut filename_1 = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        filename_1.push("resources/test/sprinkle1.donut");
        let mut filename_2 = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        filename_2.push("resources/test/sprinkle2.donut");

        let sprinkle_1 = format!(r#"sprinkle "{}"; c"#, filename_1.display());
        let sprinkle_2 = format!(r#"sprinkle "{}"; e"#, filename_2.display());

        let scenarios = vec![
            (sprinkle_1.as_str(), Integer(3)),
            (sprinkle_2.as_str(), Integer(7)),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }
}
