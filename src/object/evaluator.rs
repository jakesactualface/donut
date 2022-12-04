use std::{cell::RefCell, iter::zip, mem::discriminant, rc::Rc};

use crate::{
    parse::ast::{Expression, Node, Statement, ToNode},
    token::types::Token,
};

use super::{
    builtins::{get_builtin, has_builtin},
    types::{
        Environment,
        Object::{self, Array, Boolean, Builtin, Error, Function, Integer, Null, Return},
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
            return evaluated;
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
                _ => env.borrow_mut().set(name, evaluated),
            };
        }
        Statement::Return { value } => {
            let evaluated = eval(value, env);
            return match evaluated {
                Error(_) => evaluated,
                _ => Return(Box::new(evaluated)),
            };
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
            return evaluated;
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
            return match evaluated {
                Error(_) => evaluated,
                _ => eval_prefix_expression(operator, evaluated, env),
            };
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => match (eval(*left, env.clone()), eval(*right, env.clone())) {
            (Error(left), _) => {
                return Error(left);
            }
            (_, Error(right)) => {
                return Error(right);
            }
            (left, right) => {
                return eval_infix_expression(operator, left, right, env);
            }
        },
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            return eval_if_expression(*condition, consequence, alternative, env);
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
            return Array(evaluated_elements);
        }
        Expression::Index { value, index } => {
            return eval_index_expression(value, index, env);
        }
        Expression::Function { parameters, body } => Function {
            parameters,
            body: *body,
            env: env.clone(),
        },
        Expression::Call {
            function,
            arguments,
        } => {
            return eval_call_expression(function, arguments, env);
        }
    }
}

fn eval_index_expression(
    value: Box<Expression>,
    index: Box<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    match (eval(*value, env.clone()), eval(*index, env)) {
        (Error(array), _) => Error(array),
        (_, Error(index)) => Error(index),
        (Array(array), Integer(index)) => {
            if let Some(object) = array.get(index as usize) {
                return object.clone();
            }
            return Error(format!(
                "index out of bounds! Arrays are zero-indexed. Given: {}, Length: {}",
                index,
                array.len()
            ));
        }
        (array, Integer(_)) => Error(format!("index operator not implemented for: {:?}", array)),
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
    return FALSE;
}

fn eval_identifier(name: String, env: Rc<RefCell<Environment>>) -> Object {
    if let Some(object) = env.borrow().get(&name) {
        return Rc::<Object>::try_unwrap(object).unwrap();
    }
    if has_builtin(&name) {
        return Builtin(name);
    }
    return Error(format!("identifier not found: {name}"));
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
        operator => return Error(format!("unknown operator: {:?}{:?}", operator, value)),
    }
}

fn eval_infix_expression(
    operator: Token,
    left: Object,
    right: Object,
    _env: Rc<RefCell<Environment>>,
) -> Object {
    match (operator, left, right) {
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
        (Token::Equal, l, r) => native_bool_to_boolean(l == r),
        (Token::NotEqual, l, r) => native_bool_to_boolean(l != r),
        (operator, l, r) if discriminant(&l) != discriminant(&r) => {
            Error(format!("type mismatch: {:?} {:?} {:?}", l, operator, r))
        }
        (operator, l, r) => Error(format!("unknown operator: {:?} {:?} {:?}", l, operator, r)),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Box<Statement>,
    alternative: Option<Box<Statement>>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let truthy = match eval(condition, env.clone()) {
        Integer(0) => false,
        Integer(1) => true,
        TRUE => true,
        FALSE => false,
        NULL => false,
        _ => true,
    };

    if truthy {
        return eval(*consequence, env);
    } else if alternative.is_some() {
        return eval(*alternative.unwrap(), env);
    }
    return NULL;
}

fn eval_call_expression(
    function: Box<Expression>,
    arguments: Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let evaluated = eval(*function, env.clone());
    if let Error(_) = evaluated {
        return evaluated;
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

    if let Builtin(name) = evaluated {
        let function = get_builtin(&name);
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
                _ => panic!("Expected identifier!"),
            };
        }
        return match eval(inner_body, Rc::new(RefCell::new(extended_env))) {
            Return(value) => *value,
            value => value,
        };
    } else {
        return Error(format!("not a function: {evaluated:?}"));
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        object::{
            evaluator::eval,
            types::{
                Environment,
                Object::{self, Array, Boolean, Error, Function, Integer, Null},
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
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Boolean(expected));
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
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, Integer(expected));
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
        ];
        for (scenario, expected) in scenarios.into_iter() {
            assert_object_scenario(scenario, expected);
        }
    }

    #[test]
    fn builtin_functions() {
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
