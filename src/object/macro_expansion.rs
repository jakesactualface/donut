use std::{cell::RefCell, iter::zip, rc::Rc};

use crate::parse::ast::{Expression, Node, Program, Statement, ToNode};

use super::{
    evaluator::eval,
    modify::Modifiable,
    types::{Environment, Object},
};

pub fn define_macros(program: &mut Program, env: Rc<RefCell<Environment>>) {
    let mut definitions: Vec<usize> = vec![];
    for (i, statement) in program.statements.iter().enumerate() {
        if let Statement::Let {
            name,
            value: Expression::Macro { parameters, body },
        } = statement
        {
            let owned_body: Statement = *body.to_owned();
            env.borrow_mut().set(
                name.to_owned(),
                Object::Macro {
                    parameters: parameters.clone(),
                    body: owned_body,
                    env: env.clone(),
                },
            );
            definitions.push(i);
        }
    }
    program.statements = program
        .statements
        .clone()
        .into_iter()
        .enumerate()
        .filter(|x| !is_macro(x))
        .map(|(_, e)| e)
        .collect();
}

fn is_macro<'r>((_, statement): &'r (usize, Statement)) -> bool {
    match statement {
        Statement::Let {
            name: _,
            value: Expression::Macro { .. },
        } => true,
        _ => false,
    }
}

pub fn expand_macros(program: &mut Program, env: Rc<RefCell<Environment>>) -> Program {
    Program {
        statements: program
            .statements
            .clone()
            .into_iter()
            .map(|s| s.modify(macro_modifier, env.clone()))
            .collect(),
    }
}

fn macro_modifier(node: Node, env: Rc<RefCell<Environment>>) -> Node {
    let call_expression: Expression;
    let call_arguments: Vec<Expression>;

    if let Node::Expression(Expression::Call {
        function,
        arguments,
    }) = node.to_owned()
    {
        call_expression = *function;
        call_arguments = arguments;
    } else {
        return node;
    }

    let macro_object = is_macro_call(call_expression, env);
    if let Some(Object::Macro {
        parameters,
        body,
        env,
    }) = macro_object
    {
        let args = quote_arguments(call_arguments);
        let param_names: Vec<String> = parameters.into_iter().map(get_param_name).collect();
        let eval_env = extend_macro_environment(env, param_names, args);
        let evaluated = eval(body, eval_env);

        if let Object::Quote(_) = evaluated {
            evaluated.to_node()
        } else {
            panic!("Expected returned Quote from macro");
        }
    } else {
        node
    }
}

fn is_macro_call(expression: Expression, env: Rc<RefCell<Environment>>) -> Option<Object> {
    match expression {
        Expression::Identifier { name } => {
            if let Some(retrieved_object) = env.borrow_mut().get(&name) {
                Rc::try_unwrap(retrieved_object).ok()
            } else {
                None
            }
        }
        _ => None,
    }
}

fn get_param_name(param: Expression) -> String {
    match param {
        Expression::Identifier { name } => name,
        _ => panic!("Expected identifier expression as parameter!"),
    }
}

fn quote_arguments(args: Vec<Expression>) -> Vec<Object> {
    args.into_iter().map(Object::Quote).collect()
}

fn extend_macro_environment(
    env: Rc<RefCell<Environment>>,
    param_names: Vec<String>,
    args: Vec<Object>,
) -> Rc<RefCell<Environment>> {
    let mut extended = Environment::new_enclosure(env);

    for (arg, param_name) in zip(args, param_names) {
        extended.set(param_name, arg);
    }

    Rc::new(RefCell::new(extended))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        object::{
            macro_expansion::{define_macros, expand_macros},
            types::Environment,
        },
        parse::{
            ast::{
                Expression::{self, Identifier, InfixExpression},
                Program, Statement,
            },
            parser::Parser,
        },
        token::{lexer::Lexer, types::Token},
    };

    fn test_parse_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(Vec::<String>::new(), parser.errors);
        program
    }

    #[test]
    fn macro_removal() {
        let input = "
                let number = 1;
                let function = fn(x, y) { x + y };
                let myMacro = macro(x, y) { x + y; };
            ";
        let expected_statements = vec![
            Statement::Let {
                name: String::from("number"),
                value: Expression::Integer { value: 1 },
            },
            Statement::Let {
                name: String::from("function"),
                value: Expression::Function {
                    parameters: vec![
                        Identifier {
                            name: String::from("x"),
                        },
                        Identifier {
                            name: String::from("y"),
                        },
                    ],
                    body: Box::new(Statement::Block {
                        statements: vec![Statement::Expression {
                            value: InfixExpression {
                                left: Box::new(Identifier {
                                    name: String::from("x"),
                                }),
                                operator: Token::Plus,
                                right: Box::new(Identifier {
                                    name: String::from("y"),
                                }),
                            },
                        }],
                    }),
                },
            },
        ];

        let mut program = test_parse_program(input);
        let env = Rc::new(RefCell::new(Environment::new()));

        define_macros(&mut program, env.clone());
        let statements = program.clone().statements;

        assert_eq!(2, statements.len());
        let store = &env.borrow().store;
        assert!(!store.contains_key("number"));
        assert!(!store.contains_key("function"));
        assert!(store.contains_key("myMacro"));

        for (i, statement) in statements.into_iter().enumerate() {
            let expected_statement = expected_statements.get(i).unwrap().to_owned();
            assert_eq!(expected_statement, statement);
        }
    }

    #[test]
    fn macro_expansion() {
        let scenarios = vec![
            (
                "
                    let infixExpression = macro() { quote(1 + 2); };
                    infixExpression();
                ",
                "(1 + 2)",
            ),
            (
                "
                    let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
                    reverse(2 + 2, 10 - 5);
                ",
                "(10 - 5) - (2 + 2)",
            ),
            (
                "
                    let unless = macro(condition, consequence, alternative) {
                        quote(if (!(unquote(condition))) {
                            unquote(consequence);
                        } else {
                            unquote(alternative);
                        });
                    };

                    unless(10 > 5, puts(\"not greater\"), puts(\"greater\"));
                ",
                "if (!(10 > 5)) { puts(\"not greater\") } else { puts(\"greater\") }",
            ),
            (
                "
                    let for = macro(counter, body) {
                        quote(
                            if (true) {
                                let loop = fn(c) {
                                    if ( c > 0 ) {
                                        unquote(body);
                                        loop( c - 1 );
                                    }
                                };
                                loop(unquote(counter));
                            }
                        );
                    };
                    for(5, puts(\"test\"));
                ",
                "
                    if (true) {
                        let loop = fn(c) {
                            if (c > 0) {
                                puts(\"test\");
                                loop(c - 1);
                            }
                        };
                        loop(5);
                    }
                ",
            ),
        ];

        for (input, expected) in scenarios.into_iter() {
            let expected_program = test_parse_program(expected);
            let mut actual_program = test_parse_program(input);

            let env = Rc::new(RefCell::new(Environment::new()));
            define_macros(&mut actual_program, env.clone());
            let expanded_program = expand_macros(&mut actual_program, env);
            assert_eq!(expected_program, expanded_program);
        }
    }
}
