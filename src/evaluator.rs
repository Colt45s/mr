use crate::{
    ast::{BlockStatement, Expression, Literal, Program, Statement},
    object::Object,
};

pub fn eval(program: &Program) -> Object {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement);

        match result {
            Object::Return(return_value) => return *return_value,
            Object::Error(_) => return result,
            _ => continue,
        };
    }

    result
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(_, _) => todo!(),
        Statement::Return(expression) => {
            let evaluated = eval_expression(expression);
            Object::Return(Box::new(evaluated))
        }
        Statement::Expression(expression) => eval_expression(expression),
    }
}

fn eval_statements(block: &BlockStatement) -> Object {
    let mut result = Object::Null;
    for statement in block.statements.iter() {
        result = eval_statement(&statement);

        match result {
            Object::Return(_) => return result,
            Object::Error(_) => return result,
            _ => continue,
        };
    }
    result
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Lit(literal) => match literal {
            Literal::IntegerLiteral(v) => Object::Integer(*v),
            Literal::BooleanLiteral(v) => Object::Boolean(*v),
            Literal::IdentLiteral(_) => todo!(),
        },
        Expression::Prefix {
            operator, right, ..
        } => eval_prefix_expression(&operator.to_string(), eval_expression(right)),
        Expression::Infix {
            left,
            operator,
            right,
            ..
        } => {
            let left_evaluated = eval_expression(left);
            let right_evaluated = eval_expression(right);

            eval_infix_expression(&operator.to_string(), left_evaluated, right_evaluated)
        }
        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => eval_if_expression(condition, consequence, alternative),
        Expression::Function {
            token,
            parameters,
            body,
        } => todo!(),
        Expression::Call {
            token,
            function,
            arguments,
        } => todo!(),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_expression(right),
        "-" => eval_minus_prefix_operator_excepssion(right),
        _ => Object::Error(format!("unknown operator: {}{}", operator, right.to_type())),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    match (&left, &right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, *l, *r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(operator, *l, *r),
        _ => Object::Error(format!(
            "type mismatch: {} {} {}",
            left.to_type(),
            operator,
            right.to_type()
        )),
    }
}

fn eval_bang_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_excepssion(right: Object) -> Object {
    match right {
        Object::Integer(v) => Object::Integer(-v),
        _ => Object::Error(format!("unknown operator: -{}", right.to_type())),
    }
}

fn eval_integer_infix_expression(operator: &str, l: i32, r: i32) -> Object {
    match operator {
        "+" => Object::Integer(l + r),
        "-" => Object::Integer(l - r),
        "*" => Object::Integer(l * r),
        "/" => Object::Integer(l / r),
        "<" => Object::Boolean(l < r),
        ">" => Object::Boolean(l > r),
        "==" => Object::Boolean(l == r),
        "!=" => Object::Boolean(l != r),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            Object::Integer(l).to_type(),
            operator,
            Object::Integer(r).to_type()
        )),
    }
}

fn eval_boolean_infix_expression(operator: &str, l: bool, r: bool) -> Object {
    match operator {
        "==" => Object::Boolean(l == r),
        "!=" => Object::Boolean(l != r),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            Object::Boolean(l).to_type(),
            operator,
            Object::Boolean(r).to_type()
        )),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
) -> Object {
    let cond = eval_expression(condition);
    if is_truthy(cond) {
        return eval_statements(consequence);
    }

    match alternative {
        Some(statement) => eval_statements(statement),
        None => Object::Null,
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        Object::Null => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 - 50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
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
        ];
        for test in tests {
            let evaluated = test_eval(test.0);
            test_boolean_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_boolean_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            match test.1 {
                Object::Integer(v) => test_integer_object(evaluated, v),
                Object::Boolean(v) => test_boolean_object(evaluated, v),
                Object::Null | _ => test_null_object(evaluated),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                if (10 > 1) {
                  return 10;
                }
                return 1;
              }
            ",
                10,
            ),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "5; true + false; false",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1 ) { true + false }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
                return 1;
              }
            ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);

            match evaluated {
                Object::Error(message) => assert_eq!(message, test.1),
                _ => {
                    println!("no error object returned. {}", evaluated);
                    assert!(false);
                    continue;
                }
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("error parse program");
        eval(&program)
    }

    fn test_integer_object(evaluated: Object, expected: i32) {
        if let Object::Integer(v) = evaluated {
            assert_eq!(v, expected);
        } else {
            panic!("object has wrong value. got {}", evaluated);
        }
    }

    fn test_boolean_object(evaluated: Object, expected: bool) {
        if let Object::Boolean(v) = evaluated {
            assert_eq!(v, expected);
        } else {
            panic!("object has wrong value. got {}", evaluated);
        }
    }

    fn test_null_object(evaluated: Object) {
        if !matches!(evaluated, Object::Null) {
            panic!("object is not Null {}", evaluated);
        }
    }
}
