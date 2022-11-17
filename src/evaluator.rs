use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Identifier, Literal, Program, Statement},
    object::{Environment, Func, Object},
};

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn new_with_env(env: Environment) -> Evaluator {
        Evaluator {
            environment: Rc::new(RefCell::new(env)),
        }
    }

    pub fn eval(&self, program: &Program) -> Object {
        let mut result = Object::Null;
        for statement in &program.statements {
            result = self.eval_statement(statement);

            match result {
                Object::Return(return_value) => return *return_value,
                Object::Error(_) => return result,
                _ => continue,
            };
        }

        result
    }

    fn eval_statement(&self, statement: &Statement) -> Object {
        match statement {
            Statement::Let(identifier, expression) => {
                let val = self.eval_expression(expression);
                let name = &identifier.to_string();
                self.environment.borrow_mut().set(name, &val);
                val
            }
            Statement::Return(expression) => {
                let evaluated = self.eval_expression(expression);
                Object::Return(Box::new(evaluated))
            }
            Statement::Expression(expression) => self.eval_expression(expression),
        }
    }

    fn eval_statements(&self, block: &BlockStatement) -> Object {
        let mut result = Object::Null;
        for statement in block.statements.iter() {
            result = self.eval_statement(&statement);

            match result {
                Object::Return(_) => return result,
                Object::Error(_) => return result,
                _ => continue,
            };
        }
        result
    }

    fn eval_expression(&self, expression: &Expression) -> Object {
        match expression {
            Expression::Lit(literal) => match literal {
                Literal::IntegerLiteral(v) => Object::Integer(*v),
                Literal::BooleanLiteral(v) => Object::Boolean(*v),
                Literal::IdentLiteral(ident) => self.eval_identifier(&ident.to_string()),
            },
            Expression::Prefix {
                operator, right, ..
            } => self.eval_prefix_expression(&operator.to_string(), self.eval_expression(right)),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => {
                let left_evaluated = self.eval_expression(left);
                let right_evaluated = self.eval_expression(right);

                self.eval_infix_expression(&operator.to_string(), left_evaluated, right_evaluated)
            }
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => self.eval_if_expression(condition, consequence, alternative),
            Expression::Function {
                parameters, body, ..
            } => self.eval_function_expression(parameters, body),
            Expression::Call {
                function,
                arguments,
                ..
            } => {
                let function = self.eval_expression(function);
                if function.is_error() {
                    return function;
                }

                let args = self.eval_expressions(arguments);
                if args.len() == 1 && args[0].is_error() {
                    return args[0].clone();
                }
                return self.apply_function(function, args);
            }
        }
    }

    fn eval_expressions(&self, expressions: &Vec<Box<Expression>>) -> Vec<Object> {
        let mut result = Vec::new();
        for expression in expressions {
            let evaluated = self.eval_expression(&expression);
            if evaluated.is_error() {
                return result;
            }
            result.push(evaluated);
        }
        result
    }

    fn apply_function(&self, function_object: Object, args: Vec<Object>) -> Object {
        match function_object {
            Object::Function(Func {
                body,
                parameters,
                env,
            }) => {
                let extended_env = self.extend_function_env(&parameters, args, env);
                let evaluator = Evaluator::new_with_env(extended_env);
                let evaluated = evaluator.eval_statements(&body);
                match evaluated {
                    Object::Return(v) => *v,
                    _ => evaluated,
                }
            }
            _ => Object::Error(format!("not a function: {}", function_object)),
        }
    }

    fn extend_function_env(
        &self,
        parameters: &Vec<Identifier>,
        args: Vec<Object>,
        env: Rc<RefCell<Environment>>,
    ) -> Environment {
        let mut new_env = Environment::new_enclose_environment(env);

        for (i, parameter) in parameters.iter().enumerate() {
            new_env.set(&parameter.to_string(), &args[i].clone());
        }
        new_env
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_expression(right),
            "-" => self.eval_minus_prefix_operator_excepssion(right),
            _ => Object::Error(format!("unknown operator: {}{}", operator, right.to_type())),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                self.eval_boolean_infix_expression(operator, *l, *r)
            }
            _ => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.to_type(),
                operator,
                right.to_type()
            )),
        }
    }

    fn eval_bang_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }

    fn eval_minus_prefix_operator_excepssion(&self, right: Object) -> Object {
        match right {
            Object::Integer(v) => Object::Integer(-v),
            _ => Object::Error(format!("unknown operator: -{}", right.to_type())),
        }
    }

    fn eval_integer_infix_expression(&self, operator: &str, l: i32, r: i32) -> Object {
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

    fn eval_boolean_infix_expression(&self, operator: &str, l: bool, r: bool) -> Object {
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
        &self,
        condition: &Expression,
        consequence: &BlockStatement,
        alternative: &Option<BlockStatement>,
    ) -> Object {
        let cond = self.eval_expression(condition);
        if is_truthy(cond) {
            return self.eval_statements(consequence);
        }

        match alternative {
            Some(statement) => self.eval_statements(statement),
            None => Object::Null,
        }
    }

    fn eval_identifier(&self, ident: &str) -> Object {
        self.environment
            .borrow_mut()
            .get(ident)
            .unwrap_or(Object::Error(format!(
                "identifier not found: {}",
                ident.to_string()
            )))
    }

    fn eval_function_expression(
        &self,
        parameters: &Vec<Identifier>,
        body: &BlockStatement,
    ) -> Object {
        Object::Function(Func {
            parameters: parameters.clone(),
            body: body.clone(),
            env: self.environment.clone(),
        })
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

    use super::Evaluator;

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
            ("foobar", "identifier not found: foobar"),
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

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_function_application() {
        let tests =
            vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            ("let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);", 4)
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated, test.1);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        match evaluated {
            Object::Function(func) => {
                if let Some(ident) = func.parameters.get(0) {
                    if ident.to_string() != "x" {
                        panic!("parameter is not 'x'. {}", ident);
                    }
                }

                let expected_body = "(x + 2)";

                if func.body.to_string() != expected_body {
                    panic!("body is not {}. {}", expected_body, func.body.to_string());
                }
            }
            _ => panic!("object is not function. {}", evaluated),
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("error parse program");
        let evaluator = Evaluator::new();
        evaluator.eval(&program)
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
