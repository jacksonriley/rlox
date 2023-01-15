use crate::ast::{BinaryOp, Expr, Literal, UnaryOp};
use thiserror::Error;

pub(crate) struct Interpreter {}

impl Interpreter {
    pub fn interpret(&mut self, expr: Expr) -> Result<String, RuntimeError> {
        let lit = self.evaluate(expr)?;
        Ok(self.evaluate_literal(lit))
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Literal, RuntimeError> {
        match expr {
            Expr::Literal(l) => Ok(l),
            Expr::Unary(op, right) => self.evaluate_unary(op, *right),
            Expr::Binary(left, op, right) => self.evaluate_binary(*left, op, *right),
            Expr::Grouping(e) => self.evaluate(*e),
        }
    }

    fn evaluate_literal(&mut self, lit: Literal) -> String {
        match lit {
            Literal::Number(n) => n.to_string(),
            Literal::Str(s) => s,
            Literal::False => "false".into(),
            Literal::True => "true".into(),
            Literal::Nil => "nil".into(),
        }
    }

    fn evaluate_unary(&mut self, op: UnaryOp, right: Expr) -> Result<Literal, RuntimeError> {
        let evaluated_right = self.evaluate(right)?;
        match op {
            UnaryOp::Bang => Ok(Self::is_falsy(evaluated_right)),
            UnaryOp::Minus => match evaluated_right {
                Literal::Number(n) => Ok(Literal::Number(-n)),
                _ => Err(RuntimeError::InvalidUnaryOperand(evaluated_right)),
            },
        }
    }

    fn evaluate_binary(
        &mut self,
        left: Expr,
        op: BinaryOp,
        right: Expr,
    ) -> Result<Literal, RuntimeError> {
        let eval_left = self.evaluate(left)?;
        let eval_right = self.evaluate(right)?;
        match op {
            BinaryOp::Minus => Self::evaluate_binary_number_op(eval_left, eval_right, |l, r| l - r),
            BinaryOp::Slash => Self::evaluate_binary_number_op(eval_left, eval_right, |l, r| l / r),
            BinaryOp::Star => Self::evaluate_binary_number_op(eval_left, eval_right, |l, r| l * r),
            BinaryOp::Plus => match (&eval_left, &eval_right) {
                (Literal::Number(num_l), Literal::Number(num_r)) => {
                    Ok(Literal::Number(num_l + num_r))
                }
                (Literal::Str(str_l), Literal::Str(str_r)) => {
                    Ok(Literal::Str(format!("{str_l}{str_r}")))
                }
                _ => Err(RuntimeError::InvalidBinaryOperands(eval_left, eval_right)),
            },
            BinaryOp::Greater => {
                Self::evaluate_binary_comparison_op(eval_left, eval_right, |l, r| l > r)
            }
            BinaryOp::GreaterEqual => {
                Self::evaluate_binary_comparison_op(eval_left, eval_right, |l, r| l >= r)
            }
            BinaryOp::Less => {
                Self::evaluate_binary_comparison_op(eval_left, eval_right, |l, r| l < r)
            }
            BinaryOp::LessEqual => {
                Self::evaluate_binary_comparison_op(eval_left, eval_right, |l, r| l <= r)
            }
            BinaryOp::BangEqual => Self::evaluate_binary_equality_op(eval_left, eval_right, |v| !v),
            BinaryOp::EqualEqual => Self::evaluate_binary_equality_op(eval_left, eval_right, |v| v),
        }
    }

    fn evaluate_binary_number_op<F>(
        left: Literal,
        right: Literal,
        op: F,
    ) -> Result<Literal, RuntimeError>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        match (&left, &right) {
            (Literal::Number(num_l), Literal::Number(num_r)) => {
                Ok(Literal::Number(op(*num_l, *num_r)))
            }
            _ => Err(RuntimeError::InvalidBinaryOperands(left, right)),
        }
    }

    fn evaluate_binary_comparison_op<F>(
        left: Literal,
        right: Literal,
        op: F,
    ) -> Result<Literal, RuntimeError>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        match (&left, &right) {
            (Literal::Number(num_l), Literal::Number(num_r)) => Ok(if op(*num_l, *num_r) {
                Literal::True
            } else {
                Literal::False
            }),
            _ => Err(RuntimeError::InvalidBinaryOperands(left, right)),
        }
    }

    fn evaluate_binary_equality_op<F>(
        left: Literal,
        right: Literal,
        op: F,
    ) -> Result<Literal, RuntimeError>
    where
        F: FnOnce(bool) -> bool,
    {
        Ok(if op(left == right) {
            Literal::True
        } else {
            Literal::False
        })
    }

    fn is_falsy(lit: Literal) -> Literal {
        // False and nil are falsy - everything else is truthy
        match lit {
            Literal::False | Literal::Nil => Literal::True,
            _ => Literal::False,
        }
    }
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Operand {0:?} is not valid for a unary operation")]
    InvalidUnaryOperand(Literal),
    #[error("Operands ({0:?}, {1:?}) are not valid for a binary operation")]
    InvalidBinaryOperands(Literal, Literal),
}
