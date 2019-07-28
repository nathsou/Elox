use super::statements::Stmt;
use super::{IdentifierHandle, IdentifierUse};
use crate::interpreter::lox_function::LoxFunctionParams;
use crate::interpreter::value::Value;
use crate::interpreter::{
    environment::Environment, eval::Eval, eval_result::EvalResult, Interpreter,
};
use crate::scanner::token::{token_type::TokenType, Position};
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum Literal {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Literal {
    pub fn new(pos: Position, literal: Literal) -> ExprCtx {
        ExprCtx::new(Expr::Literal(literal), pos)
    }
}

#[derive(Clone)]
pub enum Expr {
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Literal(Literal),
    Grouping(Box<GroupingExpr>),
    Var(VarExpr),
    Assign(Box<AssignExpr>),
    Logical(Box<LogicalExpr>),
    Call(Box<CallExpr>),
    Func(FuncExpr),
    Get(Box<GetExpr>),
    Set(Box<SetExpr>),
    This(ThisExpr),
    Super(SuperExpr),
    ArrayDeclExpr(Box<ArrayDeclExpr>),
}

#[derive(Clone)]
pub struct ExprCtx {
    pub expr: Expr,
    pub pos: Position,
}

impl ExprCtx {
    pub fn new(expr: Expr, pos: Position) -> ExprCtx {
        ExprCtx { expr, pos }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Minus,
    Plus,
    Slash,
    Star,
    Percent,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOperator::*;
        write!(
            f,
            "{}",
            match self {
                Minus => "-",
                Plus => "+",
                Slash => "/",
                Star => "*",
                Percent => "%",
                BangEqual => "!=",
                EqualEqual => "==",
                Greater => ">",
                GreaterEqual => ">=",
                Less => "<",
                LessEqual => "<=",
            }
        )
    }
}

impl BinaryOperator {
    pub fn from_token_type(token_type: &TokenType) -> Option<BinaryOperator> {
        Some(match token_type {
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Slash => BinaryOperator::Slash,
            TokenType::Star => BinaryOperator::Star,
            TokenType::Percent => BinaryOperator::Percent,
            TokenType::BangEqual => BinaryOperator::BangEqual,
            TokenType::Greater => BinaryOperator::Greater,
            TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenType::Less => BinaryOperator::LessEqual,
            TokenType::PlusEqual => BinaryOperator::Plus,
            TokenType::PlusPlus => BinaryOperator::Plus,
            TokenType::MinusEqual => BinaryOperator::Minus,
            TokenType::MinusMinus => BinaryOperator::Minus,
            TokenType::StarEqual => BinaryOperator::Star,
            TokenType::PercentEqual => BinaryOperator::Percent,
            TokenType::SlashEqual => BinaryOperator::Slash,
            _ => return None,
        })
    }
}

#[derive(Clone)]
pub struct BinaryOperatorCtx {
    pub op: BinaryOperator,
    pub pos: Position,
}

#[derive(Clone)]
pub struct BinaryExpr {
    pub left: ExprCtx,
    pub operator: BinaryOperatorCtx,
    pub right: ExprCtx,
}

impl BinaryExpr {
    pub fn new(pos: Position, left: ExprCtx, operator: BinaryOperator, right: ExprCtx) -> ExprCtx {
        ExprCtx::new(
            Expr::Binary(Box::new(BinaryExpr {
                left,
                operator: BinaryOperatorCtx {
                    pos: pos.clone(),
                    op: operator,
                },
                right,
            })),
            pos,
        )
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Minus => "-",
                UnaryOperator::Bang => "!",
            }
        )
    }
}

#[derive(Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: ExprCtx,
}

impl UnaryExpr {
    pub fn new(pos: Position, operator: UnaryOperator, right: ExprCtx) -> ExprCtx {
        let expr = Expr::Unary(Box::new(UnaryExpr { operator, right }));

        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct GroupingExpr {
    pub expression: ExprCtx,
}

impl GroupingExpr {
    pub fn new(pos: Position, expression: ExprCtx) -> ExprCtx {
        let expr = Expr::Grouping(Box::new(GroupingExpr { expression }));
        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct VarExpr {
    pub identifier: IdentifierUse,
}

impl VarExpr {
    pub fn new(pos: Position, identifier: IdentifierUse) -> ExprCtx {
        ExprCtx::new(Expr::Var(VarExpr { identifier }), pos)
    }
}

#[derive(Clone)]
pub struct AssignExpr {
    pub identifier: IdentifierUse,
    pub expr: ExprCtx,
}

impl AssignExpr {
    pub fn new(pos: Position, identifier: IdentifierUse, expr: ExprCtx) -> ExprCtx {
        let expr = Expr::Assign(Box::new(AssignExpr { identifier, expr }));
        ExprCtx::new(expr, pos)
    }
}

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Clone)]
pub struct LogicalExpr {
    pub left: ExprCtx,
    pub operator: LogicalOperator,
    pub right: ExprCtx,
}

impl LogicalExpr {
    pub fn new(pos: Position, left: ExprCtx, operator: LogicalOperator, right: ExprCtx) -> ExprCtx {
        let expr = Expr::Logical(Box::new(LogicalExpr {
            left,
            operator,
            right,
        }));

        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct CallExpr {
    pub callee: ExprCtx,
    pub args: Vec<ExprCtx>,
}

impl CallExpr {
    pub fn new(pos: Position, callee: ExprCtx, args: Vec<ExprCtx>) -> ExprCtx {
        let expr = Expr::Call(Box::new(CallExpr { callee, args }));
        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub enum FuncParam {
    Required(IdentifierUse),
    DefaultValued(IdentifierUse, ExprCtx),
    Rest(IdentifierUse),
}

#[derive(Clone)]
pub enum ContextLessFuncParam {
    Required(IdentifierHandle),
    DefaultValued(IdentifierHandle, Value),
    Rest(IdentifierHandle),
}

#[derive(Clone)]
pub struct FuncExpr {
    pub name: Option<IdentifierUse>,
    pub params: Option<Vec<FuncParam>>,
    pub body: Vec<Stmt>,
    pub pos: Position,
}

impl FuncParam {
    pub fn identifier(&self) -> &IdentifierUse {
        use FuncParam::*;
        match self {
            Required(name) | DefaultValued(name, _) | Rest(name) => name,
        }
    }

    pub fn to_context_less(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
    ) -> EvalResult<ContextLessFuncParam> {
        use ContextLessFuncParam::*;
        match self {
            FuncParam::Required(id) => Ok(Required(id.name)),
            FuncParam::DefaultValued(id, expr) => {
                Ok(DefaultValued(id.name, interpreter.eval(env, &expr)?))
            }
            FuncParam::Rest(id) => Ok(Rest(id.name)),
        }
    }
}

impl ContextLessFuncParam {
    pub fn is_required(&self) -> bool {
        if let ContextLessFuncParam::Required(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_rest(&self) -> bool {
        if let ContextLessFuncParam::Rest(_) = self {
            true
        } else {
            false
        }
    }
}

impl FuncExpr {
    pub fn new(
        pos: Position,
        name: Option<IdentifierUse>,
        params: Option<Vec<FuncParam>>,
        body: Vec<Stmt>,
    ) -> ExprCtx {
        let expr = Expr::Func(FuncExpr {
            name,
            params,
            body,
            pos,
        });
        ExprCtx::new(expr, pos)
    }

    pub fn context_less_params(
        &self,
        interpreter: &Interpreter,
        env: &Environment,
    ) -> EvalResult<LoxFunctionParams> {
        if let Some(params) = &self.params {
            Ok(Some(Rc::new(
                params
                    .iter()
                    .map(|fp| fp.to_context_less(interpreter, env))
                    .collect::<EvalResult<Vec<ContextLessFuncParam>>>()?,
            )))
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone)]
pub struct GetExpr {
    pub property: IdentifierUse,
    pub object: ExprCtx,
}

impl GetExpr {
    pub fn new(pos: Position, property: IdentifierUse, object: ExprCtx) -> ExprCtx {
        let expr = Expr::Get(Box::new(GetExpr { property, object }));
        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct SetExpr {
    pub property: IdentifierUse,
    pub object: ExprCtx,
    pub value: ExprCtx,
}

impl SetExpr {
    pub fn new(pos: Position, property: IdentifierUse, object: ExprCtx, value: ExprCtx) -> ExprCtx {
        let expr = Expr::Set(Box::new(SetExpr {
            property,
            object,
            value,
        }));

        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct SuperExpr {
    pub identifier: IdentifierUse,
    pub method: IdentifierUse,
}

impl SuperExpr {
    pub fn new(pos: Position, identifier: IdentifierUse, method: IdentifierUse) -> ExprCtx {
        let expr = Expr::Super(SuperExpr { identifier, method });
        ExprCtx::new(expr, pos)
    }
}

#[derive(Clone)]
pub struct ThisExpr {
    pub identifier: IdentifierUse,
}

impl ThisExpr {
    pub fn new(pos: Position, identifier: IdentifierUse) -> ExprCtx {
        ExprCtx::new(Expr::This(ThisExpr { identifier }), pos)
    }
}

#[derive(Clone)]
pub struct ArrayDeclExpr {
    pub values: Vec<ExprCtx>,
}

impl ArrayDeclExpr {
    pub fn new(pos: Position, values: Vec<ExprCtx>) -> ExprCtx {
        ExprCtx::new(Expr::ArrayDeclExpr(Box::new(ArrayDeclExpr { values })), pos)
    }
}
