use super::statements::Stmt;
use super::IdentifierUse;
use crate::scanner::token::{token_type::TokenType, Position};
use std::fmt;

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
}

#[derive(Clone)]
pub struct ExprCtx {
    pub expr: Expr,
    pos: Position,
}

impl ExprCtx {
    pub fn new(expr: Expr, pos: Position) -> ExprCtx {
        ExprCtx { expr, pos }
    }

    pub fn pos(&self) -> Position {
        self.pos.clone()
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
        write!(f, "{}", 
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
        match token_type {
            TokenType::Minus => Some(BinaryOperator::Minus),
            TokenType::Plus => Some(BinaryOperator::Plus),
            TokenType::Slash => Some(BinaryOperator::Slash),
            TokenType::Star => Some(BinaryOperator::Star),
            TokenType::Percent => Some(BinaryOperator::Percent),
            TokenType::BangEqual => Some(BinaryOperator::BangEqual),
            TokenType::Greater => Some(BinaryOperator::Greater),
            TokenType::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            TokenType::Less => Some(BinaryOperator::LessEqual),
            TokenType::PlusEqual => Some(BinaryOperator::Plus),
            TokenType::PlusPlus => Some(BinaryOperator::Plus),
            TokenType::MinusEqual => Some(BinaryOperator::Minus),
            TokenType::MinusMinus => Some(BinaryOperator::Minus),
            TokenType::StarEqual => Some(BinaryOperator::Star),
            TokenType::PercentEqual => Some(BinaryOperator::Percent),
            TokenType::SlashEqual => Some(BinaryOperator::Slash),
            _ => None,
        }
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
            Expr::Binary(Box::new(
                BinaryExpr {
                    left,
                    operator: BinaryOperatorCtx { pos: pos.clone(), op: operator },
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
        write!(f, "{}", 
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
pub struct FuncExpr {
    pub name: Option<IdentifierUse>,
    pub params: Vec<IdentifierUse>,
    pub body: Vec<Stmt>,
}

#[derive(Clone)]
pub struct ExprWithCtxt<T> {
    pub expr: T,
    pub pos: Position,
}

impl FuncExpr {
    pub fn new(pos: Position, name: Option<IdentifierUse>, params: Vec<IdentifierUse>, body: Vec<Stmt>) -> ExprCtx {
        let expr = Expr::Func(FuncExpr { name, params, body });
        ExprCtx::new(expr, pos)
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