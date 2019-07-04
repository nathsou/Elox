use super::statements::Stmt;
use super::IdentifierUse;

#[derive(Clone)]
pub enum Literal {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
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

#[derive(Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: BinaryOperator, right: Expr) -> Expr {
        Expr::Binary(Box::new(BinaryExpr {
            left,
            operator,
            right,
        }))
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOperator, right: Expr) -> Expr {
        Expr::Unary(Box::new(UnaryExpr { operator, right }))
    }
}

#[derive(Clone)]
pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Expr {
        Expr::Grouping(Box::new(GroupingExpr { expression }))
    }
}

#[derive(Clone)]
pub struct VarExpr {
    pub identifier: IdentifierUse,
}

impl VarExpr {
    pub fn new(identifier: IdentifierUse) -> Expr {
        Expr::Var(VarExpr { identifier })
    }
}

#[derive(Clone)]
pub struct AssignExpr {
    pub identifier: IdentifierUse,
    pub expr: Expr,
}

impl AssignExpr {
    pub fn new(identifier: IdentifierUse, expr: Expr) -> Expr {
        Expr::Assign(Box::new(AssignExpr { identifier, expr }))
    }
}

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Clone)]
pub struct LogicalExpr {
    pub left: Expr,
    pub operator: LogicalOperator,
    pub right: Expr,
}

impl LogicalExpr {
    pub fn new(left: Expr, operator: LogicalOperator, right: Expr) -> Expr {
        Expr::Logical(Box::new(LogicalExpr {
            left,
            operator,
            right,
        }))
    }
}

#[derive(Clone)]
pub struct CallExpr {
    pub callee: Expr,
    pub args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(CallExpr { callee, args }))
    }
}

#[derive(Clone)]
pub struct FuncExpr {
    pub name: Option<IdentifierUse>,
    pub params: Vec<IdentifierUse>,
    pub body: Vec<Stmt>,
}

impl FuncExpr {
    pub fn new(name: Option<IdentifierUse>, params: Vec<IdentifierUse>, body: Vec<Stmt>) -> Expr {
        Expr::Func(FuncExpr { name, params, body })
    }
}

#[derive(Clone)]
pub struct GetExpr {
    pub property: IdentifierUse,
    pub object: Expr,
}

impl GetExpr {
    pub fn new(property: IdentifierUse, object: Expr) -> Expr {
        Expr::Get(Box::new(GetExpr { property, object }))
    }
}

#[derive(Clone)]
pub struct SetExpr {
    pub property: IdentifierUse,
    pub object: Expr,
    pub value: Expr,
}

impl SetExpr {
    pub fn new(property: IdentifierUse, object: Expr, value: Expr) -> Expr {
        Expr::Set(Box::new(SetExpr {
            property,
            object,
            value,
        }))
    }
}

#[derive(Clone)]
pub struct ThisExpr {
    pub identifier: IdentifierUse
}