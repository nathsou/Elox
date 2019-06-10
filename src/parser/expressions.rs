use super::statements::Stmt;
use super::IdentifierHandle;

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
    pub name: IdentifierHandle,
}

impl VarExpr {
    pub fn new(name: IdentifierHandle) -> Expr {
        Expr::Var(VarExpr { name })
    }
}

#[derive(Clone)]
pub struct AssignExpr {
    pub name: IdentifierHandle,
    pub expr: Expr,
}

impl AssignExpr {
    pub fn new(name: IdentifierHandle, expr: Expr) -> Expr {
        Expr::Assign(Box::new(AssignExpr { name, expr }))
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
    pub name: Option<IdentifierHandle>,
    pub params: Vec<IdentifierHandle>,
    pub body: Vec<Stmt>,
}

impl FuncExpr {
    pub fn new(
        name: Option<IdentifierHandle>,
        params: Vec<IdentifierHandle>,
        body: Vec<Stmt>,
    ) -> Expr {
        Expr::Func(FuncExpr { name, params, body })
    }
}
