use super::IdentifierUse;
use crate::parser::expressions::{Expr, FuncExpr};

#[derive(Clone)]
pub enum Stmt {
    Print(PrintStmt),
    Expr(ExprStmt),
    VarDecl(VarDeclStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    ClassDecl(ClassDeclStmt),
}

// expression statement aka an expression followed by ;
#[derive(Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

impl ExprStmt {
    pub fn to_stmt(expr: Expr) -> Stmt {
        Stmt::Expr(ExprStmt { expr })
    }
}

#[derive(Clone)]
pub struct PrintStmt {
    pub value: Expr,
}

impl PrintStmt {
    pub fn to_stmt(value: Expr) -> Stmt {
        Stmt::Print(PrintStmt { value })
    }
}

#[derive(Clone)]
pub struct VarDeclStmt {
    pub identifier: IdentifierUse,
    pub initializer: Option<Expr>,
}

impl VarDeclStmt {
    pub fn to_stmt(identifier: IdentifierUse, initializer: Option<Expr>) -> Stmt {
        Stmt::VarDecl(VarDeclStmt {
            identifier,
            initializer,
        })
    }
}

#[derive(Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn to_stmt(stmts: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmt { stmts })
    }
}

#[derive(Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn to_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
        Stmt::If(IfStmt {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: if let Some(else_br) = else_branch {
                Some(Box::new(else_br))
            } else {
                None
            },
        })
    }
}

#[derive(Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl WhileStmt {
    pub fn to_stmt(condition: Expr, body: Stmt) -> Stmt {
        Stmt::While(WhileStmt {
            condition,
            body: Box::new(body),
        })
    }
}

#[derive(Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

impl ReturnStmt {
    pub fn to_stmt(value: Option<Expr>) -> Stmt {
        Stmt::Return(ReturnStmt { value })
    }
}

#[derive(Clone)]
pub struct ClassDeclStmt {
    pub identifier: IdentifierUse,
    pub methods: Vec<FuncExpr>
}

impl ClassDeclStmt {
    pub fn to_stmt(identifier: IdentifierUse, methods: Vec<FuncExpr>) -> Stmt {
        Stmt::ClassDecl(ClassDeclStmt {
            identifier, methods
        })
    }
}