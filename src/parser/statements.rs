use super::IdentifierUse;
use super::Position;
use crate::parser::expressions::{ExprCtx, FuncExpr, VarExpr};

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
    pub expr: ExprCtx,
}

impl ExprStmt {
    pub fn to_stmt(expr: ExprCtx) -> Stmt {
        Stmt::Expr(ExprStmt { expr })
    }
}

#[derive(Clone)]
pub struct PrintStmt {
    pub value: ExprCtx,
}

impl PrintStmt {
    pub fn to_stmt(value: ExprCtx) -> Stmt {
        Stmt::Print(PrintStmt { value })
    }
}

#[derive(Clone)]
pub struct VarDeclStmt {
    pub identifier: IdentifierUse,
    pub initializer: Option<ExprCtx>,
}

impl VarDeclStmt {
    pub fn to_stmt(identifier: IdentifierUse, initializer: Option<ExprCtx>) -> Stmt {
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
    pub condition: ExprCtx,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn to_stmt(condition: ExprCtx, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
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
    pub condition: ExprCtx,
    pub body: Box<Stmt>,
}

impl WhileStmt {
    pub fn to_stmt(condition: ExprCtx, body: Stmt) -> Stmt {
        Stmt::While(WhileStmt {
            condition,
            body: Box::new(body),
        })
    }
}

#[derive(Clone)]
pub struct ReturnStmt {
    pub value: Option<ExprCtx>,
    pub pos: Position,
}

impl ReturnStmt {
    pub fn to_stmt(value: Option<ExprCtx>, pos: Position) -> Stmt {
        Stmt::Return(ReturnStmt { value, pos })
    }
}

#[derive(Clone)]
pub struct ClassDeclStmt {
    pub identifier: IdentifierUse,
    pub superclass: Option<VarExpr>,
    pub methods: Vec<FuncExpr>,
    pub pos: Position,
}

impl ClassDeclStmt {
    pub fn to_stmt(
        pos: Position,
        identifier: IdentifierUse,
        superclass: Option<VarExpr>,
        methods: Vec<FuncExpr>,
    ) -> Stmt {
        Stmt::ClassDecl(ClassDeclStmt {
            identifier,
            superclass,
            methods,
            pos,
        })
    }
}
