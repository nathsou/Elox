use super::Interpreter;
use super::Stmt;
use crate::parser::{expressions::Expr, expressions::FuncExpr, IdentifierHandle, IdentifierUse};
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::fmt;


pub enum LexicalScopeResolutionError {
    VariableUsedInItsInitializer,
    DuplicateVariableDeclaration,
    ReturnKeywordOutsideFunction,
}

pub type LexicalScopeResolutionResult = Result<(), LexicalScopeResolutionError>;

#[derive(Clone, Copy, Debug)]
pub enum IdentifierStatus {
    Undeclared,
    Declared,
    Defined,
}

#[derive(Clone, Copy, Debug)]
pub enum FunctionType {
    Outside,
    Function,
}

pub struct Resolver<'a> {
    scopes: Vec<FnvHashMap<IdentifierHandle, IdentifierStatus>>,
    interpreter: &'a mut Interpreter,
    pub func_type: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Resolver {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            func_type: FunctionType::Outside,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FnvHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: IdentifierHandle) -> LexicalScopeResolutionResult {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let len = self.scopes.len();

        match self.scopes[len - 1].entry(identifier) {
            Entry::Vacant(v) => {
                v.insert(IdentifierStatus::Declared);
                Ok(())
            }
            Entry::Occupied(_) => Err(LexicalScopeResolutionError::DuplicateVariableDeclaration),
        }
    }

    fn define(&mut self, identifier: IdentifierHandle) {
        if self.scopes.is_empty() {
            return;
        }

        let len = self.scopes.len();

        self.scopes[len - 1].insert(identifier, IdentifierStatus::Defined);
    }

    fn get_scoped_identifier_status(&self, identifier: &IdentifierHandle) -> IdentifierStatus {
        if let Some(scope) = self.scopes.last() {
            if let Some(status) = scope.get(identifier) {
                return *status;
            }
        }

        IdentifierStatus::Undeclared
    }

    fn resolve_local(&mut self, identifier: &IdentifierUse) {
        for i in 0..self.scopes.len() {
            if self.scopes[i].contains_key(&identifier.name) {
                self.interpreter
                    .resolve(identifier.use_handle, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        func: &FuncExpr,
        type_: FunctionType,
    ) -> LexicalScopeResolutionResult {
        let enclosing_func_type = self.func_type;
        self.func_type = type_;
        self.begin_scope();
        for &param in func.params.iter() {
            self.declare(param.name)?;
            self.define(param.name);
        }

        for stmt in &func.body {
            stmt.resolve(self)?;
        }
        self.end_scope();
        self.func_type = enclosing_func_type;
        Ok(())
    }

    pub fn resolve(&mut self, statements: &Vec<Stmt>) -> LexicalScopeResolutionResult {
        for stmt in statements {
            stmt.resolve(self)?;
        }

        Ok(())
    }
}

pub trait LexicallyScoped {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult;
}

impl LexicallyScoped for Stmt {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult {
        match self {
            Stmt::Block(block) => {
                resolver.begin_scope();
                for stmt in &block.stmts {
                    stmt.resolve(resolver)?;
                }
                resolver.end_scope();
                Ok(())
            }
            Stmt::VarDecl(decl) => {
                resolver.declare(decl.identifier.name)?;
                if let Some(init) = &decl.initializer {
                    init.resolve(resolver)?;
                }
                resolver.define(decl.identifier.name);
                Ok(())
            }
            Stmt::Expr(stmt) => stmt.expr.resolve(resolver),
            Stmt::If(if_stmt) => {
                if_stmt.condition.resolve(resolver)?;
                if_stmt.then_branch.resolve(resolver)?;
                if let Some(else_branch) = &if_stmt.else_branch {
                    else_branch.resolve(resolver)?;
                }
                Ok(())
            }
            Stmt::Print(print_stmt) => print_stmt.value.resolve(resolver),
            Stmt::Return(ret_stmt) => {

                if let FunctionType::Outside = resolver.func_type {
                    return Err(LexicalScopeResolutionError::ReturnKeywordOutsideFunction);
                }

                if let Some(expr) = &ret_stmt.value {
                    expr.resolve(resolver)?;
                }
                Ok(())
            }
            Stmt::While(while_stmt) => {
                while_stmt.condition.resolve(resolver)?;
                while_stmt.body.resolve(resolver)?;
                Ok(())
            }
        }
    }
}

impl LexicallyScoped for Expr {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult {
        match self {
            Expr::Var(expr) => match resolver.get_scoped_identifier_status(&expr.identifier.name) {
                IdentifierStatus::Declared => {
                    Err(LexicalScopeResolutionError::VariableUsedInItsInitializer)
                }
                _ => {
                    resolver.resolve_local(&expr.identifier);
                    Ok(())
                }
            },
            Expr::Assign(assignment) => {
                assignment.expr.resolve(resolver)?;
                resolver.resolve_local(&assignment.identifier);
                Ok(())
            }
            Expr::Func(func) => {
                if let Some(identifier) = func.name {
                    resolver.declare(identifier.name)?;
                    resolver.define(identifier.name);

                    resolver.resolve_function(func, FunctionType::Function)?;
                }

                Ok(())
            }
            Expr::Binary(bin) => {
                bin.left.resolve(resolver)?;
                bin.right.resolve(resolver)?;
                Ok(())
            }
            Expr::Call(call) => {
                call.callee.resolve(resolver)?;
                for arg in &call.args {
                    arg.resolve(resolver)?;
                }
                Ok(())
            }
            Expr::Grouping(group) => group.expression.resolve(resolver),
            Expr::Literal(_) => Ok(()),
            Expr::Logical(op) => {
                op.left.resolve(resolver)?;
                op.right.resolve(resolver)?;
                Ok(())
            }
            Expr::Unary(unary) => unary.right.resolve(resolver),
        }
    }
}

impl fmt::Display for LexicalScopeResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalScopeResolutionError::VariableUsedInItsInitializer => {
                write!(f, "Cannot read local variable in its own initializer")
            }
            LexicalScopeResolutionError::DuplicateVariableDeclaration => {
                write!(f, "Duplicate variable declaration")
            }
            LexicalScopeResolutionError::ReturnKeywordOutsideFunction => {
                write!(f, "return keyword found outside of a function body")
            }
        }
    }
}
