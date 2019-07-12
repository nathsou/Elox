use super::Interpreter;
use super::Stmt;
use crate::parser::{
    expressions::{Expr, ExprCtx, FuncExpr, VarExpr, FuncParam},
    Identifier, IdentifierHandle, IdentifierNames, IdentifierUse,
};
use crate::scanner::scanner_result::ErrorPosition;
use crate::scanner::token::Position;
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use std::rc::Rc;

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
    Initializer,
    Method,
}

#[derive(Clone, Copy, Debug)]
pub enum ClassType {
    NotAClass,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    scopes: Vec<FnvHashMap<IdentifierHandle, IdentifierStatus>>,
    interpreter: &'a mut Interpreter,
    pub func_type: FunctionType,
    pub class_type: ClassType,
    pub class_name: Option<IdentifierHandle>,
    names: Rc<IdentifierNames>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter, names: &Rc<IdentifierNames>) -> Resolver<'a> {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            func_type: FunctionType::Outside,
            class_type: ClassType::NotAClass,
            class_name: None,
            names: Rc::clone(names),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FnvHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: IdentifierUse) -> LexicalScopeResolutionResult {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let len = self.scopes.len();

        match self.scopes[len - 1].entry(identifier.name) {
            Entry::Vacant(v) => {
                v.insert(IdentifierStatus::Declared);
                Ok(())
            }
            Entry::Occupied(_) => Err(LexicalScopeResolutionError::DuplicateVariableDeclaration(
                identifier.pos,
                self.name(identifier.name),
            )),
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
        if let None = func.name {
            if let FunctionType::Method = type_ {
                return Err(LexicalScopeResolutionError::AnonymousClassMethod(
                    func.pos,
                    self.name(self.class_name.unwrap()),
                ));
            }
        }

        let enclosing_func_type = self.func_type;
        self.func_type = type_;

        self.begin_scope();

        if let Some(params) = &func.params {
            for param in params.iter() {
                let identifier = param.identifier();
                self.declare(*identifier)?;
                self.define(identifier.name);
                if let FuncParam::DefaultValued(_, expr) = param {
                    expr.resolve(self)?;
                }
            }
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

    pub fn name(&self, handle: IdentifierHandle) -> std::string::String {
        self.names[handle].clone()
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
                resolver.declare(decl.identifier)?;
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
                match resolver.func_type {
                    FunctionType::Outside => {
                        return Err(LexicalScopeResolutionError::ReturnKeywordOutsideFunction(
                            ret_stmt.pos,
                        ));
                    }
                    FunctionType::Initializer => {
                        return Err(LexicalScopeResolutionError::CannotReturnInsideInitializer(
                            ret_stmt.pos,
                            resolver.name(resolver.class_name.unwrap()),
                        ));
                    }
                    _ => {}
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
            Stmt::ClassDecl(class_decl) => {
                let enclosing_class = resolver.class_type;
                resolver.class_type = ClassType::Class;
                resolver.class_name = Some(class_decl.identifier.name);

                resolver.declare(class_decl.identifier)?;
                resolver.define(class_decl.identifier.name);

                if let Some(superclass) = &class_decl.superclass {
                    resolver.class_type = ClassType::Subclass;
                    if superclass.identifier.name != class_decl.identifier.name {
                        superclass.resolve(resolver)?;
                    } else {
                        return Err(LexicalScopeResolutionError::ClassCannotInheritFromItself(
                            class_decl.identifier.pos,
                            resolver.name(class_decl.identifier.name),
                        ));
                    }

                    resolver.begin_scope();
                    resolver.define(Identifier::super_());
                }

                resolver.begin_scope();
                resolver.define(Identifier::this());

                for method in &class_decl.methods {
                    if let Some(method_handle) = method.name {
                        let func_type = if method_handle.name == Identifier::init() {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Method
                        };
                        resolver.resolve_function(&method, func_type)?;
                    } else {
                        return Err(LexicalScopeResolutionError::AnonymousClassMethod(
                            method.pos,
                            resolver.name(resolver.class_name.unwrap()),
                        ));
                    }
                }

                resolver.end_scope();
                if let Some(_) = class_decl.superclass {
                    resolver.end_scope();
                }

                resolver.class_type = enclosing_class;
                resolver.class_name = None;

                Ok(())
            }
        }
    }
}

impl LexicallyScoped for VarExpr {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult {
        match resolver.get_scoped_identifier_status(&self.identifier.name) {
            IdentifierStatus::Declared => {
                Err(LexicalScopeResolutionError::VariableUsedInItsInitializer(
                    self.identifier.pos,
                    resolver.name(self.identifier.name),
                ))
            }
            _ => {
                resolver.resolve_local(&self.identifier);
                Ok(())
            }
        }
    }
}

impl LexicallyScoped for ExprCtx {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult {
        match &self.expr {
            Expr::Var(expr) => expr.resolve(resolver),
            Expr::Assign(assignment) => {
                assignment.expr.resolve(resolver)?;
                resolver.resolve_local(&assignment.identifier);
                Ok(())
            }
            Expr::Func(func) => {
                if let Some(identifier) = func.name {
                    resolver.declare(identifier)?;
                    resolver.define(identifier.name);
                }

                resolver.resolve_function(&func, FunctionType::Function)?;

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
            Expr::Get(get) => get.object.resolve(resolver),
            Expr::Set(set) => {
                set.object.resolve(resolver)?;
                set.value.resolve(resolver)?;
                Ok(())
            }
            Expr::This(this_expr) => {
                if let ClassType::NotAClass = resolver.class_type {
                    return Err(LexicalScopeResolutionError::CannotUseThisOutsideOfAClass(
                        this_expr.identifier.pos,
                    ));
                }

                resolver.resolve_local(&this_expr.identifier);
                Ok(())
            }
            Expr::Super(super_expr) => match resolver.class_type {
                ClassType::NotAClass => {
                    return Err(LexicalScopeResolutionError::CannotUseSuperOutsideAclass(
                        super_expr.identifier.pos,
                    ))
                }
                ClassType::Class => {
                    return Err(
                        LexicalScopeResolutionError::CannotUseSuperInAClassWithNoSuperClass(
                            super_expr.identifier.pos,
                            resolver.name(resolver.class_name.unwrap()),
                        ),
                    )
                }
                ClassType::Subclass => {
                    resolver.resolve_local(&super_expr.identifier);
                    return Ok(());
                }
            },
        }
    }
}

pub enum LexicalScopeResolutionError {
    VariableUsedInItsInitializer(Position, String),
    DuplicateVariableDeclaration(Position, String),
    ReturnKeywordOutsideFunction(Position),
    AnonymousClassMethod(Position, String),
    CannotUseThisOutsideOfAClass(Position),
    CannotReturnInsideInitializer(Position, String),
    ClassCannotInheritFromItself(Position, String),
    CannotUseSuperOutsideAclass(Position),
    CannotUseSuperInAClassWithNoSuperClass(Position, String),
}

impl fmt::Display for LexicalScopeResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LexicalScopeResolutionError::*;
        match self {
            VariableUsedInItsInitializer(_, name) => write!(
                f,
                "Cannot read local variable '{}' in its own initializer",
                name
            ),
            DuplicateVariableDeclaration(_, name) => {
                write!(f, "Duplicate variable declaration for '{}'", name)
            }
            ReturnKeywordOutsideFunction(_) => {
                write!(f, "'return' keyword found outside of a function body")
            }
            AnonymousClassMethod(_, name) => {
                write!(f, "A class method cannot be anonymous: in class '{}'", name)
            }
            CannotUseThisOutsideOfAClass(_) => {
                write!(f, "Cannot use the 'this' keyword outside of a class")
            }
            CannotReturnInsideInitializer(_, name) => write!(
                f,
                "Cannot use the 'return' keyword inside an initializer, in class '{}'",
                name
            ),
            ClassCannotInheritFromItself(_, name) => {
                write!(f, "Class '{}' cannot inherit from itself", name)
            }
            CannotUseSuperOutsideAclass(_) => write!(f, "Cannot use 'super' outside of a class"),
            CannotUseSuperInAClassWithNoSuperClass(_, name) => write!(
                f,
                "Cannot use 'super' in class '{}' which has no superclass",
                name
            ),
        }
    }
}

impl ErrorPosition for LexicalScopeResolutionError {
    fn position(&self) -> &Position {
        use LexicalScopeResolutionError::*;
        match self {
            VariableUsedInItsInitializer(pos, _)
            | DuplicateVariableDeclaration(pos, _)
            | ReturnKeywordOutsideFunction(pos)
            | AnonymousClassMethod(pos, _)
            | CannotUseThisOutsideOfAClass(pos)
            | CannotReturnInsideInitializer(pos, _)
            | ClassCannotInheritFromItself(pos, _)
            | CannotUseSuperOutsideAclass(pos)
            | CannotUseSuperInAClassWithNoSuperClass(pos, _) => pos,
        }
    }
}
