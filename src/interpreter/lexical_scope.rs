use super::Stmt;
use crate::parser::{expressions::Expr, IdentifierHandle};
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;

pub enum LexicalScopeResolutionError {
    UnhandledStmt,
}

pub type LexicalScopeResolutionResult = Result<(), LexicalScopeResolutionError>;

pub struct Resolver {
    scopes: Vec<FnvHashMap<IdentifierHandle, bool>>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new() }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FnvHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: IdentifierHandle) {
        if self.scopes.is_empty() {
            return;
        }

        let len = self.scopes.len();

        match self.scopes[len - 1].entry(identifier) {
            Entry::Vacant(v) => {
                v.insert(false);
            }
            _ => {}
        }
    }

    fn define(&mut self, identifier: IdentifierHandle) {
        if self.scopes.is_empty() {
            return;
        }

        let len = self.scopes.len();

        self.scopes[len - 1].insert(identifier, true);
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
                // resolver.declare(decl.name);
                if let Some(init) = &decl.initializer {
                    init.resolve(resolver)?;
                }
                // resolver.define(decl.name);
                Ok(())
            }
            _ => Err(LexicalScopeResolutionError::UnhandledStmt),
        }
    }
}

impl LexicallyScoped for Expr {
    fn resolve(&self, resolver: &mut Resolver) -> LexicalScopeResolutionResult {
        Ok(())
    }
}
