use super::{Chunk, Inst, Obj, Value};
use crate::interpreter::lexical_scope::LexicalScopeResolutionError;
use crate::parser::expressions::{BinaryOperator, Expr, ExprCtx, Literal, UnaryOperator};
use crate::parser::statements::Stmt;
use crate::parser::{IdentifierHandle, IdentifierHandlesGenerator};
use crate::runner::{EloxError, EloxResult};
use crate::scanner::token::Position;
use fnv::FnvHashMap;
use std::ops::Deref;
use std::rc::Rc;

struct Local {
    handle: IdentifierHandle,
    depth: usize,
}

pub struct Compiler<'a> {
    chunk: &'a mut Chunk,
    identifiers: &'a mut IdentifierHandlesGenerator,
    strings: &'a mut FnvHashMap<String, Rc<Obj>>,
    scope_depth: usize,
    locals: Vec<Local>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        chunk: &'a mut Chunk,
        identifiers: &'a mut IdentifierHandlesGenerator,
        strings: &'a mut FnvHashMap<String, Rc<Obj>>,
    ) -> Compiler<'a> {
        Compiler {
            chunk,
            identifiers,
            strings,
            scope_depth: 0,
            locals: Vec::new(),
        }
    }

    #[inline(always)]
    fn emit(&mut self, inst: Inst, pos: Position) {
        self.chunk.write(inst, pos);
    }

    #[inline(always)]
    fn emit_constant(&mut self, val: Value, pos: Position) {
        self.chunk.write_constant(val, pos);
    }

    fn get_named_variable(&mut self, handle: IdentifierHandle, pos: Position) -> EloxResult {
        if let Some(idx) = self.resolve_local(handle, pos)? {
            self.emit(Inst::GetLocal(idx), pos);
        } else {
            self.emit(Inst::GetGlobal(handle), pos);
        }

        Ok(())
    }

    fn set_named_variable(&mut self, handle: IdentifierHandle, pos: Position) -> EloxResult {
        if let Some(idx) = self.resolve_local(handle, pos)? {
            self.emit(Inst::SetLocal(idx), pos);
        } else {
            self.emit(Inst::SetGlobal(handle), pos);
        }

        Ok(())
    }

    fn resolve_local(
        &self,
        handle: IdentifierHandle,
        pos: Position,
    ) -> Result<Option<usize>, EloxError> {
        for (idx, local) in self.locals.iter().rev().enumerate() {
            if local.handle == handle {
                return if local.depth == usize::max_value() {
                    Err(EloxError::Resolution(
                        LexicalScopeResolutionError::VariableUsedInItsInitializer(
                            pos,
                            self.identifiers.name(handle),
                        ),
                    ))
                } else {
                    Ok(Some(self.locals.len() - 1 - idx))
                };
            }
        }

        Ok(None)
    }

    fn declare_variable(&mut self, handle: IdentifierHandle, pos: Position) -> EloxResult {
        // Global variables are implicitly declared.
        if self.scope_depth == 0 {
            return Ok(());
        }

        for local in self.locals.iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }

            if local.handle == handle {
                return Err(EloxError::Resolution(
                    LexicalScopeResolutionError::DuplicateVariableDeclaration(
                        pos,
                        self.identifiers.name(handle),
                    ),
                ));
            }
        }

        self.add_local(handle);

        return Ok(());
    }

    fn add_local(&mut self, handle: IdentifierHandle) {
        self.locals.push(Local {
            handle,
            depth: usize::max_value(), // mark as uninitialized
        });
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth != 0 {
            let mut local = self.locals.last_mut().unwrap();
            local.depth = self.scope_depth;
        }
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    #[inline]
    fn end_scope(&mut self, pos: Position) {
        self.scope_depth -= 1;
        let mut pops = 0;

        while self.locals.len() > 0 && self.locals[self.locals.len() - 1].depth > self.scope_depth {
            self.locals.pop();
            pops += 1;
        }

        match pops {
            0 => {}
            1 => self.emit(Inst::Pop, pos),
            _ => self.emit(Inst::PopN(pops), pos),
        };
    }

    pub fn compile(&mut self, ast: &Vec<Stmt>) -> EloxResult {
        for stmt in ast {
            self.compile_stmt(stmt)?;
        }

        self.emit(Inst::Ret, Position { line: 0, col: 0 });

        return Ok(());
    }

    fn compile_expr(&mut self, expr_ctx: &ExprCtx) -> EloxResult {
        match &expr_ctx.expr {
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => {
                    self.emit_constant(Value::Number(*n), expr_ctx.pos);
                }
                Literal::Boolean(b) => {
                    if *b {
                        self.emit(Inst::True, expr_ctx.pos);
                    } else {
                        self.emit(Inst::False, expr_ctx.pos);
                    }
                }
                Literal::Nil => {
                    self.emit(Inst::Nil, expr_ctx.pos);
                }
                Literal::String(ref string) => {
                    let obj = Value::new_str(string, &mut self.strings);
                    self.emit_constant(obj, expr_ctx.pos);
                }
            },
            Expr::Grouping(sub_expr) => {
                self.compile_expr(&sub_expr.deref().expression)?;
            }
            Expr::Unary(unary_expr) => {
                let expr = unary_expr.deref();
                self.compile_expr(&expr.right)?;
                match expr.operator {
                    UnaryOperator::Minus => self.emit(Inst::Neg, expr_ctx.pos),
                    UnaryOperator::Bang => self.emit(Inst::Not, expr_ctx.pos),
                }
            }
            Expr::Binary(bin_expr) => {
                let expr = bin_expr.deref();
                let op_ctx = &expr.operator;
                self.compile_expr(&expr.left)?;
                self.compile_expr(&expr.right)?;

                match op_ctx.op {
                    BinaryOperator::Plus => self.emit(Inst::Add, op_ctx.pos),
                    BinaryOperator::Minus => self.emit(Inst::Sub, op_ctx.pos),
                    BinaryOperator::Star => self.emit(Inst::Mult, op_ctx.pos),
                    BinaryOperator::Slash => self.emit(Inst::Div, op_ctx.pos),
                    BinaryOperator::Percent => self.emit(Inst::Mod, op_ctx.pos),
                    BinaryOperator::EqualEqual => self.emit(Inst::Equ, op_ctx.pos),
                    BinaryOperator::BangEqual => self.emit(Inst::Neq, op_ctx.pos),
                    BinaryOperator::Greater => self.emit(Inst::Gtr, op_ctx.pos),
                    BinaryOperator::Less => self.emit(Inst::Lss, op_ctx.pos),
                    BinaryOperator::GreaterEqual => self.emit(Inst::Gtq, op_ctx.pos),
                    BinaryOperator::LessEqual => self.emit(Inst::Leq, op_ctx.pos),
                }
            }
            Expr::Var(var_expr) => {
                self.get_named_variable(var_expr.identifier.name, var_expr.identifier.pos)?;
            }
            Expr::Assign(assignment_expr) => {
                self.compile_expr(&assignment_expr.expr)?;
                self.set_named_variable(
                    assignment_expr.identifier.name,
                    assignment_expr.identifier.pos,
                )?;
            }
            _ => panic!("Unimplemented expr"),
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> EloxResult {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.compile_expr(&expr_stmt.expr)?;
                self.emit(Inst::Pop, expr_stmt.expr.pos);
            }
            Stmt::Print(print_stmt) => {
                self.compile_expr(&print_stmt.value)?;
                self.emit(Inst::Print, print_stmt.pos);
            }
            Stmt::VarDecl(var_decl) => {
                self.declare_variable(var_decl.identifier.name, var_decl.identifier.pos)?;
                if let Some(init) = &var_decl.initializer {
                    self.compile_expr(init)?;
                } else {
                    self.emit(Inst::Nil, var_decl.identifier.pos);
                }
                self.mark_initialized();

                if self.scope_depth == 0 {
                    self.emit(
                        Inst::DefGlobal(var_decl.identifier.name),
                        var_decl.identifier.pos,
                    );
                }
            }
            Stmt::Block(block) => {
                self.begin_scope();
                for stmt in &block.stmts {
                    self.compile_stmt(stmt)?;
                }
                self.end_scope(block.end_pos);
            }
            _ => panic!("Unimplemented stmt"),
        }

        Ok(())
    }
}
