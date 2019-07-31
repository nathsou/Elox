use super::{instructions::FuncObj, Inst, Obj, Value};
use crate::interpreter::lexical_scope::LexicalScopeResolutionError;
use crate::parser::expressions::{
    BinaryOperator, Expr, ExprCtx, FuncExpr, Literal, LogicalOperator, UnaryOperator,
};
use crate::parser::statements::Stmt;
use crate::parser::{IdentifierHandle, IdentifierHandlesGenerator, IdentifierUse};
use crate::runner::{EloxError, EloxResult};
use crate::scanner::token::Position;
use fnv::FnvHashMap;
use std::ops::Deref;
use std::rc::Rc;

pub struct Local {
    handle: IdentifierHandle,
    depth: usize,
}

enum JumpKind {
    Unconditional,
    OnTrue,
    OnFalse,
}

use JumpKind::*;

pub enum FuncType {
    SCRIPT, // implicit main function
    FUNC,
}

pub struct Compiler<'a> {
    func: &'a mut FuncObj,
    func_type: FuncType,
    identifiers: &'a mut IdentifierHandlesGenerator,
    strings: &'a mut FnvHashMap<String, Rc<Obj>>,
    scope_depth: usize,
    locals: &'a mut Vec<Local>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        locals: &'a mut Vec<Local>,
        scope_depth: usize,
        func: &'a mut FuncObj,
        func_type: FuncType,
        identifiers: &'a mut IdentifierHandlesGenerator,
        strings: &'a mut FnvHashMap<String, Rc<Obj>>,
    ) -> Compiler<'a> {
        Compiler {
            locals,
            scope_depth,
            func,
            func_type,
            identifiers,
            strings,
        }
    }

    #[inline]
    fn emit(&mut self, inst: Inst, pos: Position) {
        self.func.chunk.write(inst, pos);
    }

    #[inline]
    fn emit_constant(&mut self, val: Value, pos: Position) {
        self.func.chunk.write_constant(val, pos);
    }

    fn get_named_variable(&mut self, handle: IdentifierHandle, pos: Position) -> EloxResult {
        if let Some(idx) = self.resolve_local(handle, pos)? {
            self.emit(Inst::GetLocal(idx + 1), pos); // + 1 since idx 0 is used by the surrounding func
        } else {
            self.emit(Inst::GetGlobal(handle), pos);
        }

        Ok(())
    }

    fn set_named_variable(&mut self, handle: IdentifierHandle, pos: Position) -> EloxResult {
        if let Some(idx) = self.resolve_local(handle, pos)? {
            self.emit(Inst::SetLocal(idx + 1), pos); // + 1 since idx 0 is used by the surrounding func
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
            let mut local = self
                .locals
                .last_mut()
                .expect("Could not mark as initialized");
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

        // println!("POPPPY");

        match pops {
            0 => {}
            1 => self.emit(Inst::Pop, pos),
            _ => self.emit(Inst::PopN(pops), pos),
        };
    }

    fn emit_jmp(&mut self, kind: JumpKind, pos: Position) -> usize {
        let inst = match kind {
            Unconditional => Inst::Jmp(0),
            OnTrue => Inst::JmpIfTrue(0),
            OnFalse => Inst::JmpIfFalse(0),
        };

        self.emit(inst, pos);
        self.func.chunk.inst_count() - 1
    }

    #[inline]
    fn patch_jmp(&mut self, idx: usize) {
        self.func.chunk.patch_jmp(idx);
    }

    fn emit_loop(&mut self, loop_start: usize, pos: Position) {
        let offset = self.func.chunk.inst_count() - loop_start;
        self.emit(Inst::Loop(offset), pos);
    }

    pub fn compile(&mut self, ast: &Vec<Stmt>) -> EloxResult {
        for stmt in ast {
            self.compile_stmt(stmt)?;
        }

        self.end();

        Ok(())
    }

    fn end(&mut self) {
        self.emit(Inst::Nil, Position { line: 0, col: 0 });
        self.emit(Inst::Ret, Position { line: 0, col: 0 });
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
            Expr::Logical(logical_expr) => {
                let logical_expr = logical_expr.deref();

                self.compile_expr(&logical_expr.left)?;

                match logical_expr.operator {
                    LogicalOperator::And => {
                        let short_circuit = self.emit_jmp(OnFalse, logical_expr.left.pos);

                        // discard the left operand (which is true)
                        self.emit(Inst::Pop, logical_expr.right.pos);
                        self.compile_expr(&logical_expr.right)?;

                        self.patch_jmp(short_circuit);
                    }
                    LogicalOperator::Or => {
                        let short_circuit = self.emit_jmp(OnTrue, logical_expr.left.pos);

                        self.emit(Inst::Pop, logical_expr.right.pos);
                        self.compile_expr(&logical_expr.right)?;

                        self.patch_jmp(short_circuit);
                    }
                }
            }
            Expr::Func(func_expr) => {
                self.compile_func(func_expr, FuncType::FUNC)?;
            }
            Expr::Call(call_expr) => {
                self.compile_expr(&call_expr.callee)?;
                for arg in &call_expr.args {
                    self.compile_expr(&arg)?;
                }
                self.emit(Inst::Call(call_expr.args.len()), call_expr.callee.pos);
            }
            _ => panic!("Unimplemented expr"),
        }

        Ok(())
    }

    fn emit_identifier(&mut self, id: &IdentifierUse) -> EloxResult {
        // self.emit_constant(obj, id.pos);
        // self.get_named_variable(id.name, id.pos)?;
        self.declare_variable(id.name, id.pos)?;
        self.define_variable(id.name, id.pos);
        // self.mark_initialized();

        // if self.scope_depth > 0 {
        //     self.mark_initialized();
        // }

        Ok(())
    }

    fn compile_func(&mut self, func_expr: &FuncExpr, type_: FuncType) -> EloxResult {
        let arity = if let Some(params) = &func_expr.params {
            params.len()
        } else {
            0
        };
        let name = if let Some(id) = func_expr.name {
            self.declare_variable(id.name, id.pos)?;
            Some(id.name)
        } else {
            None
        };
        let mut func = FuncObj::new(name, arity);
        let mut compiler = Compiler::new(
            &mut self.locals,
            self.scope_depth,
            &mut func,
            type_,
            &mut self.identifiers,
            &mut self.strings,
        );

        compiler.begin_scope();

        if let Some(params) = &func_expr.params {
            for param in params {
                compiler.emit_identifier(param.identifier())?;
            }
        }

        for stmt in &func_expr.body {
            compiler.compile_stmt(stmt)?;
        }
        compiler.end();
        compiler.end_scope(func_expr.pos);

        func.chunk.disassemble(&format!(
            "<fn {:?}>",
            if let Some(handle) = func_expr.name {
                self.identifiers.name(handle.name)
            } else {
                "anonymous".into()
            }
        ));
        let func_val = Value::Object(Rc::new(Obj::Func(Rc::new(func))));
        self.emit_constant(func_val, func_expr.pos);

        if let Some(id) = func_expr.name {
            self.define_variable(id.name, id.pos);
        }

        Ok(())
    }

    fn define_variable(&mut self, global: IdentifierHandle, pos: Position) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit(Inst::DefGlobal(global), pos);
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> EloxResult {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                let pop = match expr_stmt.expr.expr {
                    Expr::Func(_) => false,
                    _ => true,
                };

                self.compile_expr(&expr_stmt.expr)?;
                if pop {
                    self.emit(Inst::Pop, expr_stmt.expr.pos);
                }
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

                self.define_variable(var_decl.identifier.name, var_decl.pos);
            }
            Stmt::Block(block) => {
                self.begin_scope();
                for stmt in &block.stmts {
                    self.compile_stmt(stmt)?;
                }
                self.end_scope(block.end_pos);
            }
            Stmt::If(if_stmt) => {
                self.compile_expr(&if_stmt.condition)?;

                let then_jmp = self.emit_jmp(OnFalse, if_stmt.condition.pos);
                // pop the condition
                self.emit(Inst::Pop, if_stmt.condition.pos);
                self.compile_stmt(if_stmt.then_branch.deref())?;

                let else_jmp = self.emit_jmp(Unconditional, if_stmt.condition.pos);
                self.patch_jmp(then_jmp);

                // pop the condition
                self.emit(Inst::Pop, if_stmt.condition.pos);

                if let Some(else_branch) = &if_stmt.else_branch {
                    self.compile_stmt(else_branch.deref())?;
                }

                self.patch_jmp(else_jmp);
            }
            Stmt::While(while_stmt) => {
                let loop_start = self.func.chunk.inst_count();

                self.compile_expr(&while_stmt.condition)?;

                let exit_jmp = self.emit_jmp(OnFalse, while_stmt.condition.pos);

                self.emit(Inst::Pop, while_stmt.condition.pos);
                self.compile_stmt(while_stmt.body.deref())?;

                self.emit_loop(loop_start, while_stmt.condition.pos);

                self.patch_jmp(exit_jmp);

                self.emit(Inst::Pop, while_stmt.condition.pos);
            }
            Stmt::Return(ret_stmt) => {
                if let Some(ret) = &ret_stmt.value {
                    self.compile_expr(ret)?;
                    self.emit(Inst::Ret, ret_stmt.pos);
                } else {
                    self.emit(Inst::Nil, Position { line: 0, col: 0 });
                }
            }
            _ => panic!("Unimplemented stmt"),
        }

        Ok(())
    }
}
