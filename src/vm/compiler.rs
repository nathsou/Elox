use super::{EloxVM, Inst, Value};
use crate::parser::expressions::{BinaryOperator, Expr, ExprCtx, Literal, UnaryOperator};
use crate::parser::statements::Stmt;
use crate::parser::{IdentifierHandle, Parser};
use crate::runner::{EloxError, EloxResult};
use crate::scanner::token::Position;
use crate::scanner::Scanner;
use std::ops::Deref;

impl EloxVM {
    #[inline(always)]
    fn emit(&mut self, inst: Inst, pos: Position) {
        self.chunk.write(inst, pos);
    }

    #[inline(always)]
    fn emit_constant(&mut self, val: Value, pos: Position) {
        self.chunk.write_constant(val, pos);
    }

    #[inline]
    fn named_variable(&mut self, handle: IdentifierHandle, pos: Position) {
        self.emit(Inst::GetGlobal(handle), pos);
    }

    pub fn compile(&mut self, source: &str) -> EloxResult {
        let scanner = Scanner::new(source.chars().peekable());
        self.identifiers.clear();
        let ast = Parser::new(scanner.peekable(), &mut self.identifiers).parse();

        match ast {
            Ok(ast) => {
                for stmt in &ast {
                    self.compile_stmt(stmt)?;
                }
            }
            Err(err) => return Err(EloxError::Parser(err)),
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
                self.named_variable(var_expr.identifier.name, var_expr.identifier.pos);
            }
            Expr::Assign(assignment_expr) => {
                self.compile_expr(&assignment_expr.expr)?;
                self.emit(
                    Inst::SetGlobal(assignment_expr.identifier.name),
                    assignment_expr.identifier.pos,
                );
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
                if let Some(init) = &var_decl.initializer {
                    self.compile_expr(init)?;
                } else {
                    self.emit(Inst::Nil, var_decl.identifier.pos);
                }

                self.emit(
                    Inst::Global(var_decl.identifier.name),
                    var_decl.identifier.pos,
                );
            }
            _ => panic!("Unimplemented stmt"),
        }

        Ok(())
    }
}