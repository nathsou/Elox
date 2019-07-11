pub mod expressions;
pub mod parser_result;
pub mod pretty_printer;
pub mod statements;
use crate::scanner::scanner_result::ScannerResult;
use crate::scanner::token::token_type::{TokenType, TokenType::*};
use crate::scanner::token::{Position, Token};
use crate::scanner::Scanner;
use expressions::*;
use fnv::FnvHashMap;
use parser_result::{ParserError, ParserResult};
use statements::*;
use std::iter::Peekable;

pub struct Identifier {}

impl Identifier {
    pub fn this() -> IdentifierHandle {
        0
    }

    pub fn init() -> IdentifierHandle {
        1
    }

    pub fn super_() -> IdentifierHandle {
        2
    }

    pub fn array() -> IdentifierHandle {
        3
    }

    pub fn clock() -> IdentifierHandle {
        4
    }

    pub fn anonymous() -> IdentifierHandle {
        5
    }

    pub fn get() -> IdentifierHandle {
        6
    }

    pub fn set() -> IdentifierHandle {
        7
    }

    pub fn str_() -> IdentifierHandle {
        8
    }
}

pub type IdentifierHandle = usize;
pub type IdentifierUseHandle = usize;
pub type IdentifierNames = Vec<std::string::String>;

#[derive(Debug)]
pub struct IdentifierHandlesGenerator {
    handles: FnvHashMap<std::string::String, IdentifierHandle>,
    names: IdentifierNames,
    next_id_handle: IdentifierHandle,
    next_use_handle: IdentifierUseHandle,
}

impl IdentifierHandlesGenerator {
    pub fn new() -> IdentifierHandlesGenerator {
        let mut handle_gen = IdentifierHandlesGenerator {
            next_id_handle: 9,
            handles: FnvHashMap::default(),
            next_use_handle: 0,
            names: Vec::with_capacity(9),
        };

        handle_gen.insert(std::string::String::from("this"), Identifier::this());
        handle_gen.insert(std::string::String::from("init"), Identifier::init());
        handle_gen.insert(std::string::String::from("super"), Identifier::super_());
        handle_gen.insert(std::string::String::from("Array"), Identifier::array());
        handle_gen.insert(std::string::String::from("clock"), Identifier::clock());
        handle_gen.insert(
            std::string::String::from("anonymous"),
            Identifier::anonymous(),
        );
        handle_gen.insert(std::string::String::from("#get"), Identifier::get());
        handle_gen.insert(std::string::String::from("#set"), Identifier::set());
        handle_gen.insert(std::string::String::from("#str"), Identifier::str_());

        handle_gen
    }

    pub fn insert(&mut self, name: std::string::String, handle: IdentifierHandle) {
        self.names.push(name.clone());
        self.handles.insert(name, handle);
    }

    pub fn names(&self) -> IdentifierNames {
        self.names.clone()
    }

    fn next_with_name(&mut self, name: &str) -> IdentifierUse {
        IdentifierUse::new(self.by_name(name), self.next_use_handle())
    }

    fn next_with_handle(&mut self, name: IdentifierHandle) -> IdentifierUse {
        IdentifierUse::new(name, self.next_use_handle())
    }

    fn next_id_handle(&mut self) -> IdentifierHandle {
        let curr = self.next_id_handle;
        self.next_id_handle += 1;

        curr
    }

    pub fn next_use_handle(&mut self) -> IdentifierUseHandle {
        let curr = self.next_use_handle;
        self.next_use_handle += 1;

        curr
    }

    pub fn by_name(&mut self, name: &str) -> IdentifierHandle {
        if let Some(handle) = self.handles.get(name) {
            return *handle;
        }

        let handle = self.next_id_handle();
        self.insert(name.into(), handle);

        handle
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct IdentifierUse {
    pub name: IdentifierHandle,
    pub use_handle: IdentifierUseHandle,
}

impl std::fmt::Display for IdentifierUse {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(name handle: {}, use: {})", self.name, self.use_handle)
    }
}

impl IdentifierUse {
    pub fn new(name: IdentifierHandle, use_handle: IdentifierUseHandle) -> IdentifierUse {
        IdentifierUse { name, use_handle }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Scanner<'a>>,
    identifiers: IdentifierHandlesGenerator,
    pos: Position,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Scanner<'a>>, identifiers: IdentifierHandlesGenerator) -> Self {
        Parser {
            tokens,
            identifiers,
            pos: Position { line: 1, col: 1 },
        }
    }

    pub fn names(&self) -> IdentifierNames {
        self.identifiers.names()
    }

    fn next(&mut self) -> Option<ScannerResult<Token>> {
        let token = self.tokens.next();

        if let Some(Ok(tok)) = &token {
            self.pos.line = tok.pos.line;
            self.pos.col = tok.pos.col;
            // println!("[{}:{}] : {:?}", self.pos.line, self.pos.col, tok);
        }

        token
    }

    fn peek_token_type(&mut self) -> ParserResult<TokenType> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(token.token_type.clone()),
            Some(Err(err)) => Err(ParserError::ScannerError(err.clone())),
            None => Ok(EOF),
        }
    }

    pub fn parse(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        loop {
            match self.peek_token_type()? {
                EOF => break,
                _ => {
                    let stmt = self.declaration()?;
                    stmts.push(stmt);
                }
            }
        }

        // TODO: global scope Issue?

        Ok(stmts)
    }

    fn consume(&mut self, token_type: TokenType) -> ParserResult<bool> {
        if self.peek_token_type()? == token_type {
            self.next();
            return Ok(true);
        }

        Ok(false)
    }

    fn match_next(&mut self, token_type: TokenType) -> ParserResult<bool> {
        Ok(self.peek_token_type()? == token_type)
    }

    fn declaration(&mut self) -> ParserResult<Stmt> {
        let mut res: Option<ParserResult<Stmt>> = None;

        match self.peek_token_type()? {
            Fun => {
                self.next();
                let f = self.function_declaration()?;
                res = Some(Ok(ExprStmt::to_stmt(f)));
            }
            Var => {
                self.next();
                res = Some(self.var_declaration());
            }
            Class => {
                self.next();
                res = Some(self.class_declaration());
            }
            _ => {}
        }

        if let Some(res) = res {
            match res {
                Ok(stmt) => return Ok(stmt),
                Err(err) => {
                    self.synchronize();
                    return Err(err);
                }
            }
        }

        return self.statement();
    }

    fn class_declaration(&mut self) -> ParserResult<Stmt> {
        if let Some(name) = self.consume_identifier()? {
            let mut superclass = None;

            if self.consume(Less)? {
                if let Some(parent_class) = self.consume_identifier()? {
                    superclass = Some(VarExpr {
                        identifier: parent_class,
                    });
                } else {
                    return Err(ParserError::ExpectedSuperclassName(self.pos.clone()));
                }
            }

            if self.consume(LeftBrace)? {
                let mut methods = Vec::new();

                while !self.match_next(RightBrace)? {
                    let func = self.function_declaration()?;
                    let pos = func.pos();
                    match func.expr {
                        Expr::Func(f) => {
                            methods.push(ExprWithCtxt {
                                expr: f,
                                pos,
                            });
                        }
                        _ => {
                            return Err(ParserError::ExpectedMethodDeclarationInClass(
                                self.pos.clone(),
                                name.name,
                            ))
                        }
                    }
                }

                if !self.consume(RightBrace)? {
                    return Err(ParserError::ExpectedRightBraceAfterClassBody(
                        self.pos.clone(),
                    ));
                }

                return Ok(ClassDeclStmt::to_stmt(self.pos.clone(), name, superclass, methods));
            } else {
                return Err(ParserError::ExpectedLeftBraceBeforeClassBody(
                    self.pos.clone(),
                ));
            }
        } else {
            return Err(ParserError::ExpectedClassName(self.pos.clone()));
        }
    }

    fn function_declaration(&mut self) -> ParserResult<ExprCtx> {
        let name = self.consume_identifier()?;
        let pos = self.pos.clone();

        if self.consume(LeftParen)? {
            let mut params = Vec::new();

            if !self.match_next(RightParen)? {
                loop {
                    if let Some(param) = self.consume_identifier()? {
                        params.push(param);
                    } else {
                        return Err(ParserError::ExpectedFuncParamName(self.pos.clone()));
                    }

                    if !self.consume(Comma)? {
                        break;
                    }
                }
            }

            if !self.consume(RightParen)? {
                return Err(ParserError::ExpectedRightParenAfterCallExpr(
                    self.pos.clone(),
                ));
            }

            if !self.consume(LeftBrace)? {
                return Err(ParserError::ExpectedLeftBraceBeforeFuncBody(
                    self.pos.clone(),
                ));
            }

            let body = ((self.block()?) as BlockStmt).stmts;

            return Ok(FuncExpr::new(pos.clone(), name, params, body));
        } else {
            Err(ParserError::ExpectedRightParenAfterCallExpr(
                self.pos.clone(),
            ))
        }
    }

    fn match_identifier(&mut self) -> ParserResult<Option<IdentifierUse>> {
        if let Identifier(name) = self.peek_token_type()? {
            return Ok(Some(IdentifierUse::new(
                self.identifiers.by_name(&name),
                self.identifiers.next_use_handle(),
            )));
        }

        Ok(None)
    }

    fn consume_identifier(&mut self) -> ParserResult<Option<IdentifierUse>> {
        if let Some(identifier) = self.match_identifier()? {
            self.next();
            return Ok(Some(identifier));
        }

        Ok(None)
    }

    fn var_declaration(&mut self) -> ParserResult<Stmt> {
        if let Some(identifier) = self.match_identifier()? {
            self.next();
            let mut initializer: Option<ExprCtx> = None;

            if self.consume(Equal)? {
                initializer = Some(self.expression()?);
            }

            if !self.consume(SemiColon)? {
                return Err(ParserError::ExpectedSemicolonAfterExpr(self.pos.clone()));
            }

            return Ok(VarDeclStmt::to_stmt(identifier, initializer));
        } else {
            if let Some(Ok(tok)) = self.tokens.peek() {
                return Err(ParserError::ExpectedVarName(
                    self.pos.clone(),
                    tok.lexeme.clone(),
                ));
            } else {
                return Err(ParserError::ExpectedStatement(self.pos.clone()));
            }
        }
    }

    fn statement(&mut self) -> ParserResult<Stmt> {
        if let Some(Ok(token)) = self.tokens.peek() {
            match token.token_type {
                For => {
                    self.next();
                    return self.for_stmt();
                }
                If => {
                    self.next();
                    return self.if_stmt();
                }
                Print => {
                    // consumme the print token
                    self.next();
                    return self.print_stmt();
                }
                Return => {
                    self.next();
                    return self.return_stmt();
                }
                While => {
                    self.next();
                    return self.while_stmt();
                }
                LeftBrace => {
                    self.next();
                    return Ok(Stmt::Block(self.block()?));
                }
                _ => return self.expr_stmt(),
            }
        }

        Err(ParserError::ExpectedStatement(self.pos.clone()))
    }

    fn return_stmt(&mut self) -> ParserResult<Stmt> {
        let mut value: Option<ExprCtx> = None;

        if !self.match_next(SemiColon)? {
            value = Some(self.expression()?);
        }

        if !self.consume(SemiColon)? {
            return Err(ParserError::ExpectedSemiColonAfterReturnValue(
                self.pos.clone(),
            ));
        }

        Ok(ReturnStmt::to_stmt(value))
    }

    // forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    fn for_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen)? {
            let mut initializer;

            if self.consume(Var)? {
                initializer = Some(self.var_declaration()?);
            } else {
                initializer = Some(self.expr_stmt()?);
            }

            let mut condition = None;

            if !self.match_next(SemiColon)? {
                condition = Some(self.expression()?);
            }

            if !self.consume(SemiColon)? {
                return Err(ParserError::ExpectedSemicolonAfterLoopCondition(
                    self.pos.clone(),
                ));
            }

            let mut increment = None;

            if !self.match_next(RightParen)? {
                increment = Some(self.expression()?);
            }

            if !self.consume(RightParen)? {
                return Err(ParserError::ExpectedRightParenAfterForClauses(
                    self.pos.clone(),
                ));
            }

            let mut body = self.statement()?;

            if let Some(inc) = increment {
                body = BlockStmt::to_stmt(vec![body, ExprStmt::to_stmt(inc)]);
            }

            body = WhileStmt::to_stmt(
                if let Some(cond) = condition {
                    cond
                } else {
                    Literal::new(self.pos.clone(), Literal::Boolean(true))
                },
                body,
            );

            if let Some(init) = initializer {
                body = BlockStmt::to_stmt(vec![init, body]);
            }

            Ok(body)
        } else {
            Err(ParserError::ExpectedLeftParenAfterLoop(self.pos.clone()))
        }
    }

    // whileStmt → "while" "(" expression ")" statement ;
    fn while_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen)? {
            let condition = self.expression()?;
            if self.consume(RightParen)? {
                let body = self.statement()?;

                Ok(WhileStmt::to_stmt(condition, body))
            } else {
                Err(ParserError::ExpectedRightParenAfterLoop(self.pos.clone()))
            }
        } else {
            Err(ParserError::ExpectedLeftParenAfterLoop(self.pos.clone()))
        }
    }

    fn or_expr(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.and_expr()?;

        while self.consume(Or)? {
            let right = self.and_expr()?;
            expr_ctx = LogicalExpr::new(self.pos.clone(), expr_ctx, LogicalOperator::Or, right);
        }

        Ok(expr_ctx)
    }

    fn and_expr(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.equality()?;

        while self.consume(And)? {
            let right = self.equality()?;
            expr_ctx = LogicalExpr::new(self.pos.clone(), expr_ctx, LogicalOperator::And, right);
        }

        Ok(expr_ctx)
    }

    fn if_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen)? {
            let condition = self.expression()?;
            if self.consume(RightParen)? {
                let then_branch = self.statement()?;
                let mut else_branch = None;

                if self.consume(Else)? {
                    else_branch = Some(self.statement()?);
                }

                Ok(IfStmt::to_stmt(condition, then_branch, else_branch))
            } else {
                Err(ParserError::ExpectedRightParenAfterIf(self.pos.clone()))
            }
        } else {
            Err(ParserError::ExpectedLeftParenAfterIf(self.pos.clone()))
        }
    }

    fn block(&mut self) -> ParserResult<BlockStmt> {
        let mut stmts = Vec::new();

        while let Some(Ok(tok)) = self.tokens.peek() {
            if tok.token_type == RightBrace {
                break;
            }

            stmts.push(self.declaration()?);
        }

        if self.consume(RightBrace)? {
            return Ok(BlockStmt { stmts });
        }

        Err(ParserError::ExpectedRightBraceAfterBlock(self.pos.clone()))
    }

    fn print_stmt(&mut self) -> ParserResult<Stmt> {
        let value = self.expression()?;

        if self.consume(SemiColon)? {
            return Ok(PrintStmt::to_stmt(value));
        }

        Err(ParserError::ExpectedSemicolonAfterExpr(self.pos.clone()))
    }

    fn expr_stmt(&mut self) -> ParserResult<Stmt> {
        let expr_ctx = self.expression()?;

        if self.consume(SemiColon)? {
            return Ok(ExprStmt::to_stmt(expr_ctx));
        }

        Err(ParserError::ExpectedSemicolonAfterExpr(self.pos.clone()))
    }

    // expression     → equality ;
    fn expression(&mut self) -> ParserResult<ExprCtx> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<ExprCtx> {
        let expr_ctx = self.or_expr()?;

        let next_token = self.tokens.peek();

        if let Some(Ok(token)) = next_token {
            let token_type = token.token_type.clone();
            let right_value = match token.token_type {
                Equal => {
                    self.next();
                    Some(self.assignment()?)
                }
                PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual => {
                    self.next();
                    let val = self.assignment()?;
                    let op = BinaryOperator::from_token_type(&token_type).unwrap();
                    Some(BinaryExpr::new(self.pos.clone(), expr_ctx.clone(), op, val))
                }
                PlusPlus => {
                    self.next();
                    Some(BinaryExpr::new(
                        self.pos.clone(),
                        expr_ctx.clone(),
                        BinaryOperator::Plus,
                        Literal::new(self.pos.clone(), Literal::Number(1f64)),
                    ))
                }
                MinusMinus => {
                    self.next();
                    Some(BinaryExpr::new(
                        self.pos.clone(),
                        expr_ctx.clone(),
                        BinaryOperator::Minus,
                        Literal::new(self.pos.clone(), Literal::Number(1f64)),
                    ))
                }
                _ => None,
            };

            if let Some(right_val) = right_value {
                match expr_ctx.expr {
                    Expr::Var(v) => return Ok(AssignExpr::new(self.pos.clone(), v.identifier, right_val)),
                    Expr::Get(g) => return Ok(SetExpr::new(self.pos.clone(), g.property, g.object, right_val)),
                    Expr::Call(expr_ctx) => {
                        if let Expr::Get(access) = expr_ctx.callee.expr {
                            if access.property.name == Identifier::get() {
                                let set = self
                                    .identifiers
                                    .next_with_handle(Identifier::set());
                                let mut args = expr_ctx.args;
                                args.push(right_val);
                                return Ok(CallExpr::new(self.pos.clone(), GetExpr::new(self.pos.clone(), set, access.object), args));
                            } else {
                                return Err(ParserError::InvalidAssignmentTarget(self.pos.clone()));
                            }
                        }

                        return Err(ParserError::InvalidAssignmentTarget(self.pos.clone()));
                    }
                    _ => return Err(ParserError::InvalidAssignmentTarget(self.pos.clone())),
                };
            }
        }

        return Ok(expr_ctx);
    }

    fn match_equality(&mut self) -> ParserResult<Option<BinaryOperator>> {
        match self.peek_token_type()? {
            BangEqual => {
                self.next();
                Ok(Some(BinaryOperator::BangEqual))
            }
            EqualEqual => {
                self.next();
                Ok(Some(BinaryOperator::EqualEqual))
            }
            _ => Ok(None),
        }
    }

    fn match_comparison(&mut self) -> ParserResult<Option<BinaryOperator>> {
        match self.peek_token_type()? {
            Greater => {
                self.next();
                Ok(Some(BinaryOperator::Greater))
            }
            GreaterEqual => {
                self.next();
                Ok(Some(BinaryOperator::GreaterEqual))
            }
            Less => {
                self.next();
                Ok(Some(BinaryOperator::Less))
            }
            LessEqual => {
                self.next();
                Ok(Some(BinaryOperator::LessEqual))
            }
            _ => Ok(None),
        }
    }

    fn match_addition(&mut self) -> ParserResult<Option<BinaryOperator>> {
        match self.peek_token_type()? {
            Plus => {
                if !self.match_next(Equal)? {
                    self.next();
                    return Ok(Some(BinaryOperator::Plus));
                }
                Ok(None)
            }
            Minus => {
                self.next();
                Ok(Some(BinaryOperator::Minus))
            }
            _ => Ok(None),
        }
    }

    fn match_multiplication(&mut self) -> ParserResult<Option<BinaryOperator>> {
        match self.peek_token_type()? {
            Star => {
                self.next();
                Ok(Some(BinaryOperator::Star))
            }
            Slash => {
                self.next();
                Ok(Some(BinaryOperator::Slash))
            }
            Percent => {
                self.next();
                Ok(Some(BinaryOperator::Percent))
            }
            _ => Ok(None),
        }
    }

    fn match_unary(&mut self) -> ParserResult<Option<UnaryOperator>> {
        match self.peek_token_type()? {
            Minus => {
                self.next();
                Ok(Some(UnaryOperator::Minus))
            }
            Bang => {
                self.next();
                Ok(Some(UnaryOperator::Bang))
            }
            _ => Ok(None),
        }
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.comparison()?;

        while let Some(op) = self.match_equality()? {
            let right = self.comparison()?;
            expr_ctx = BinaryExpr::new(self.pos.clone(), expr_ctx, op, right);
        }

        Ok(expr_ctx)
    }

    // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    fn comparison(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.addition()?;

        while let Some(op) = self.match_comparison()? {
            let right = self.addition()?;
            expr_ctx = BinaryExpr::new(self.pos.clone(), expr_ctx, op, right);
        }

        Ok(expr_ctx)
    }

    fn addition(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.multiplication()?;

        while let Some(op) = self.match_addition()? {
            let right = self.multiplication()?;
            expr_ctx = BinaryExpr::new(self.pos.clone(), expr_ctx, op, right);
        }

        Ok(expr_ctx)
    }

    fn multiplication(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.unary()?;

        while let Some(op) = self.match_multiplication()? {
            let right = self.unary()?;
            expr_ctx = BinaryExpr::new(self.pos.clone(), expr_ctx, op, right);
        }

        Ok(expr_ctx)
    }

    fn unary(&mut self) -> ParserResult<ExprCtx> {
        if let Some(op) = self.match_unary()? {
            let right = self.unary()?;
            return Ok(UnaryExpr::new(self.pos.clone(), op, right));
        }

        self.call()
    }

    fn call(&mut self) -> ParserResult<ExprCtx> {
        let mut expr_ctx = self.primary()?;

        loop {
            if self.consume(LeftParen)? {
                expr_ctx = self.finish_call(expr_ctx)?;
            } else if self.consume(LeftBracket)? {
                // array access
                let prop = self
                    .identifiers
                    .next_with_handle(Identifier::get());
                expr_ctx = self.finish_array_access(GetExpr::new(self.pos.clone(), prop, expr_ctx))?;
            } else if self.consume(Dot)? {
                if let Some(prop) = self.consume_identifier()? {
                    expr_ctx = GetExpr::new(self.pos.clone(), prop, expr_ctx);
                } else {
                    return Err(ParserError::ExpectedPropertyNameAfterDot(self.pos.clone()));
                }
            } else {
                break;
            }
        }

        Ok(expr_ctx)
    }

    fn finish_call(&mut self, expr_ctx: ExprCtx) -> ParserResult<ExprCtx> {
        let mut args = Vec::new();

        if !self.match_next(RightParen)? {
            loop {
                args.push(self.expression()?);
                if !self.consume(Comma)? {
                    break;
                }
            }
        }

        if !self.consume(RightParen)? {
            return Err(ParserError::ExpectedRightParenAfterCallExpr(
                self.pos.clone(),
            ));
        }

        Ok(CallExpr::new(self.pos.clone(), expr_ctx, args))
    }

    fn finish_array_access(&mut self, expr_ctx: ExprCtx) -> ParserResult<ExprCtx> {
        let mut args = Vec::new();

        if !self.match_next(RightParen)? {
            loop {
                args.push(self.expression()?);
                if !self.consume(Comma)? {
                    break;
                }
            }
        }

        if !self.consume(RightBracket)? {
            return Err(ParserError::ExpectedRightParenAfterCallExpr(
                self.pos.clone(),
            ));
        }

        Ok(CallExpr::new(self.pos.clone(), expr_ctx, args))
    }

    fn primary(&mut self) -> ParserResult<ExprCtx> {
        let next = self.next().unwrap().unwrap();

        match next.token_type {
            Nil => {
                return Ok(Literal::new(self.pos.clone(), Literal::Nil));
            }
            True => {
                return Ok(Literal::new(self.pos.clone(), Literal::Boolean(true)));
            }
            False => {
                return Ok(Literal::new(self.pos.clone(), Literal::Boolean(false)));
            }
            Number(nb) => {
                return Ok(Literal::new(self.pos.clone(), Literal::Number(nb.clone())));
            }
            String(s) => {
                return Ok(Literal::new(self.pos.clone(), Literal::String(s.clone())));
            }
            LeftParen => {
                let expr_ctx = self.expression()?;
                if self.consume(RightParen)? {
                    return Ok(GroupingExpr::new(self.pos.clone(), expr_ctx));
                } else {
                    return Err(ParserError::UnmatchingClosingParen(self.pos.clone()));
                }
            }
            Identifier(name) => {
                return Ok(VarExpr::new(
                    self.pos.clone(),
                    self.identifiers.next_with_name(&name),
                ))
            }
            Fun => self.function_declaration(),
            This => Ok(ThisExpr::new(
                self.pos.clone(),
                self
                    .identifiers
                    .next_with_handle(Identifier::this()),
            )),
            Super => {
                if self.consume(Dot)? {
                    if let Some(method) = self.consume_identifier()? {
                        return Ok(SuperExpr::new(
                            self.pos.clone(), 
                            self.identifiers
                                .next_with_handle(Identifier::super_()),
                            method,
                        ));
                    }
                }

                Err(ParserError::ExpectedSuperclassMethodName(self.pos.clone()))
            }
            _ => Err(ParserError::UnexpectedToken(self.pos.clone(), next.lexeme)),
        }
    }

    fn synchronize(&mut self) {
        while let Some(Ok(tok)) = self.tokens.peek() {
            if tok.token_type == SemiColon {
                return;
            }

            // TODO: return ¨ParseErrors instead of printing in place
            match self.next() {
                Some(Ok(t)) => match t.token_type {
                    Class | Fun | Var | For | If | While | Print | Return => return,
                    _ => continue,
                },
                Some(Err(e)) => println!("scanner error: {}", e),
                None => println!("could not synchronize : no more tokens"),
            }
        }
    }
}
