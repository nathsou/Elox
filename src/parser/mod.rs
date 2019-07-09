pub mod expressions;
pub mod parser_result;
pub mod pretty_printer;
pub mod statements;
use crate::scanner::token::token_type::{TokenType, TokenType::*};
use crate::scanner::Scanner;
use expressions::*;
use fnv::FnvHashMap;
use parser_result::{ParserError, ParserResult};
use statements::*;
use std::iter::Peekable;

pub type IdentifierHandle = usize;
pub type IdentifierUseHandle = usize;

#[derive(Debug)]
pub struct IdentifierHandlesGenerator {
    handles: FnvHashMap<std::string::String, IdentifierHandle>,
    next_id_handle: IdentifierHandle,
    next_use_handle: IdentifierUseHandle,
}

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
}

impl IdentifierHandlesGenerator {
    pub fn new() -> IdentifierHandlesGenerator {
        let mut handles = FnvHashMap::default();

        handles.insert(std::string::String::from("this"), Identifier::this());
        handles.insert(std::string::String::from("init"), Identifier::init());
        handles.insert(std::string::String::from("super"), Identifier::super_());
        handles.insert(std::string::String::from("Array"), Identifier::array());

        IdentifierHandlesGenerator {
            next_id_handle: handles.len(),
            handles,
            next_use_handle: 0,
        }
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

        return curr;
    }

    pub fn next_use_handle(&mut self) -> IdentifierUseHandle {
        let curr = self.next_use_handle;
        self.next_use_handle += 1;

        return curr;
    }

    pub fn by_name(&mut self, name: &str) -> IdentifierHandle {
        if let Some(handle) = self.handles.get(name) {
            return *handle;
        }

        let handle = self.next_id_handle();
        self.handles.insert(name.into(), handle);

        // println!("{} -> {}", name, handle);

        return handle;
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
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Scanner<'a>>, identifiers: IdentifierHandlesGenerator) -> Self {
        Parser {
            tokens,
            identifiers,
        }
    }

    pub fn parse(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(Ok(tok)) = self.tokens.peek() {
            match tok.token_type {
                EOF => break,
                _ => {
                    let stmt = self.declaration()?;
                    stmts.push(stmt);
                }
            }
        }

        Ok(stmts)
    }

    fn consume(&mut self, token_type: TokenType) -> bool {
        if let Some(Ok(token)) = self.tokens.peek() {
            if token.token_type == token_type {
                self.tokens.next();
                return true;
            }
        }

        false
    }

    fn match_next(&mut self, token_type: TokenType) -> bool {
        if let Some(Ok(token)) = self.tokens.peek() {
            return token.token_type == token_type;
        }

        false
    }

    fn declaration(&mut self) -> ParserResult<Stmt> {
        let mut res: Option<ParserResult<Stmt>> = None;

        if let Some(Ok(token)) = self.tokens.peek() {
            match token.token_type {
                Fun => {
                    self.tokens.next();
                    let f = self.function_declaration()?;
                    res = Some(Ok(ExprStmt::to_stmt(f)));
                }
                Var => {
                    self.tokens.next();
                    res = Some(self.var_declaration());
                }
                Class => {
                    self.tokens.next();
                    res = Some(self.class_declaration());
                }
                _ => {}
            }
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
        if let Some(name) = self.consume_identifier() {
            let mut superclass = None;

            if self.consume(Less) {
                if let Some(parent_class) = self.consume_identifier() {
                    superclass = Some(VarExpr {
                        identifier: parent_class,
                    });
                } else {
                    return Err(ParserError::ExpectedSuperclassName());
                }
            }

            if self.consume(LeftBrace) {
                let mut methods = Vec::new();

                while !self.match_next(RightBrace) {
                    let func = self.function_declaration()?;
                    match func {
                        Expr::Func(f) => {
                            methods.push(f);
                        }
                        _ => return Err(ParserError::ExpectedMethodDeclarationInClass(name.name)),
                    }
                }

                if !self.consume(RightBrace) {
                    return Err(ParserError::ExpectedRightBraceAfterClassBody());
                }

                return Ok(ClassDeclStmt::to_stmt(name, superclass, methods));
            } else {
                return Err(ParserError::ExpectedLeftBraceBeforeClassBody());
            }
        } else {
            return Err(ParserError::ExpectedClassName());
        }
    }

    fn function_declaration(&mut self) -> ParserResult<Expr> {
        let name = self.consume_identifier();

        if self.consume(LeftParen) {
            let mut params = Vec::new();

            if !self.match_next(RightParen) {
                loop {
                    if let Some(param) = self.consume_identifier() {
                        params.push(param);
                    } else {
                        return Err(ParserError::ExpectedFuncParamName());
                    }

                    if !self.consume(Comma) {
                        break;
                    }
                }
            }

            if !self.consume(RightParen) {
                return Err(ParserError::ExpectedRightParenAfterCallExpr());
            }

            if !self.consume(LeftBrace) {
                return Err(ParserError::ExpectedLeftBraceBeforeFuncBody());
            }

            let body = ((self.block()?) as BlockStmt).stmts;

            return Ok(FuncExpr::new(name, params, body));
        } else {
            Err(ParserError::ExpectedRightParenAfterLoop())
        }
    }

    fn match_identifier(&mut self) -> Option<IdentifierUse> {
        if let Some(Ok(token)) = self.tokens.peek() {
            if let Identifier(name) = &token.token_type {
                return Some(IdentifierUse::new(
                    self.identifiers.by_name(name),
                    self.identifiers.next_use_handle(),
                ));
            }
        }

        None
    }

    fn consume_identifier(&mut self) -> Option<IdentifierUse> {
        if let Some(identifier) = self.match_identifier() {
            self.tokens.next();
            return Some(identifier);
        }

        None
    }

    fn var_declaration(&mut self) -> ParserResult<Stmt> {
        if let Some(identifier) = self.match_identifier() {
            self.tokens.next();
            let mut initializer: Option<Expr> = None;

            if self.consume(Equal) {
                initializer = Some(self.expression()?);
            }

            if !self.consume(SemiColon) {
                return Err(ParserError::ExpectedSemicolonAfterExpr());
            }

            return Ok(VarDeclStmt::to_stmt(identifier, initializer));
        } else {
            if let Some(Ok(tok)) = self.tokens.peek() {
                return Err(ParserError::ExpectedVarName(tok.token_type.clone()));
            } else {
                return Err(ParserError::ExpectedStatement());
            }
        }
    }

    fn statement(&mut self) -> ParserResult<Stmt> {
        if let Some(Ok(token)) = self.tokens.peek() {
            match token.token_type {
                For => {
                    self.tokens.next();
                    return self.for_stmt();
                }
                If => {
                    self.tokens.next();
                    return self.if_stmt();
                }
                Print => {
                    // consumme the print token
                    self.tokens.next();
                    return self.print_stmt();
                }
                Return => {
                    self.tokens.next();
                    return self.return_stmt();
                }
                While => {
                    self.tokens.next();
                    return self.while_stmt();
                }
                LeftBrace => {
                    self.tokens.next();
                    return Ok(Stmt::Block(self.block()?));
                }
                _ => return self.expr_stmt(),
            }
        }

        Err(ParserError::ExpectedStatement())
    }

    fn return_stmt(&mut self) -> ParserResult<Stmt> {
        let mut value: Option<Expr> = None;

        if !self.match_next(SemiColon) {
            value = Some(self.expression()?);
        }

        if !self.consume(SemiColon) {
            return Err(ParserError::ExpectedSemiColonAfterReturnValue());
        }

        Ok(ReturnStmt::to_stmt(value))
    }

    // forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    fn for_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen) {
            let mut initializer;

            if self.consume(Var) {
                initializer = Some(self.var_declaration()?);
            } else {
                initializer = Some(self.expr_stmt()?);
            }

            let mut condition = None;

            if !self.match_next(SemiColon) {
                condition = Some(self.expression()?);
            }

            if !self.consume(SemiColon) {
                return Err(ParserError::ExpectedSemicolonAfterLoopCondition());
            }

            let mut increment = None;

            if !self.match_next(RightParen) {
                increment = Some(self.expression()?);
            }

            if !self.consume(RightParen) {
                return Err(ParserError::ExpectedRightParenAfterForClauses());
            }

            let mut body = self.statement()?;

            if let Some(inc) = increment {
                body = BlockStmt::to_stmt(vec![body, ExprStmt::to_stmt(inc)]);
            }

            body = WhileStmt::to_stmt(
                if let Some(cond) = condition {
                    cond
                } else {
                    Expr::Literal(Literal::Boolean(true))
                },
                body,
            );

            if let Some(init) = initializer {
                body = BlockStmt::to_stmt(vec![init, body]);
            }

            Ok(body)
        } else {
            Err(ParserError::ExpectedLeftParenAfterLoop())
        }
    }

    // whileStmt → "while" "(" expression ")" statement ;
    fn while_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen) {
            let condition = self.expression()?;
            if self.consume(RightParen) {
                let body = self.statement()?;

                Ok(WhileStmt::to_stmt(condition, body))
            } else {
                Err(ParserError::ExpectedRightParenAfterLoop())
            }
        } else {
            Err(ParserError::ExpectedLeftParenAfterLoop())
        }
    }

    fn or_expr(&mut self) -> ParserResult<Expr> {
        let mut expr = self.and_expr()?;

        while self.consume(Or) {
            let right = self.and_expr()?;
            expr = LogicalExpr::new(expr, LogicalOperator::Or, right);
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> ParserResult<Expr> {
        let mut expr = self.equality()?;

        while self.consume(And) {
            let right = self.equality()?;
            expr = LogicalExpr::new(expr, LogicalOperator::And, right);
        }

        Ok(expr)
    }

    fn if_stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume(LeftParen) {
            let condition = self.expression()?;
            if self.consume(RightParen) {
                let then_branch = self.statement()?;
                let mut else_branch = None;

                if self.consume(Else) {
                    else_branch = Some(self.statement()?);
                }

                Ok(IfStmt::to_stmt(condition, then_branch, else_branch))
            } else {
                Err(ParserError::ExpectedRightParenAfterIf())
            }
        } else {
            Err(ParserError::ExpectedLeftParenAfterIf())
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

        if self.consume(RightBrace) {
            return Ok(BlockStmt { stmts });
        }

        Err(ParserError::ExpectedRightBraceAfterBlock())
    }

    fn print_stmt(&mut self) -> ParserResult<Stmt> {
        let value = self.expression()?;

        if self.consume(SemiColon) {
            return Ok(PrintStmt::to_stmt(value));
        }

        Err(ParserError::ExpectedSemicolonAfterExpr())
    }

    fn expr_stmt(&mut self) -> ParserResult<Stmt> {
        let expr = self.expression()?;

        if self.consume(SemiColon) {
            return Ok(ExprStmt::to_stmt(expr));
        }

        Err(ParserError::ExpectedSemicolonAfterExpr())
    }

    // expression     → equality ;
    fn expression(&mut self) -> ParserResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Expr> {
        let expr = self.or_expr()?;

        if self.consume(Equal) {
            let value = self.assignment()?;

            match expr {
                Expr::Var(v) => return Ok(AssignExpr::new(v.identifier, value)),
                Expr::Get(g) => return Ok(SetExpr::new(g.property, g.object, value)),
                _ => return Err(ParserError::InvalidAssignmentTarget()),
            }
        } else {
            return Ok(expr);
        }
    }

    fn match_equality(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(t)) = self.tokens.peek() {
            match t.token_type {
                BangEqual => {
                    self.tokens.next();
                    return Some(BinaryOperator::BangEqual);
                }
                EqualEqual => {
                    self.tokens.next();
                    return Some(BinaryOperator::EqualEqual);
                }
                _ => return None,
            }
        }

        None
    }

    fn match_comparison(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(t)) = self.tokens.peek() {
            match t.token_type {
                Greater => {
                    self.tokens.next();
                    return Some(BinaryOperator::Greater);
                }
                GreaterEqual => {
                    self.tokens.next();
                    return Some(BinaryOperator::GreaterEqual);
                }
                Less => {
                    self.tokens.next();
                    return Some(BinaryOperator::Less);
                }
                LessEqual => {
                    self.tokens.next();
                    return Some(BinaryOperator::LessEqual);
                }
                _ => return None,
            }
        }

        None
    }

    fn match_addition(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(t)) = self.tokens.peek() {
            match t.token_type {
                Plus => {
                    self.tokens.next();
                    return Some(BinaryOperator::Plus);
                }
                Minus => {
                    self.tokens.next();
                    return Some(BinaryOperator::Minus);
                }
                _ => return None,
            }
        }

        None
    }

    fn match_multiplication(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(t)) = self.tokens.peek() {
            match t.token_type {
                Star => {
                    self.tokens.next();
                    return Some(BinaryOperator::Star);
                }
                Slash => {
                    self.tokens.next();
                    return Some(BinaryOperator::Slash);
                }
                Percent => {
                    self.tokens.next();
                    return Some(BinaryOperator::Percent);
                }
                _ => return None,
            }
        }

        None
    }

    fn match_unary(&mut self) -> Option<UnaryOperator> {
        if let Some(Ok(t)) = self.tokens.peek() {
            match t.token_type {
                Minus => {
                    self.tokens.next();
                    return Some(UnaryOperator::Minus);
                }
                Bang => {
                    self.tokens.next();
                    return Some(UnaryOperator::Bang);
                }
                _ => return None,
            }
        }

        None
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> ParserResult<Expr> {
        let mut expr = self.comparison()?;

        while let Some(op) = self.match_equality() {
            let right = self.comparison()?;
            expr = BinaryExpr::new(expr, op, right);
        }

        Ok(expr)
    }

    // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    fn comparison(&mut self) -> ParserResult<Expr> {
        let mut expr = self.addition()?;

        while let Some(op) = self.match_comparison() {
            let right = self.addition()?;
            expr = BinaryExpr::new(expr, op, right);
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParserResult<Expr> {
        let mut expr = self.multiplication()?;

        while let Some(op) = self.match_addition() {
            let right = self.multiplication()?;
            expr = BinaryExpr::new(expr, op, right);
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ParserResult<Expr> {
        let mut expr = self.unary()?;

        while let Some(op) = self.match_multiplication() {
            let right = self.unary()?;
            expr = BinaryExpr::new(expr, op, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult<Expr> {
        if let Some(op) = self.match_unary() {
            let right = self.unary()?;
            return Ok(UnaryExpr::new(op, right));
        }

        self.call()
    }

    fn call(&mut self) -> ParserResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.consume(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.consume(Dot) {
                if let Some(prop) = self.consume_identifier() {
                    expr = GetExpr::new(prop, expr);
                } else {
                    return Err(ParserError::ExpectedPropertyNameAfterDot());
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> ParserResult<Expr> {
        let mut args = Vec::new();

        if !self.match_next(RightParen) {
            loop {
                args.push(self.expression()?);
                if !self.consume(Comma) {
                    break;
                }
            }
        }

        if !self.consume(RightParen) {
            return Err(ParserError::ExpectedRightParenAfterCallExpr());
        }

        Ok(CallExpr::new(expr, args))
    }

    fn primary(&mut self) -> ParserResult<Expr> {
        let next = self.tokens.next().unwrap().unwrap().token_type.clone();

        match next {
            Nil => {
                return Ok(Expr::Literal(Literal::Nil));
            }
            True => {
                return Ok(Expr::Literal(Literal::Boolean(true)));
            }
            False => {
                return Ok(Expr::Literal(Literal::Boolean(false)));
            }
            Number(nb) => {
                return Ok(Expr::Literal(Literal::Number(nb.clone())));
            }
            String(s) => {
                return Ok(Expr::Literal(Literal::String(s.clone())));
            }
            LeftParen => {
                let expr = self.expression().unwrap();
                if self.consume(RightParen) {
                    return Ok(GroupingExpr::new(expr));
                } else {
                    return Err(ParserError::UnmatchingClosingParen());
                }
            }
            Identifier(name) => return Ok(VarExpr::new(self.identifiers.next_with_name(&name))),
            Fun => self.function_declaration(),
            This => Ok(Expr::This(ThisExpr {
                identifier: self.identifiers.next_with_handle(Identifier::this()),
            })),
            Super => {
                if self.consume(Dot) {
                    if let Some(method) = self.consume_identifier() {
                        return Ok(SuperExpr::new(
                            self.identifiers.next_with_handle(Identifier::super_()),
                            method,
                        ));
                    }
                }

                Err(ParserError::ExpectedSuperclassMethodName())
            }
            _ => Err(ParserError::UnexpectedToken(next)),
        }
    }

    fn synchronize(&mut self) {
        while let Some(Ok(tok)) = self.tokens.peek() {
            if tok.token_type == SemiColon {
                return;
            }

            match self.tokens.next() {
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
