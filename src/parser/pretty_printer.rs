use crate::parser::expressions::*;
use crate::parser::statements::*;

pub trait PrettyPrinter {
    fn pretty_print(&self) -> String;
}

impl PrettyPrinter for Expr {
    fn pretty_print(&self) -> String {
        match self {
            Expr::Unary(u) => u.pretty_print(),
            Expr::Binary(b) => b.pretty_print(),
            Expr::Grouping(g) => g.pretty_print(),
            Expr::Literal(l) => l.pretty_print(),
            Expr::Var(v) => v.pretty_print(),
            Expr::Assign(a) => a.pretty_print(),
            Expr::Logical(l) => l.pretty_print(),
            Expr::Call(c) => c.pretty_print(),
            Expr::Func(f) => f.pretty_print(),
            Expr::Get(g) => g.pretty_print(),
            Expr::Set(s) => s.pretty_print(),
            Expr::This(_) => "this".into(),
            Expr::Super(_) => "super".into(),
        }
    }
}

impl PrettyPrinter for UnaryExpr {
    fn pretty_print(&self) -> String {
        format!("({:?} {})", self.operator, self.right.expr.pretty_print())
    }
}

impl PrettyPrinter for GroupingExpr {
    fn pretty_print(&self) -> String {
        format!("(group {})", self.expression.expr.pretty_print())
    }
}

impl PrettyPrinter for BinaryExpr {
    fn pretty_print(&self) -> String {
        format!(
            "({:?} {} {})",
            self.operator.op,
            self.left.expr.pretty_print(),
            self.right.expr.pretty_print()
        )
    }
}

impl PrettyPrinter for LogicalExpr {
    fn pretty_print(&self) -> String {
        format!(
            "(LogicalExpr {} {:?} {})",
            self.left.expr.pretty_print(),
            self.operator,
            self.right.expr.pretty_print()
        )
    }
}

impl PrettyPrinter for Literal {
    fn pretty_print(&self) -> String {
        match self {
            Literal::Nil => "nil".to_owned(),
            Literal::Boolean(b) => {
                if *b {
                    "(boolean true)".to_owned()
                } else {
                    "(boolean false)".to_owned()
                }
            }
            Literal::Number(nb) => format!("(number {})", nb),
            Literal::String(s) => format!("(string {})", s),
        }
    }
}

impl PrettyPrinter for VarExpr {
    fn pretty_print(&self) -> String {
        format!("{}", self.identifier)
    }
}

impl PrettyPrinter for AssignExpr {
    fn pretty_print(&self) -> String {
        String::from(format!(
            "(AssignExpr {} {})",
            self.identifier,
            self.expr.expr.pretty_print()
        ))
    }
}

impl PrettyPrinter for CallExpr {
    fn pretty_print(&self) -> String {
        let args = self
            .args
            .iter()
            .map(|arg| arg.expr.pretty_print())
            .collect::<String>();

        format!("(CallExpr {}({}))", self.callee.expr.pretty_print(), args)
    }
}

impl PrettyPrinter for FuncExpr {
    fn pretty_print(&self) -> String {
        format!(
            "(FuncExpr {} {:?} => {})",
            if let Some(identifier) = &self.name {
                format!("{}, ", identifier)
            } else {
                String::from("0")
            },
            self.params,
            self.body
                .iter()
                .map(|stmt| format!("{}, ", stmt.pretty_print()))
                .collect::<String>()
        )
    }
}

impl PrettyPrinter for GetExpr {
    fn pretty_print(&self) -> String {
        format!("(GetExpr {}.{})", self.object.expr.pretty_print(), self.property)
    }
}

impl PrettyPrinter for SetExpr {
    fn pretty_print(&self) -> String {
        format!(
            "(SetExpr {}.{} <- {})",
            self.object.expr.pretty_print(),
            self.property,
            self.value.expr.pretty_print()
        )
    }
}

impl PrettyPrinter for Stmt {
    fn pretty_print(&self) -> String {
        match self {
            Stmt::Print(stmt) => stmt.pretty_print(),
            Stmt::Expr(stmt) => stmt.pretty_print(),
            Stmt::VarDecl(stmt) => stmt.pretty_print(),
            Stmt::Block(stmt) => stmt.pretty_print(),
            Stmt::If(stmt) => stmt.pretty_print(),
            Stmt::While(stmt) => stmt.pretty_print(),
            Stmt::Return(stmt) => stmt.pretty_print(),
            Stmt::ClassDecl(stmt) => stmt.pretty_print(),
        }
    }
}

impl PrettyPrinter for PrintStmt {
    fn pretty_print(&self) -> String {
        format!("(Print {})", self.value.expr.pretty_print())
    }
}

impl PrettyPrinter for ExprStmt {
    fn pretty_print(&self) -> String {
        format!("(ExprStmt {})", self.expr.expr.pretty_print())
    }
}

impl PrettyPrinter for VarDeclStmt {
    fn pretty_print(&self) -> String {
        if let Some(init) = &self.initializer {
            format!(
                "(VarDeclStmt {} <- {:?})",
                self.identifier,
                init.expr.pretty_print()
            )
        } else {
            format!("(VarDeclStmt {} <- nil)", self.identifier)
        }
    }
}

impl PrettyPrinter for BlockStmt {
    fn pretty_print(&self) -> String {
        let mut block = "".to_string();

        for stmt in &self.stmts {
            block.push_str(&format!("   {}\n", &stmt.pretty_print()));
        }

        format!("(BlockStmt\n{})", block)
    }
}

impl PrettyPrinter for IfStmt {
    fn pretty_print(&self) -> String {
        if let Some(else_branch) = &self.else_branch {
            format!(
                "(IfStmt ({}) => ({}) else ({}))",
                self.condition.expr.pretty_print(),
                self.then_branch.pretty_print(),
                else_branch.pretty_print()
            )
        } else {
            format!(
                "(IfStmt ({}) => ({}))",
                self.condition.expr.pretty_print(),
                self.then_branch.pretty_print()
            )
        }
    }
}

impl PrettyPrinter for WhileStmt {
    fn pretty_print(&self) -> String {
        format!(
            "(WhileStmt {} => {})",
            self.condition.expr.pretty_print(),
            self.body.pretty_print()
        )
    }
}

impl PrettyPrinter for ReturnStmt {
    fn pretty_print(&self) -> String {
        if let Some(ret) = &self.value {
            return format!("(ReturnStmt {})", ret.expr.pretty_print());
        }

        format!("(ReturnStmt nil)")
    }
}

impl PrettyPrinter for ClassDeclStmt {
    fn pretty_print(&self) -> String {
        format!(
            "(ClassDeclStmt {} {})",
            self.identifier,
            self.methods
                .iter()
                .map(|stmt| format!("{}, ", stmt.pretty_print()))
                .collect::<String>()
        )
    }
}
