use crate::parser::expressions::*;
use crate::parser::statements::*;
use crate::parser::{IdentifierNames, IdentifierUse};

pub trait PrettyPrinter {
    fn pretty_print(&self, names: &IdentifierNames) -> String;
}

impl PrettyPrinter for Expr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        match self {
            Expr::Unary(u) => u.pretty_print(names),
            Expr::Binary(b) => b.pretty_print(names),
            Expr::Grouping(g) => g.pretty_print(names),
            Expr::Literal(l) => l.pretty_print(names),
            Expr::Var(v) => v.pretty_print(names),
            Expr::Assign(a) => a.pretty_print(names),
            Expr::Logical(l) => l.pretty_print(names),
            Expr::Call(c) => c.pretty_print(names),
            Expr::Func(f) => f.pretty_print(names),
            Expr::Get(g) => g.pretty_print(names),
            Expr::Set(s) => s.pretty_print(names),
            Expr::This(_) => "this".into(),
            Expr::Super(_) => "super".into(),
            Expr::ArrayDeclExpr(a) => a.pretty_print(names),
        }
    }
}

impl PrettyPrinter for IdentifierUse {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!("{}:{}", names[self.name], self.use_handle)
    }
}

impl PrettyPrinter for UnaryExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "({:?} {})",
            self.operator,
            self.right.expr.pretty_print(names)
        )
    }
}

impl PrettyPrinter for GroupingExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!("(group {})", self.expression.expr.pretty_print(names))
    }
}

impl PrettyPrinter for BinaryExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "({:?} {} {})",
            self.operator.op,
            self.left.expr.pretty_print(names),
            self.right.expr.pretty_print(names)
        )
    }
}

impl PrettyPrinter for LogicalExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(LogicalExpr {} {:?} {})",
            self.left.expr.pretty_print(names),
            self.operator,
            self.right.expr.pretty_print(names)
        )
    }
}

impl PrettyPrinter for Literal {
    fn pretty_print(&self, _names: &IdentifierNames) -> String {
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
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!("{}", self.identifier.pretty_print(names))
    }
}

impl PrettyPrinter for AssignExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        String::from(format!(
            "(AssignExpr {} {})",
            self.identifier.pretty_print(names),
            self.expr.expr.pretty_print(names)
        ))
    }
}

impl PrettyPrinter for CallExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        let args = self
            .args
            .iter()
            .map(|arg| arg.expr.pretty_print(names))
            .collect::<String>();

        format!(
            "(CallExpr {}({}))",
            self.callee.expr.pretty_print(names),
            args
        )
    }
}

impl PrettyPrinter for FuncExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(FuncExpr {} {:?} => {})",
            if let Some(identifier) = &self.name {
                format!("{}, ", identifier.pretty_print(names))
            } else {
                String::from("anonymous")
            },
            self.params
                .clone()
                .unwrap_or_default()
                .iter()
                .map(|p| format!("{}", p.identifier().pretty_print(names)))
                .collect::<String>(),
            self.body
                .iter()
                .map(|stmt| format!("{}, ", stmt.pretty_print(names)))
                .collect::<String>()
        )
    }
}

impl PrettyPrinter for GetExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(GetExpr {}.{})",
            self.object.expr.pretty_print(names),
            self.property.pretty_print(names)
        )
    }
}

impl PrettyPrinter for SetExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(SetExpr {}.{} <- {})",
            self.object.expr.pretty_print(names),
            self.property.pretty_print(names),
            self.value.expr.pretty_print(names)
        )
    }
}

impl PrettyPrinter for ArrayDeclExpr {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(ArrayDeclExpr {:?})",
            self.values
                .iter()
                .map(|val| val.expr.pretty_print(names))
                .collect::<Vec<_>>()
        )
    }
}

impl PrettyPrinter for Stmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        match self {
            Stmt::Print(stmt) => stmt.pretty_print(names),
            Stmt::Expr(stmt) => stmt.pretty_print(names),
            Stmt::VarDecl(stmt) => stmt.pretty_print(names),
            Stmt::Block(stmt) => stmt.pretty_print(names),
            Stmt::If(stmt) => stmt.pretty_print(names),
            Stmt::While(stmt) => stmt.pretty_print(names),
            Stmt::Return(stmt) => stmt.pretty_print(names),
            Stmt::ClassDecl(stmt) => stmt.pretty_print(names),
        }
    }
}

impl PrettyPrinter for PrintStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!("(Print {})", self.value.expr.pretty_print(names))
    }
}

impl PrettyPrinter for ExprStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!("(ExprStmt {})", self.expr.expr.pretty_print(names))
    }
}

impl PrettyPrinter for VarDeclStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        if let Some(init) = &self.initializer {
            format!(
                "(VarDeclStmt {} <- {:?})",
                self.identifier.pretty_print(names),
                init.expr.pretty_print(names)
            )
        } else {
            format!("(VarDeclStmt {} <- nil)", self.identifier)
        }
    }
}

impl PrettyPrinter for BlockStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        let mut block = "".to_string();

        for stmt in &self.stmts {
            block.push_str(&format!("   {}\n", &stmt.pretty_print(names)));
        }

        format!("(BlockStmt\n{})", block)
    }
}

impl PrettyPrinter for IfStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        if let Some(else_branch) = &self.else_branch {
            format!(
                "(IfStmt ({}) => ({}) else ({}))",
                self.condition.expr.pretty_print(names),
                self.then_branch.pretty_print(names),
                else_branch.pretty_print(names)
            )
        } else {
            format!(
                "(IfStmt ({}) => ({}))",
                self.condition.expr.pretty_print(names),
                self.then_branch.pretty_print(names)
            )
        }
    }
}

impl PrettyPrinter for WhileStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(WhileStmt {} => {})",
            self.condition.expr.pretty_print(names),
            self.body.pretty_print(names)
        )
    }
}

impl PrettyPrinter for ReturnStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        if let Some(ret) = &self.value {
            return format!("(ReturnStmt {})", ret.expr.pretty_print(names));
        }

        format!("(ReturnStmt nil)")
    }
}

impl PrettyPrinter for ClassDeclStmt {
    fn pretty_print(&self, names: &IdentifierNames) -> String {
        format!(
            "(ClassDeclStmt {} {})",
            self.identifier.pretty_print(names),
            self.methods
                .iter()
                .map(|stmt| format!("{}, ", stmt.pretty_print(names)))
                .collect::<String>()
        )
    }
}
