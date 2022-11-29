//! Defines [`SimpleOp`], a simple lambda calculus which can be used with babble.

use std::{
    convert::Infallible,
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};

use egg::Symbol;

use crate::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};

/// Simplest language to use with babble
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimpleOp {
	/// An integer literal
	Int(i32),
	/// A boolean literal
	Bool(bool),
    /// A conditional expression
    If,
    /// A function application
    Apply,
    /// A de Bruijn-indexed variable
    Var(DeBruijnIndex),
    /// A reference to a lib fn
    LibVar(LibId),
    /// An uninterpreted symbol
    Symbol(Symbol),
    /// An anonymous function
    Lambda,
    /// A library function binding
    Lib(LibId),
    /// A shift
    Shift,
    /// A list of expressions
    List,
}

impl Arity for SimpleOp {
    fn min_arity(&self) -> usize {
        match self {
            Self::Var(_) | Self::Symbol(_) | Self::Int(_) | Self::Bool(_) => 0,
            Self::Lambda | Self::Shift | Self::LibVar(_) | Self::List => 1,
            Self::Apply | Self::Lib(_) => 2,
            Self::If => 3,
        }
    }

	fn max_arity(&self) -> Option<usize> {
		match self {
			Self::List => None,
			other => Some(other.min_arity()),
		}
	}
}

impl Display for SimpleOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Apply => "@",
            Self::Lambda => "λ",
            Self::If => "if",
            Self::Shift => "shift",
            Self::Lib(libid) => {
                return write!(f, "lib {}", libid);
            }
            Self::LibVar(libid) => {
                return write!(f, "l{}", libid);
            }
			Self::Bool(b) => {
                return write!(f, "{}", b);
            }
            Self::Int(i) => {
                return write!(f, "{}", i);
            }
            Self::Var(index) => {
                return write!(f, "${}", index);
            }
            Self::Symbol(sym) => {
                return write!(f, "{}", sym);
            }
            Self::List => "list",
        };
        f.write_str(s)
    }
}

impl FromStr for SimpleOp {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "shift" => Self::Shift,
            "apply" | "@" => Self::Apply,
            "lambda" | "λ" => Self::Lambda,
            "list" => Self::List,
            input => input
                .parse()
                .map(Self::Var)
                .or_else(|_| input.parse().map(Self::LibVar))
                .or_else(|_| input.parse().map(Self::Int))
                .or_else(|_| input.parse().map(Self::Bool))
                .or_else(|_| {
                    input
                        .strip_prefix("lib ")
                        .ok_or(ParseLibIdError::NoLeadingL)
                        .and_then(|x| x.parse().map(Self::Lib))
                })
                .unwrap_or_else(|_| Self::Symbol(input.into())),
        };
        Ok(op)
    }
}

impl Teachable for SimpleOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(index)),
            BindingExpr::Lib(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
            BindingExpr::LibVar(ix) => AstNode::new(Self::LibVar(ix), []),
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (Self::Var(index), []) => BindingExpr::Var(*index),
            (Self::Lib(ix), [bound_value, body]) => BindingExpr::Lib(*ix, bound_value, body),
            (Self::LibVar(ix), []) => BindingExpr::LibVar(*ix),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }

    fn list() -> Self {
        Self::List
    }
}

impl Printable for SimpleOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Bool(_) | Self::Int(_) | Self::Symbol(_)
                | Self::Var(_) | Self::LibVar(_) => 60,
            Self::List => 50,
            Self::Apply | Self::Shift => 40,
            Self::If => 20,
            Self::Lambda | Self::Lib(_) => 10,
        }
    }

    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
        match (expr.0.operation(), expr.0.args()) {
            (&Self::Int(i), []) => {
                write!(printer.writer, "{}", i)
            }
            (&Self::Bool(b), []) => {
                write!(printer.writer, "{}", b)
            }
            (&Self::Symbol(sym), []) => {
                write!(printer.writer, "{}", sym)
            }
            (&Self::If, [cond, then, els]) => {
                printer.writer.write_str("if ")?;
                printer.print_in_context(cond, 0)?; // children do not need parens
                printer.writer.write_str(" then ")?;
                printer.print_in_context(then, 0)?;
                printer.writer.write_str(" else ")?;
                printer.print_in_context(els, 0)
            }
            (&Self::List, ts) => {
                let elem = |p: &mut Printer<W>, i: usize| {
                    p.print_in_context(&ts[i], 0) // children do not need parens
                };
                printer.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")))
            }
            (op, _) => write!(printer.writer, "{} ???", op),
        }
    }
}