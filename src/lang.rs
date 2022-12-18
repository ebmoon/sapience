//! Defines [`SimpleOp`], a simple lambda calculus which can be used with babble.

use std::{
    convert::Infallible,
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};
use crate::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use egg::{Id, Pattern, EGraph, Symbol};
use ruler::{
    map,
    util::*,
    SynthLanguage, 
    SynthAnalysis, 
    CVec, 
    ValidationResult,
};
use z3::ast::Ast;

/// Simplest language to use with babble
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum SimpleOp {
	/// An integer literal
	Int(i32),
	/// A boolean literal
	Bool(bool),
    /// A conditional expression
    If,
    /// A function application
    Apply,
    /// Integer Addition
    Op(Symbol),
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
            Self::Var(_) 
            | Self::LibVar(_)
            | Self::Symbol(_) 
            | Self::Int(_) 
            | Self::Bool(_)
            | Self::Op(_) => 0,
            Self::Lambda | Self::Shift | Self::List => 1,
            Self::Apply | Self::Lib(_) => 2,
            Self::If => 3,
        }
    }

	fn max_arity(&self) -> Option<usize> {
		match self {
			Self::List => None,
            Self::Op(_) => None,
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
            Self::Op(sym) => {
                return write!(f, "{}", sym);
            }
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
                return write!(f, "sym{}", sym);
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
            "if" => Self::If,
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
                .unwrap_or_else(|_| Self::Op(input.into())),
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
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
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
                | Self::Var(_) | Self::LibVar(_) | Self::Op(_) => 60,
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

impl SynthLanguage for AstNode<SimpleOp> {
    type Constant = i32;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where 
        F: FnMut(&'a Id) -> &'a CVec<Self> 
    {
        let args = self.args();
        match self.operation() {
            SimpleOp::Int(n) => vec![Some(*n); cvec_len],
            SimpleOp::Bool(b) => vec![Some(*b as i32); cvec_len],
            SimpleOp::Op(sym) if sym.as_str() == "neg" => {
                let x = &args[0];
                map!(get_cvec, x => Some(-x))
            },
            SimpleOp::Op(sym) if sym.as_str() == "+" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => Some(x + y))
            },
            SimpleOp::Op(sym) if sym.as_str() == "-" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => Some(x - y))
            },
            SimpleOp::Op(sym) if sym.as_str() == "*" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => Some(x * y))
            },
            SimpleOp::Op(sym) if sym.as_str() == "/" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if *y == 0i32 { None } else { Some(x / y) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == ">" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x > y { Some(1) } else { Some(0) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == "<" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x < y { Some(1) } else { Some(0) }
                })
            }, 
            SimpleOp::Op(sym) if sym.as_str() == ">=" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x >= y { Some(1) } else { Some(0) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == "<=" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x <= y { Some(1) } else { Some(0) }
                })
            },

            SimpleOp::Op(sym) if sym.as_str() == "==" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x == y { Some(1) } else { Some(0) }
                })
            }, 
            SimpleOp::Op(sym) if sym.as_str() == "!=" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if x != y { Some(1) } else { Some(0) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == "!" => {
                let x = &args[0];
                map!(get_cvec, x => {
                    if *x == 0i32 { Some(1) } else { Some(0) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == "&&" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if (*x != 0i32) && (*y != 0i32) { Some(1) } else { Some(0) }
                })
            },
            SimpleOp::Op(sym) if sym.as_str() == "||" => {
                let x = &args[0];
                let y = &args[1];
                map!(get_cvec, x, y => {
                    if (*x != 0i32) || (*y != 0i32) { Some(1) } else { Some(0) }
                })
            },
            _op => vec![],
        }
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts = vec![Some(-1), Some(0), Some(1), Some(10)];
        let cvecs = self_product(&consts, vars.len());

        for (i, v) in vars.iter().enumerate() {
            let var = Self::mk_var(Symbol::from(v.clone()));
            let id = egraph.add(var);
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        match self.operation() {
            SimpleOp::Op(v) if self.args().len() == 0 => Some(*v),
            _ => None,
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Self::new(SimpleOp::Op(sym), vec![])
    }

    fn is_constant(&self) -> bool {
        self.is_empty()
    }

    fn mk_constant(
        c: Self::Constant,
        _egraph: &mut EGraph<Self, SynthAnalysis>
    ) -> Self {
        Self::new(SimpleOp::Int(c), vec![])
    }

    fn validate(lhs: &Pattern<Self>, rhs: &Pattern<Self>) -> ValidationResult {
        // Future work: add type check
        let mut cfg = z3::Config::new();
        cfg.set_timeout_msec(1000);
        let ctx = z3::Context::new(&cfg);
        let solver = z3::Solver::new(&ctx);
        let lexpr = egg_to_z3(&ctx, Self::instantiate(lhs).as_ref());
        let rexpr = egg_to_z3(&ctx, Self::instantiate(rhs).as_ref());
        solver.assert(&lexpr._eq(&rexpr).not());
        match solver.check() {
            z3::SatResult::Unsat => ValidationResult::Valid,
            z3::SatResult::Unknown => ValidationResult::Unknown,
            z3::SatResult::Sat => ValidationResult::Invalid,
        }
    }
}

fn egg_to_z3<'a>(ctx: &'a z3::Context, expr: &[AstNode<SimpleOp>]) 
    -> z3::ast::Int<'a> 
{
    let mut buf: Vec<z3::ast::Int> = vec![];
    let zero = z3::ast::Int::from_i64(ctx, 0);
    let one = z3::ast::Int::from_i64(ctx, 1);
    for node in expr.as_ref().iter() {
        let args = node.args();
        match node.operation() {
            SimpleOp::Int(n) => buf.push(z3::ast::Int::from_i64(ctx, *n as i64)),
            SimpleOp::Bool(b) => buf.push(z3::ast::Int::from_i64(ctx, *b as i64)),
            SimpleOp::Op(sym) if sym.as_str() == "neg" => {
                let t = &buf[usize::from(args[0])];
                buf.push(z3::ast::Int::unary_minus(t))
            },
            SimpleOp::Op(sym) if sym.as_str() == "+" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Int::add(ctx, &[l, r]))
            },
            SimpleOp::Op(sym) if sym.as_str() == "-" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Int::sub(ctx, &[l, r]))
            },
            SimpleOp::Op(sym) if sym.as_str() == "*" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Int::mul(ctx, &[l, r]))
            },
            SimpleOp::Op(sym) if sym.as_str() == "/" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(
                    &r._eq(&zero),
                    &zero,
                    &z3::ast::Int::div(l, r)
                ))
            },
            SimpleOp::Op(sym) if sym.as_str() == ">" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::gt(l, r), &one, &zero))
            },
            SimpleOp::Op(sym) if sym.as_str() == "<" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::lt(l, r), &one, &zero))
            }, 
            SimpleOp::Op(sym) if sym.as_str() == ">=" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::ge(l, r), &one, &zero))
            },
            SimpleOp::Op(sym) if sym.as_str() == "<=" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::le(l, r), &one, &zero))
            },
            SimpleOp::Op(sym) if sym.as_str() == "==" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::_eq(l, r), &one, &zero))
            }, 
            SimpleOp::Op(sym) if sym.as_str() == "!=" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                buf.push(z3::ast::Bool::ite(&z3::ast::Int::_eq(l, r), &zero, &one))
            },
            SimpleOp::Op(sym) if sym.as_str() == "!" => {
                let t = &buf[usize::from(args[0])];
                buf.push(z3::ast::Bool::ite(&t._eq(&zero), &one, &zero))
            },
            SimpleOp::Op(sym) if sym.as_str() == "&&" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                let l_nonzero = z3::ast::Bool::not(&l._eq(&zero));
                let r_nonzero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::and(ctx, &[&l_nonzero, &r_nonzero]), 
                    &one, 
                    &zero
                ))
            },
            SimpleOp::Op(sym) if sym.as_str() == "||" => {
                let l = &buf[usize::from(args[0])];
                let r = &buf[usize::from(args[1])];
                let l_nonzero = z3::ast::Bool::not(&l._eq(&zero));
                let r_nonzero = z3::ast::Bool::not(&r._eq(&zero));
                buf.push(z3::ast::Bool::ite(
                    &z3::ast::Bool::or(ctx, &[&l_nonzero, &r_nonzero]), 
                    &one, 
                    &zero
                ))
            },
            SimpleOp::Op(sym) if node.is_empty() =>
                buf.push(z3::ast::Int::new_const(ctx, sym.as_str())),
            _op => unreachable!(),
        }
    }
    buf.pop().unwrap()
}