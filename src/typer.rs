use std::collections::HashMap;

use crate::parser::Expr;

type TypeVar = usize;
type Term = usize;
type Substitution = HashMap<TypeVar, Term>;

#[derive(Clone, Debug)]
pub enum Ty {
    Var(TypeVar),
    Term(Term),
}

#[derive(Clone, Debug)]
pub enum Monotype {
    Var(Ty),
    Func(Box<Monotype>, Box<Monotype>),
}

#[derive(Clone)]
pub struct Polytype {
    quantifiers: Vec<TypeVar>,
    mono: Monotype,
}

impl ToString for Monotype {
    fn to_string(&self) -> String {
        match self {
            Monotype::Var(Ty::Var(v)) | Monotype::Var(Ty::Term(v)) => v.to_string(),
            Monotype::Func(t1, t2) => {
                if let Monotype::Func(_, _) = **t1 {
                    format!("({}) -> {}", t1.to_string(), t2.to_string())
                } else {
                    format!("{} -> {}", t1.to_string(), t2.to_string())
                }
            }
        }
    }
}

impl Monotype {
    fn apply(&mut self, substitution: &Substitution) {
        match self {
            Monotype::Var(ty) => match ty {
                Ty::Var(v) => {
                    *ty = Ty::Term(*substitution.get(v).unwrap_or(v));
                }
                Ty::Term(_) => {}
            },
            Monotype::Func(t1, t2) => {
                t1.apply(substitution);
                t2.apply(substitution);
            }
        }
    }

    fn apply_backwards(&mut self, substitution: &Substitution) {
        match self {
            Monotype::Var(ty) => match ty {
                Ty::Term(v) => {
                    *ty = Ty::Var(*substitution.get(v).unwrap_or(v));
                }
                Ty::Var(_) => {}
            },
            Monotype::Func(t1, t2) => {
                t1.apply(substitution);
                t2.apply(substitution);
            }
        }
    }

    fn unify(&mut self, other: &mut Self) {
        match (self.clone(), other.clone()) {
            (Monotype::Func(mut self1, mut self2), Monotype::Func(mut other1, mut other2)) => {
                self1.unify(&mut other1);
                self2.unify(&mut other2);
                *other = self.clone();
            }
            (Monotype::Var(Ty::Term(_)), _) => {
                *other = self.clone();
            }
            _ => {
                *self = other.clone();
            }
        }
    }

    fn vars(&self) -> Vec<&Ty> {
        match self {
            Monotype::Var(v) => vec![v],
            Monotype::Func(t1, t2) => {
                let mut vars = t1.vars();
                vars.append(&mut t2.vars());
                vars
            }
        }
    }
}

impl Polytype {
    fn mono(ty: Monotype) -> Self {
        Self {
            quantifiers: Vec::new(),
            mono: ty,
        }
    }
}

#[derive(Default)]
pub struct Typer {
    gamma: HashMap<String, Polytype>,
    next_type: usize,
}

impl Typer {
    pub fn type_expr(expr: Expr) -> Monotype {
        let mut typer = Self::default();
        typer.expr(expr)
    }

    fn expr(&mut self, expr: Expr) -> Monotype {
        match expr {
            Expr::Var(x) => self.var(x),
            Expr::App { e1, e2 } => self.app(*e1, *e2),
            Expr::Abs { x, e } => {
                if let Expr::Var(x) = *x {
                    self.abs(x, *e)
                } else {
                    unreachable!()
                }
            }
            Expr::Let { x, e1, e2 } => {
                if let Expr::Var(x) = *x {
                    self.r#let(x, *e1, *e2)
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn newvar(&mut self) -> usize {
        self.next_type += 1;
        self.next_type - 1
    }

    // inst (forall a b. a -> b -> a) = c -> d -> c
    fn inst(&mut self, mut ty: Polytype) -> Monotype {
        let mut sub = Substitution::new();
        for q in &ty.quantifiers {
            sub.insert(*q, self.newvar());
        }
        ty.mono.apply(&sub);
        ty.mono
    }

    fn generalize(&mut self, ty: &Monotype) -> Polytype {
        let mut mono = ty.clone();
        let mut sub = Substitution::new();
        let mut quantifiers = Vec::new();
        for var in mono.vars() {
            if let Ty::Term(v) = var {
                let newvar = self.newvar();
                quantifiers.push(newvar);
                sub.entry(*v).or_insert(newvar);
            }
        }
        mono.apply_backwards(&sub);
        Polytype { quantifiers, mono }
    }

    fn var(&mut self, x: String) -> Monotype {
        let poly = self.gamma.get_mut(&x).expect("var initialized").clone();
        self.inst(poly)
    }

    fn app(&mut self, e1: Expr, e2: Expr) -> Monotype {
        let mut t1 = self.expr(e1);
        let mut t2in = self.expr(e2);
        let mut t2out = Monotype::Var(Ty::Var(self.newvar()));
        if let Monotype::Func(t1in, t1out) = &mut t1 {
            t1in.unify(&mut t2in);
            t1out.unify(&mut t2out);
        } else {
            unreachable!()
        }
        t2out
    }

    fn abs(&mut self, x: String, e: Expr) -> Monotype {
        let input = Monotype::Var(Ty::Var(self.newvar()));
        self.gamma.insert(x, Polytype::mono(input.clone()));
        let e_ty = self.expr(e);
        Monotype::Func(Box::new(input), Box::new(e_ty))
    }

    fn r#let(&mut self, x: String, e1: Expr, e2: Expr) -> Monotype {
        let t1 = self.expr(e1);
        let gen = self.generalize(&t1);
        self.gamma.insert(x, gen);
        self.expr(e2)
    }
}
