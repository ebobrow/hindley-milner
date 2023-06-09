use std::collections::HashMap;

use anyhow::{anyhow, bail};

use crate::parser::Expr;

type Substitution = HashMap<usize, usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    Bound(usize),
    Free(usize),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Monotype {
    Var(Ty),
    Func(Box<Monotype>, Box<Monotype>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Polytype {
    quantifiers: Vec<usize>,
    mono: Monotype,
}

impl ToString for Monotype {
    fn to_string(&self) -> String {
        match self {
            Monotype::Var(Ty::Bound(v)) | Monotype::Var(Ty::Free(v)) => v.to_string(),
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
                Ty::Bound(v) => {
                    *ty = Ty::Free(*substitution.get(v).unwrap_or(v));
                }
                Ty::Free(_) => {}
            },
            Monotype::Func(t1, t2) => {
                t1.apply(substitution);
                t2.apply(substitution);
            }
        }
    }

    fn vars(&mut self) -> Vec<&mut Ty> {
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
    pub fn type_expr(expr: Expr) -> anyhow::Result<Monotype> {
        let mut typer = Self::default();
        typer.expr(expr)
    }

    fn expr(&mut self, expr: Expr) -> anyhow::Result<Monotype> {
        match expr {
            Expr::Var(x) => self.var(x),
            Expr::App { e1, e2 } => self.app(*e1, *e2),
            Expr::Abs { x, e } => {
                if let Expr::Var(x) = *x {
                    self.abs(x, *e)
                } else {
                    bail!("type error");
                }
            }
            Expr::Let { x, e1, e2 } => {
                if let Expr::Var(x) = *x {
                    self.r#let(x, *e1, *e2)
                } else {
                    bail!("type error");
                }
            }
        }
    }

    fn newvar(&mut self) -> usize {
        self.next_type += 1;
        self.next_type - 1
    }

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
        let mut quantifiers = Vec::new();
        for var in mono.vars() {
            if let Ty::Free(v) = var {
                if !quantifiers.contains(v) {
                    quantifiers.push(*v);
                }
                *var = Ty::Bound(*v);
            }
        }
        Polytype { quantifiers, mono }
    }

    fn unify(&mut self, a: &mut Monotype, b: &mut Monotype) {
        // TODO: if we have unify(a->a, b->b->c) and we replace the first a with b->b we need to
        // also replace the second a
        match (a.clone(), b.clone()) {
            (Monotype::Var(Ty::Free(_)), _) => {
                *a = b.clone();
            }
            (_, Monotype::Var(Ty::Free(_))) => {
                *b = a.clone();
            }
            (Monotype::Func(mut a1, mut a2), Monotype::Func(mut b1, mut b2)) => {
                self.unify(&mut a1, &mut b1);
                self.unify(&mut a2, &mut b2);
                *a = Monotype::Func(a1, a2);
                *b = Monotype::Func(b1, b2);
            }
            _ => panic!(":("),
        }
    }

    fn var(&mut self, x: String) -> anyhow::Result<Monotype> {
        let poly = self
            .gamma
            .get_mut(&x)
            .ok_or_else(|| anyhow!("var {x} not initialized"))?
            .clone();
        Ok(self.inst(poly))
    }

    fn app(&mut self, e1: Expr, e2: Expr) -> anyhow::Result<Monotype> {
        let mut t1 = self.expr(e1)?;
        let mut t2in = self.expr(e2)?;
        let mut t2out = Monotype::Var(Ty::Free(self.newvar()));
        if let Monotype::Func(t1in, t1out) = &mut t1 {
            self.unify(t1in, &mut t2in);
            self.unify(t1out, &mut t2out);
            // t1in.unify(&mut t2in);
            // t1out.unify(&mut t2out);
        } else {
            bail!("type error");
        }
        Ok(t2out)
    }

    fn abs(&mut self, x: String, e: Expr) -> anyhow::Result<Monotype> {
        let input = Monotype::Var(Ty::Free(self.newvar()));
        self.gamma.insert(x, Polytype::mono(input.clone()));
        let e_ty = self.expr(e)?;
        Ok(Monotype::Func(Box::new(input), Box::new(e_ty)))
    }

    fn r#let(&mut self, x: String, e1: Expr, e2: Expr) -> anyhow::Result<Monotype> {
        let t1 = self.expr(e1)?;
        let gen = self.generalize(&t1);
        self.gamma.insert(x, gen);
        self.expr(e2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inst() {
        // forall a b. a -> b -> a
        let ty = Polytype {
            quantifiers: vec![0, 1],
            mono: Monotype::Func(
                Box::new(Monotype::Var(Ty::Bound(0))),
                Box::new(Monotype::Func(
                    Box::new(Monotype::Var(Ty::Bound(1))),
                    Box::new(Monotype::Var(Ty::Bound(0))),
                )),
            ),
        };
        let mut typer = Typer::default();
        assert_eq!(
            typer.inst(ty),
            // a -> b -> a
            Monotype::Func(
                Box::new(Monotype::Var(Ty::Free(0))),
                Box::new(Monotype::Func(
                    Box::new(Monotype::Var(Ty::Free(1))),
                    Box::new(Monotype::Var(Ty::Free(0)))
                ))
            )
        );
    }

    #[test]
    fn generalize() {
        // a -> b -> a
        let ty = Monotype::Func(
            Box::new(Monotype::Var(Ty::Free(0))),
            Box::new(Monotype::Func(
                Box::new(Monotype::Var(Ty::Free(1))),
                Box::new(Monotype::Var(Ty::Free(0))),
            )),
        );
        let mut typer = Typer::default();
        assert_eq!(
            typer.generalize(&ty),
            // forall a b. a -> b -> a
            Polytype {
                quantifiers: vec![0, 1],
                mono: Monotype::Func(
                    Box::new(Monotype::Var(Ty::Bound(0))),
                    Box::new(Monotype::Func(
                        Box::new(Monotype::Var(Ty::Bound(1))),
                        Box::new(Monotype::Var(Ty::Bound(0))),
                    )),
                ),
            }
        );
    }

    #[test]
    fn unify() {
        let mut t1 = Monotype::Func(
            Box::new(Monotype::Var(Ty::Free(0))),
            Box::new(Monotype::Var(Ty::Free(0))),
        );
        let mut t2 = Monotype::Func(
            Box::new(Monotype::Var(Ty::Free(1))),
            Box::new(Monotype::Func(
                Box::new(Monotype::Var(Ty::Free(1))),
                Box::new(Monotype::Var(Ty::Free(2))),
            )),
        );
        let mut typer = Typer::default();
        typer.unify(&mut t1, &mut t2);
        panic!("{:#?}\n{:#?}", t1, t2);
    }
}
