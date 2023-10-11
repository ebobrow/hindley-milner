use anyhow::{anyhow, bail};

use crate::scanner::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// x
    Var(String),
    /// e1 e2
    App { e1: Box<Expr>, e2: Box<Expr> },
    /// \x.e
    Abs { x: Box<Expr>, e: Box<Expr> },
    /// let x = e1 in e2
    Let {
        x: Box<Expr>,
        e1: Box<Expr>,
        e2: Box<Expr>,
    },
}

pub struct Parser {
    stream: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn parse(stream: Vec<Token>) -> anyhow::Result<Expr> {
        let mut parser = Self { stream, pos: 0 };
        parser.expr()
    }

    fn expr(&mut self) -> anyhow::Result<Expr> {
        self.do_expr(true)
    }

    fn do_expr(&mut self, recurse_app: bool) -> anyhow::Result<Expr> {
        match self.peek().ok_or_else(|| anyhow!("empty stream"))? {
            Token::Identifier(_) => {
                let e1 = self.var()?;
                if recurse_app {
                    self.maybe_app(e1)
                } else {
                    Ok(e1)
                }
            }
            Token::Lambda => self.abstraction(),
            Token::Let => self.let_in(),
            Token::LeftParen => {
                self.consume(&Token::LeftParen)?;
                let e = self.expr()?;
                self.consume(&Token::RightParen)?;
                if recurse_app {
                    self.maybe_app(e)
                } else {
                    Ok(e)
                }
            }

            t @ (Token::In | Token::Equal | Token::Dot | Token::RightParen) => {
                bail!("unexpected token {:?}", t)
            }
        }
    }

    fn let_in(&mut self) -> anyhow::Result<Expr> {
        self.consume(&Token::Let)?;
        let x = self.var()?;
        self.consume(&Token::Equal)?;
        let e1 = self.expr()?;
        self.consume(&Token::In)?;
        let e2 = self.expr()?;
        Ok(Expr::Let {
            x: Box::new(x),
            e1: Box::new(e1),
            e2: Box::new(e2),
        })
    }

    fn var(&mut self) -> anyhow::Result<Expr> {
        let ident = self.consume_ident()?;
        Ok(Expr::Var(ident))
    }

    fn maybe_app(&mut self, e1: Expr) -> anyhow::Result<Expr> {
        let mut ee = Vec::new();
        while let Ok(e) = self.do_expr(false) {
            ee.push(e);
        }
        Ok(ee.iter().fold(e1, |a, b| Expr::App {
            e1: Box::new(a),
            e2: Box::new(b.clone()),
        }))
    }

    fn abstraction(&mut self) -> anyhow::Result<Expr> {
        self.consume(&Token::Lambda)?;
        let x = self.var()?;
        self.consume(&Token::Dot)?;
        let e = self.expr()?;
        Ok(Expr::Abs {
            x: Box::new(x),
            e: Box::new(e),
        })
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.pos < self.stream.len() {
            Some(&self.stream[self.pos])
        } else {
            None
        }
    }

    fn consume(&mut self, tok: &Token) -> anyhow::Result<&Token> {
        let peek = self.peek();
        if peek == Some(tok) {
            self.pos += 1;
            Ok(self.peek().expect("is some"))
        } else {
            bail!("expected {:?}, found {:?}", tok, peek)
        }
    }

    fn consume_ident(&mut self) -> anyhow::Result<String> {
        let peek = self.peek();
        if let Some(Token::Identifier(ident)) = peek {
            let ident = ident.clone();
            self.pos += 1;
            Ok(ident)
        } else {
            bail!("exptected identifier, found {:?}", peek)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works() {
        let stream = vec![
            Token::Let,
            Token::Identifier("id".into()),
            Token::Equal,
            Token::Lambda,
            Token::Identifier("x".into()),
            Token::Dot,
            Token::Identifier("x".into()),
            Token::In,
            Token::Identifier("id".into()),
            Token::Identifier("id".into()),
        ];
        let e = Parser::parse(stream).unwrap();
        assert_eq!(
            e,
            Expr::Let {
                x: Box::new(Expr::Var("id".into())),
                e1: Box::new(Expr::Abs {
                    x: Box::new(Expr::Var("x".into())),
                    e: Box::new(Expr::Var("x".into()))
                }),
                e2: Box::new(Expr::App {
                    e1: Box::new(Expr::Var("id".into())),
                    e2: Box::new(Expr::Var("id".into()))
                })
            }
        );
    }
}
