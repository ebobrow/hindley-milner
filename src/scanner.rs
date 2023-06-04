use anyhow::anyhow;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Lambda,
    Let,
    In,
    Equal,
    Dot,
    LeftParen,
    RightParen,
}

pub struct Scanner {
    stream: String,
    pos: usize,
}

impl Scanner {
    pub fn scan(stream: String) -> anyhow::Result<Vec<Token>> {
        let mut scanner = Self { stream, pos: 0 };
        let mut tokens = Vec::new();
        while let Some(token) = scanner.scan_token() {
            tokens.push(token?);
        }
        Ok(tokens)
    }

    fn scan_token(&mut self) -> Option<anyhow::Result<Token>> {
        match self.advance()? {
            '\\' => Some(Ok(Token::Lambda)),
            '.' => Some(Ok(Token::Dot)),
            '=' => Some(Ok(Token::Equal)),
            '(' => Some(Ok(Token::LeftParen)),
            ')' => Some(Ok(Token::RightParen)),
            c => {
                if c.is_alphabetic() {
                    let start = self.pos - 1;
                    while matches!(self.advance(), Some(c) if !" .\\=()".contains(c)) {}
                    self.pos -= 1;
                    let ident = self.stream.get(start..self.pos)?;
                    let tok = match ident {
                        "let" => Token::Let,
                        "in" => Token::In,
                        _ => Token::Identifier(ident.to_string()),
                    };
                    Some(Ok(tok))
                } else if c.is_whitespace() {
                    while matches!(self.advance(), Some(c) if c.is_whitespace()) {}
                    self.pos -= 1;
                    self.scan_token()
                } else {
                    Some(Err(anyhow!("invalid identifier")))
                }
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.stream.chars().nth(self.pos - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works() {
        let stream = String::from("let id = \\x.x in id id");
        let tokens = Scanner::scan(stream).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier("id".into()),
                Token::Equal,
                Token::Lambda,
                Token::Identifier("x".into()),
                Token::Dot,
                Token::Identifier("x".into()),
                Token::In,
                Token::Identifier("id".into()),
                Token::Identifier("id".into())
            ]
        );
    }
}
