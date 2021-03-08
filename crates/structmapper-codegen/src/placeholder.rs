use syn::visit_mut::{self, VisitMut};
use syn::{parse_quote, Expr};
use proc_macro2::TokenStream;

struct ReplacePlaceholder(Expr);

enum Token<'a> {
  Lit(&'a str),
  Placeholder(Placeholder<'a>)
}

struct Placeholder<'a>(&'a Expr);

#[derive(Clone)]
enum LexerError {
  UnclosedBracket,
  InvalidMember,
}

struct Lexer<'a> {
  src: &'a str,
  err: Option<LexerError>,
}

impl<'a> Lexer<'a> {
  fn new(src: &'a str) -> Self {
    Self {
      src,
      err: None
    }
  }

  fn parse_placeholder(&mut self) -> Result<Placeholder, LexerError> {
    if let Some(end) = self.src.find('}') {
      let (inner, rest) = if end == self.src.len() - 1 {
        (&self.src, "")
      } else {
        self.src.split_at(end + 1)
      };
      let tokens: TokenStream = match syn::parse_str(inner) {

      };
      self.src = rest;

    } else {
      return Err(LexerError::UnclosedBracket)
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Result<Token<'a>, LexerError>;

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(ref err) = self.err {
      return Some(Err(err.clone()))
    }

    if self.src.is_empty() {
      return None
    }

    if let Some(stripped) = self.src.strip_prefix(|c: char| c.is_whitespace()) {
      self.src = stripped;
    }

    if self.src.starts_with('{') {
      return Some(self.parse_placeholder()
        .map(Token::Placeholder)
        .map_err(|err| {
          self.err = err.clone().into();
          err
        })
      )
    }

    if let Some(pos) = self.src.find('{') {
      let (lit, rest) = self.src.split_at(pos);
      self.src = rest;
      return Some(Ok(Token::Lit(lit)))
    }

    if !self.src.is_empty() {
      let rest = self.src;
      self.src = "";
      Some(Ok(Token::Lit(rest)))
    } else {
      None
    }
  }
}

#[test]
fn test_self() {
  use syn::Expr;

  let expr: Expr = syn::parse_str("{0}").unwrap();
  panic!("{:?}", expr);
}