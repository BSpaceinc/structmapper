use syn::visit_mut::{self, VisitMut};
use syn::{parse_quote, Expr};
use proc_macro2::TokenStream;
use syn::parse::{ParseBuffer, Parse};

struct ReplacePlaceholder(Expr);

#[derive(Debug)]
enum Token<'a> {
  Lit(&'a str),
  Placeholder(Placeholder)
}

#[derive(Debug)]
struct Placeholder(Expr);

impl Parse for Placeholder {
  fn parse(input: &ParseBuffer) -> syn::Result<Self> {
    let content;
    let _brace = syn::braced!(content in input);
    let expr: Expr = content.parse()?;
    Ok(Self(expr))
  }
}

#[derive(Debug, Clone)]
enum LexerError {
  UnclosedBracket,
  InvalidExpression,
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
        (self.src, "")
      } else {
        self.src.split_at(end + 1)
      };
      self.src = rest;
      let parsed: Placeholder = syn::parse_str(inner).map_err(|_| LexerError::InvalidExpression)?;
      Ok(parsed)
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
  let tokens: Vec<_> = Lexer::new("{a.b.c.d}").collect::<Result<Vec<_>, _>>().unwrap();
  panic!("{:?}", tokens);
}