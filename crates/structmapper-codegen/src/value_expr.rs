use syn::visit_mut::{self, VisitMut};
use syn::{parse_quote, Expr};
use proc_macro2::TokenStream;
use syn::parse::{ParseBuffer, Parse};
use quote::ToTokens;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Error {
  UnclosedBracket,
  InvalidExpression,
}

struct ValueExpr<'a> {
  tokens: Vec<Token<'a>>,
}

impl<'a> ValueExpr<'a> {
  pub fn from_lit(src: &'a str) -> Result<ValueExpr<'a>, Error> {
    Ok(Self {
      tokens: Lexer::new(src).collect::<Result<Vec<_>, _>>()?,
    })
  }

  pub fn into_tokens(self, self_expr: &Expr) -> Result<TokenStream, Error> {
    let transformed = self.tokens.into_iter()
      .map(|token| match token {
        Token::Lit(v) => Cow::from(v),
        Token::Placeholder(v) => {
          dbg!(&v.0);
          AddSelf {
            self_expr
          }.transform(v.0).into_token_stream().to_string().into()
        }
      })
      .collect::<Vec<_>>()
      .join("");
    transformed.parse().map_err(|err| {
      panic!("generated value expr is invalid: {}: {}", err, transformed)
    })
  }
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
  Lit(&'a str),
  Placeholder(Placeholder)
}

#[derive(Debug, PartialEq)]
struct Placeholder(Expr);

impl Parse for Placeholder {
  fn parse(input: &ParseBuffer) -> syn::Result<Self> {
    let content;
    let _brace = syn::braced!(content in input);
    let expr: Expr = content.parse()?;
    Ok(Self(expr))
  }
}

struct Lexer<'a> {
  src: &'a str,
  err: Option<Error>,
}

impl<'a> Lexer<'a> {
  fn new(src: &'a str) -> Self {
    Self {
      src,
      err: None
    }
  }

  fn parse_placeholder(&mut self) -> Result<Placeholder, Error> {
    if let Some(end) = self.src.find('}') {
      let (inner, rest) = if end == self.src.len() - 1 {
        (self.src, "")
      } else {
        self.src.split_at(end + 1)
      };
      self.src = rest;
      let parsed: Placeholder = syn::parse_str(inner).map_err(|_| Error::InvalidExpression)?;
      Ok(parsed)
    } else {
      return Err(Error::UnclosedBracket)
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Result<Token<'a>, Error>;

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

struct AddSelf<'a> {
  self_expr: &'a Expr,
}

impl<'a> AddSelf<'a> {
  fn transform(mut self, mut expr: Expr) -> Expr {
    visit_mut::visit_expr_mut(&mut self, &mut expr);
    expr
  }
}

impl<'a> VisitMut for AddSelf<'a> {
  fn visit_expr_mut(&mut self, node: &mut Expr) {
    use syn::{ExprField, Member, parse_quote};

    match node {
      // {a.b.c} => {self.a.b.c}
      Expr::Field(expr) => {
        visit_mut::visit_expr_mut(self, &mut expr.base);
      }
      // {a} => {self.a}
      Expr::Path(expr) if expr.path.segments.len() == 1 => {
        let self_expr = self.self_expr;
        let path = &expr.path;
        let mut replaced: Expr = parse_quote! {
          #self_expr.#path
        };
        *node = replaced;
      }
      _ => {
        visit_mut::visit_expr_mut(self, node);
      }
    }
  }
}

#[test]
fn test_binary() {
  let src = "1 + {a}";
  let self_expr: Expr = syn::parse_quote!(self);
  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_expr).unwrap();
  let expected: TokenStream = syn::parse_quote! {
    1 + self.a
  };
  assert_eq!(tokens.to_string(), expected.to_string());
}