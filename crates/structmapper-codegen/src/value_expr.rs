use syn::visit_mut::{self, VisitMut};
use syn::{parse_quote, Expr, Lit, ExprLit};
use proc_macro2::TokenStream;
use syn::parse::{ParseBuffer, Parse};
use quote::ToTokens;
use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum Error {
  #[error("Unclosed bracket")]
  UnclosedBracket,
  #[error("Invalid expression")]
  InvalidExpression,
}

pub struct ValueExpr<'a> {
  tokens: Vec<Token<'a>>,
}

impl<'a> ValueExpr<'a> {
  pub fn from_lit(src: &'a str) -> Result<ValueExpr<'a>, Error> {
    Ok(Self {
      tokens: Lexer::new(src).collect::<Result<Vec<_>, _>>()?,
    })
  }

  pub fn into_tokens(self, default_base_tokens: &TokenStream) -> Result<TokenStream, Error> {
    let transformed = self.tokens.into_iter()
      .map(|token| match token {
        Token::Lit(v) => Cow::from(v),
        Token::Placeholder(v) => {
          AddBase {
            default_base_tokens
          }.transform(v.0).into_token_stream().to_string().into()
        }
      })
      .collect::<Vec<_>>()
      .join(" ");
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

struct AddBase<'a> {
  default_base_tokens: &'a TokenStream,
}

impl<'a> AddBase<'a> {
  fn transform(mut self, mut expr: Expr) -> Expr {
    if let Expr::Lit(ref lit) = expr {
      return self.transform_lit(lit)
    }
    self.visit_expr_mut(&mut expr);
    expr
  }

  fn transform_lit(&self, lit: &ExprLit) -> Expr {
    match lit.lit {
      // tuple access: {0}
      Lit::Int(ref lit_int) => {
        if lit_int.suffix().is_empty() {
          let self_tokens = self.default_base_tokens;
          return syn::parse_quote! {
            #self_tokens . #lit_int
          }
        }
      },
      _ => {}
    }
    panic!("Invalid tuple index: {}", lit.into_token_stream())
  }
}

impl<'a> VisitMut for AddBase<'a> {
  fn visit_expr_mut(&mut self, node: &mut Expr) {
    match node {
      // {a.b.c} => {self.a.b.c}
      // {0.a} => {self.0.a}
      Expr::Field(expr) => {
        if let Expr::Lit(ref lit) = *expr.base {
          let base_expr = self.transform_lit(lit);
          expr.base = Box::new(base_expr);
          return
        }

        visit_mut::visit_expr_mut(self, &mut expr.base);
      }
      // {a} => {self.a}
      Expr::Path(expr) if expr.path.segments.len() == 1 => {
        let self_expr = self.default_base_tokens;
        let path = &expr.path;
        let replaced: Expr = parse_quote! {
          #self_expr . #path
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
fn test_ident() {
  let src = "{a}";
  let self_tokens = quote::quote!(self);
  let expected: TokenStream = syn::parse_quote! {
    self.a
  };

  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_tokens).unwrap();
  assert_eq!(tokens.to_string(), expected.to_string());
}


#[test]
fn test_tuple_index() {
  let src = "{0}";
  let self_tokens = quote::quote!(self);
  let expected: TokenStream = syn::parse_quote! {
    self.0
  };

  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_tokens).unwrap();
  assert_eq!(tokens.to_string(), expected.to_string());
}

#[test]
fn test_tuple_index_field() {
  let src = "{0.a}";
  let self_tokens = quote::quote!(self);
  let expected: TokenStream = syn::parse_quote! {
    self.0.a
  };

  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_tokens).unwrap();
  assert_eq!(tokens.to_string(), expected.to_string());
}

#[test]
fn test_field_deep() {
  let src = "{a}.b.c.d";
  let self_tokens = quote::quote!(self);
  let expected: TokenStream = syn::parse_quote! {
    self.a.b.c.d
  };

  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_tokens).unwrap();
  assert_eq!(tokens.to_string(), expected.to_string());
}

#[test]
fn test_binary() {
  let src = "{1} + {a}";
  let self_tokens = quote::quote!(self);
  let expected: TokenStream = syn::parse_quote! {
    self.1 + self.a
  };

  let expr = ValueExpr::from_lit(src).unwrap();
  let tokens = expr.into_tokens(&self_tokens).unwrap();
  assert_eq!(tokens.to_string(), expected.to_string());
}