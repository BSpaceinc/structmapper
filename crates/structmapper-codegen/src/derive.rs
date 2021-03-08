use quote::ToTokens;
use proc_macro2::TokenStream;
use syn::{DeriveInput, Ident, Data, Meta, MetaList, Fields, Expr, NestedMeta, Path, Lit, Type};
use proc_macro_error::{abort, abort_call_site, emit_error, diagnostic, Level, ResultExt, OptionExt};
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
pub struct DeriveOpts {
  ident: syn::Ident,
  fields: Vec<StructField>,
  from_targets: Vec<FromStruct>,
}

impl DeriveOpts {
  pub fn from_derive_input(input: &DeriveInput) -> Self {
    let ident = input.ident.clone();

    let fields = match input.data {
      Data::Struct(ref data) => {
        match data.fields {
          Fields::Named(ref fields) => {
            fields.named.iter().map(|field| {
              StructField {
                ident: field.ident.clone().unwrap(),
                ty: field.ty.clone(),
              }
            }).collect()
          }
          _ => abort!(data.fields, "Only support named fields.")
        }
      }
      _ => {
        abort_call_site!("Only support structs.");
      }
    };

    let from_targets = input.attrs.iter().filter_map(|attr| {
      let meta = attr.parse_meta().unwrap_or_abort();
      FromStruct::from_meta(&meta)
    }).collect();

    Self {
      ident,
      fields,
      from_targets,
    }
  }
}

impl ToTokens for DeriveOpts {
  fn to_tokens(&self, _tokens: &mut TokenStream) {
    panic!("{:#?}", self)
  }
}

#[derive(Debug)]
struct StructField {
  ident: syn::Ident,
  ty: syn::Type,
}

#[derive(Debug)]
struct FromStruct {
  from_type: FromType,
  self_value: Option<Expr>,
  mappings: Vec<FromStructField>,
}

impl FromStruct {
  fn from_meta(meta: &Meta) -> Option<Self> {
    match meta {
      // Empty attribute:
      // #[struct_mapper]
      Meta::Path(_) => {
        None
      }
      // #[struct_mapper(...)]
      Meta::List(ref list) => {
        Self::from_meta_list(list).into()
      }
      // NameValue:
      // #[struct_mapper = "foo"]
      Meta::NameValue(_) => {
        abort!(meta, "Invalid syntax.")
      }
    }
  }

  fn from_meta_list(list: &MetaList) -> Self {
    let opts: Vec<_> = list.nested.iter().map(FromStructOpts::from_nested_meta).collect();
    let from_type = opts.iter()
      .filter_map(|opt| {
        if let FromStructOpts::FromType(ref from_type) = opt {
          Some(from_type.clone())
        } else {
          None
        }
      })
      .last()
      .expect_or_abort("from_type option");

    Self {
      from_type,
      self_value: None,
      mappings: vec![]
    }
  }
}

#[derive(Debug)]
enum FromStructOpts {
  FromType(FromType),
  SelfValue(Expr),
  Fields(FromStructFields)
}

#[derive(Debug, Clone)]
enum FromType {
  Path(Path),
  Tuple(Type),
}


// impl Parse for FromType {
//   fn parse(input: ParseStream) -> syn::Result<Self> {
//     if input.peek(syn::Token![(]) {
//       Ok(Self::Tuple(input.parse()?))
//     } else {
//
//     }
//   }
// }

impl FromType {
  fn from_lit(v: &Lit) -> Self {
    match v {
      Lit::Str(lit) => {
        let src = lit.value();
        if src.trim_start().starts_with("(") {
          FromType::Tuple(syn::parse_str(&src)
            .map_err(|err| diagnostic!(lit, Level::Error, err))
            .expect_or_abort("Not a tuple type"))
        } else {
          FromType::Path(syn::parse_str(&src)
            .map_err(|err| diagnostic!(lit, Level::Error, err))
            .expect_or_abort("Not a type"))
        }
      }
      _ => abort!(v, "Invalid syntax.")
    }
  }
}

impl FromStructOpts {
  fn from_nested_meta(meta: &NestedMeta) -> Self {
    match meta {
      NestedMeta::Meta(ref meta) => {
        match meta {
          Meta::Path(_) => {
            abort!(meta, "Invalid syntax.")
          }
          // fields(..)
          Meta::List(ref v) => {
            if let Some(true) = v.path.get_ident().map(|ident| ident == "fields") {
              Self::Fields(FromStructFields::from_meta_list(v))
            } else {
              abort!(v, "Unknown option.")
            }
          }
          // from_type = ".."
          Meta::NameValue(ref v) => {
            match v.path.get_ident() {
              Some(ident) => {
                let ident = ident.to_string();
                match ident.as_str() {
                  "from_type" => Self::FromType(FromType::from_lit(&v.lit)),
                  "self_value" => Self::parse_self_value(&v.lit),
                  _ => {
                    abort!(v, "Unknown option.")
                  }
                }
              },
              None => {
                abort!(v, "Unknown option.")
              }
            }
          }
        }
      }
      NestedMeta::Lit(_) => {
        abort!(meta, "Invalid syntax.")
      }
    }
  }

  fn parse_self_value(v: &Lit) -> Self {
    Self::SelfValue(Self::parse_expr(v))
  }

  fn parse_expr(v: &Lit) -> Expr {
    let expr: Expr = syn::parse2(v.into_token_stream())
      .map_err(|err| diagnostic!(v, Level::Error, err))
      .expect_or_abort("Expression");
    expr
  }
}

#[derive(Debug)]
struct FromStructFields {
  fields: Vec<FromStructField>,
}


impl FromStructFields {
  fn from_meta_list(v: &MetaList) -> Self {
    Self {
      fields: vec![]
    }
  }
}

#[derive(Debug)]
struct FromStructField {
  ident: Ident,
  value: Expr,
}
