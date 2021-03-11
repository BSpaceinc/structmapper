use quote::{ToTokens, quote};
use proc_macro2::{TokenStream, Span};
use syn::{DeriveInput, Ident, Data, Meta, MetaList, Fields, NestedMeta, Path, Lit, Type};
use proc_macro_error::{abort, abort_call_site, diagnostic, Level, ResultExt};
use crate::value_expr::ValueExpr;
use syn::spanned::Spanned;

#[derive(Debug)]
pub struct Derive {
  ident: syn::Ident,
  generics: syn::Generics,
  fields: Vec<StructField>,
  from_targets: Vec<FromStruct>,
}

impl Derive {
  pub fn from_derive_input(input: &DeriveInput) -> Self {
    let fields: Vec<_> = match input.data {
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
        .map(|v| {
          v.validate_override_fields(&fields);
          v
        })
    }).collect();

    Self {
      ident: input.ident.clone(),
      generics: input.generics.clone(),
      fields,
      from_targets,
    }
  }
}

impl ToTokens for Derive {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let items: Vec<_> = self.from_targets.iter()
      .map(|v| {
        v.get_impl_from_tokens(self)
      })
      .collect();

    tokens.extend(quote! {
      #(#items)*
    });
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
  default_base_tokens: TokenStream,
  override_fields: Option<FromStructFields>,
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

    let mut from_type = None;
    let mut default_base = None;
    let mut override_fields = None;

    for opt in opts {
      match opt {
        FromStructOpts::FromType(v) => {
          from_type = Some(v);
        }
        FromStructOpts::DefaultBase(v) => {
          default_base = Some(v)
        }
        FromStructOpts::Fields(v) => {
          override_fields = Some(v);
        }
      }
    }

    Self {
      from_type: from_type.unwrap_or_else(|| {
        abort!(list, "`from_type` option is missing")
      }),
      default_base_tokens: default_base.unwrap_or_else(|| quote!(__from)),
      override_fields,
    }
  }

  fn validate_override_fields(&self, self_fields: &[StructField]) {
    if let Some(ref override_fields) = self.override_fields.as_ref() {
      let self_field_idents: Vec<_> = self_fields.iter()
        .map(|i| &i.ident)
        .collect();
      for field in &override_fields.fields {
        if !self_field_idents.contains(&&field.ident) {
          abort!(field.ident, format!("Unknown field `{}`", field.ident));
        }
      }
    }
  }

  fn get_impl_from_tokens(&self, input: &Derive) -> TokenStream {
    let self_ident = &input.ident;
    let from_type = &self.from_type;
    let default_base_tokens = &self.default_base_tokens;
    let base_tokens = quote! {__from};
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let assign_items: Vec<_> = input.fields.iter()
      .map(|field| {
        if let Some(ref fields) = self.override_fields {
          if let Some(f) = fields.fields.iter().find(|i| i.ident == field.ident) {
            return f.get_field_assign_tokens(&base_tokens)
          }
        }
        let field = &field.ident;
        quote! {
          #field : #default_base_tokens . #field
        }
      })
      .collect();
    quote! {
      impl #impl_generics std::convert::From<#from_type> for #self_ident #ty_generics #where_clause {
        fn from(__from: #from_type) -> Self {
          Self {
            #(#assign_items),*
          }
        }
      }
    }
  }
}

#[derive(Debug)]
enum FromStructOpts {
  FromType(FromType),
  DefaultBase(TokenStream),
  Fields(FromStructFields)
}

#[derive(Debug, Clone)]
enum FromType {
  Path(Path),
  Tuple(Type),
}

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

impl ToTokens for FromType {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match *self {
      FromType::Path(ref v) => v.to_tokens(tokens),
      FromType::Tuple(ref v) => v.to_tokens(tokens),
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
                  "default_base" => Self::parse_default_base(&v.lit),
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

  fn parse_default_base(v: &Lit) -> Self {
    let default_self = quote!(__from);
    if let Lit::Str(ref lit) = v {
      let tokens = ValueExpr::from_lit(&lit.value())
        .map_err(|err| diagnostic!(v, Level::Error, err))
        .expect_or_abort("`self_value` expression")
        .into_tokens(&default_self)
        .map_err(|err| diagnostic!(v, Level::Error, err))
        .expect_or_abort("`self_value` expression parse");
      Self::DefaultBase(tokens)
    } else {
      abort!(v, "Invalid `self_value` expression.")
    }
  }
}

#[derive(Debug)]
struct FromStructFields {
  fields: Vec<FromStructField>,
  span: Span,
}


impl FromStructFields {
  fn from_meta_list(v: &MetaList) -> Self {
    Self {
      fields: v.nested.iter()
        .map(FromStructField::from_nested_meta)
        .collect(),
      span: v.span(),
    }
  }
}

#[derive(Debug)]
struct FromStructField {
  ident: Ident,
  lit: Lit,
}

impl FromStructField {
  fn from_nested_meta(meta: &NestedMeta) -> Self {
    const ABORT_MESSAGE: &str = r#"Expected: key = "<expr>""#;

    match meta {
      NestedMeta::Meta(ref meta) => {
        match meta {
          Meta::Path(_) => {
            abort!(meta, ABORT_MESSAGE)
          }
          Meta::List(_) => {
            abort!(meta, ABORT_MESSAGE)
          }
          // key = ".."
          Meta::NameValue(ref v) => {
            match v.path.get_ident() {
              Some(ident) => {
                Self {
                  ident: ident.clone(),
                  lit: v.lit.clone(),
                }
              },
              None => {
                abort!(v, ABORT_MESSAGE)
              }
            }
          }
        }
      }
      NestedMeta::Lit(_) => {
        abort!(meta, ABORT_MESSAGE)
      }
    }
  }
}

impl FromStructField {
  // `field : value`
  fn get_field_assign_tokens(&self, base_tokens: &TokenStream) -> TokenStream {
    let src = if let Lit::Str(ref lit) = self.lit {
      lit.value()
    } else {
      abort!(self.lit, "Invalid value expression.")
    };
    let expr = ValueExpr::from_lit(&src)
      .map_err(|err| diagnostic!(self.lit, Level::Error, err))
      .expect_or_abort("Value expression parse");
    let value_tokens = expr.into_tokens(base_tokens)
      .map_err(|err| diagnostic!(self.lit, Level::Error, err))
      .expect_or_abort("Value expression transform");

    let ident = &self.ident;
    quote! {
      #ident : #value_tokens
    }
  }
}
