use crate::value_expr::ValueExpr;
use proc_macro2::{Span, TokenStream};
use proc_macro_error::{abort, abort_call_site, diagnostic, Level, ResultExt};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{
  Data, DeriveInput, Fields, Ident, Lit, Meta, MetaList, NestedMeta, Path, Type, TypeReference,
  TypeTuple,
};

const ATTR_NAME: &str = "struct_mapper";

#[derive(Debug)]
pub struct Derive {
  ident: syn::Ident,
  generics: syn::Generics,
  data: TypeData,
  mappings: Vec<Mapping>,
}

impl Derive {
  pub fn from_derive_input(input: &DeriveInput) -> Self {
    let data = match input.data {
      Data::Struct(ref data) => TypeData::Struct(match data.fields {
        Fields::Named(ref fields) => fields
          .named
          .iter()
          .map(|field| StructField {
            ident: field.ident.clone().unwrap(),
            ty: field.ty.clone(),
          })
          .collect(),
        _ => abort!(data.fields, "Only support named fields."),
      }),
      Data::Enum(ref data) => {
        TypeData::Enum(data.variants.iter().map(|v| {
          if let syn::Fields::Unit = v.fields {
            v.ident.clone()
          } else {
            abort!(v, "Only support unit variant.")
          }
        }).collect())
      }
      _ => {
        abort_call_site!("Only support struct and enum.");
      }
    };

    let mappings = input
      .attrs
      .iter()
      .filter_map(|attr| {
        let meta = attr.parse_meta().unwrap_or_abort();
        Mapping::from_meta(&meta).map(|v| {
          if let ImplTrait::From(_) = v.impl_trait {
            if let TypeData::Struct(ref fields) = data {
              v.validate_override_fields(fields);
            }
          }
          v
        })
      })
      .collect();

    Self {
      ident: input.ident.clone(),
      generics: input.generics.clone(),
      data,
      mappings,
    }
  }
}

impl ToTokens for Derive {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let items: Vec<_> = self
      .mappings
      .iter()
      .map(|v| v.get_impl_tokens(self))
      .collect();

    tokens.extend(quote! {
      #(#items)*
    });
  }
}

#[derive(Debug)]
enum TypeData {
  Struct(Vec<StructField>),
  Enum(Vec<syn::Ident>),
}

#[derive(Debug)]
struct StructField {
  ident: syn::Ident,
  ty: syn::Type,
}

#[derive(Debug)]
struct Mapping {
  impl_trait: ImplTrait,
  default_base: Option<TokenStream>,
  override_fields: Option<MappingAssignFields>,
  ignore_fields: Option<MappingIgnoreFields>,
}

impl Mapping {
  fn from_meta(meta: &Meta) -> Option<Self> {
    match meta {
      // Empty attribute:
      // #[struct_mapper]
      Meta::Path(_) => None,
      // #[struct_mapper(...)]
      Meta::List(ref list) => {
        if !list.path.get_ident().map(|v| v == ATTR_NAME).unwrap_or_default() {
          return None
        }
        Self::from_meta_list(list).into()
      },
      // NameValue:
      // #[struct_mapper = "foo"]
      Meta::NameValue(_) => None,
    }
  }

  fn from_meta_list(list: &MetaList) -> Self {
    let opts: Vec<_> = list
      .nested
      .iter()
      .map(MappingOpts::from_nested_meta)
      .collect();

    let mut from_type = None;
    let mut into_type = None;
    let mut default_base = None;
    let mut override_fields = None;
    let mut ignore_fields = None;

    for opt in opts {
      match opt {
        MappingOpts::FromType(v) => {
          from_type = Some(v);
        }
        MappingOpts::IntoType(v) => {
          into_type = Some(v)
        }
        MappingOpts::DefaultBase(v) => default_base = Some(v),
        MappingOpts::AssignFields(v) => {
          override_fields = Some(v);
        }
        MappingOpts::IgnoreFields(v) => {
          ignore_fields = Some(v)
        }
      }
    }

    if (from_type.is_none() && into_type.is_none()) || (from_type.is_some() && into_type.is_some()) {
      abort!(list, format!("Either `from_type` or `into_type` needs to be specified"));
    }

    let impl_trait = match (from_type, into_type) {
      (Some(_), Some(_)) | (None, None) => {
        abort!(list, format!("Either `from_type` or `into_type` needs to be specified"));
      }
      (Some(from_type), None) => {
        ImplTrait::From(from_type)
      }
      (None, Some(into_type)) => {
        ImplTrait::Into(into_type)
      }
    };

    Self {
      impl_trait,
      default_base,
      override_fields,
      ignore_fields,
    }
  }

  fn validate_override_fields(&self, self_fields: &[StructField]) {
    if let Some(ref override_fields) = self.override_fields.as_ref() {
      let self_field_idents: Vec<_> = self_fields.iter().map(|i| &i.ident).collect();
      for field in &override_fields.fields {
        if !self_field_idents.contains(&&field.ident) {
          abort!(field.ident, format!("Unknown field `{}`", field.ident));
        }
      }
    }
  }

  fn get_impl_tokens(&self, input: &Derive) -> TokenStream {
    match input.data {
      TypeData::Struct(ref fields) => {
        self.get_struct_tokens(input, fields)
      },
      TypeData::Enum(ref variants) => {
        self.get_enum_tokens(input, variants)
      },
    }
  }

  fn get_struct_tokens(&self, input: &Derive, fields: &[StructField]) -> TokenStream {
    let self_ident = &input.ident;
    let default_base = self.default_base.as_ref();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    match self.impl_trait {
      ImplTrait::From(ref from_type) => {
        let base_tokens = quote! {__from};
        let default_base = default_base.unwrap_or_else(|| &base_tokens);
        let assign_items: Vec<_> = fields
          .iter()
          .filter_map(|field| {
            if self.ignore_fields.as_ref().map(|f| f.idents.contains(&field.ident)).unwrap_or_default() {
              return None
            }

            if let Some(ref fields) = self.override_fields {
              if let Some(f) = fields.fields.iter().find(|i| i.ident == field.ident) {
                return Some(f.get_field_assign_tokens(&base_tokens));
              }
            }

            let ty = &field.ty;
            let field = &field.ident;
            let value = expand_value(ty, quote! {
              #default_base . #field
            });
            Some(quote! {
              #field : #value
            })
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
      },
      ImplTrait::Into(ref into_type) => {
        use std::collections::BTreeMap;
        let base_tokens = quote! {self};
        let default_base = default_base.unwrap_or_else(|| &base_tokens);
        let mut override_map: BTreeMap<_, _> = if let Some(ref value) = self.override_fields {
          value.fields
            .iter()
            .map(|f| {
              (f.ident.clone(), f)
            })
            .collect()
        } else {
          Default::default()
        };
        let assign_items: Vec<_> = fields
          .iter()
          .filter_map(|field| {
            if self.ignore_fields.as_ref().map(|f| f.idents.contains(&field.ident)).unwrap_or_default() {
              return None
            }

            if let Some(f) = override_map.remove(&&field.ident) {
              return Some(f.get_field_assign_tokens(&base_tokens));
            }
            let ty = &field.ty;
            let field = &field.ident;
            let value = expand_value(ty, quote! {
              #default_base . #field
            });
            Some(quote! {
              #field : #value
            })
          })
          .collect();
        let assign_items: Vec<_> = override_map.values().map(|f| {
          f.get_field_assign_tokens(&base_tokens)
        }).chain(assign_items.into_iter()).collect();
        quote! {
          impl #impl_generics std::convert::Into<#into_type> for #self_ident #ty_generics #where_clause {
            fn into(self) -> #into_type {
              #into_type {
                #(#assign_items),*
              }
            }
          }
        }
      },
    }    
  }

  fn get_enum_tokens(&self, input: &Derive, variants: &[Ident]) -> TokenStream {
    let self_ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    match self.impl_trait {
      ImplTrait::From(ref from_type) => {
        let variant_arms: Vec<_> = variants
          .iter()
          .filter_map(|ident| {
            if self.ignore_fields.as_ref().map(|f| f.idents.contains(ident)).unwrap_or_default() {
              return None
            }

            return Some(quote! {
              #from_type :: #ident => Self :: #ident
            });
          })
          .collect();

        quote! {
          impl #impl_generics std::convert::From<#from_type> for #self_ident #ty_generics #where_clause {
            fn from(__from: #from_type) -> Self {
              match __from {
                #(#variant_arms),*
              }
            }
          }
        }
      },
      ImplTrait::Into(ref into_type) => {
        let variant_arms: Vec<_> = variants
          .iter()
          .filter_map(|ident| {
            if self.ignore_fields.as_ref().map(|f| f.idents.contains(ident)).unwrap_or_default() {
              return None
            }

            Some(quote! {
              Self :: #ident => #into_type :: #ident
            })
          })
          .collect();
        quote! {
          impl #impl_generics std::convert::Into<#into_type> for #self_ident #ty_generics #where_clause {
            fn into(self) -> #into_type {
              match self {
                #(#variant_arms),*
              }
            }
          }
        }
      },
    }    
  }
}

#[derive(Debug)]
enum ImplTrait {
  From(TargetType),
  Into(TargetType),
}

#[derive(Debug)]
enum MappingOpts {
  FromType(TargetType),
  IntoType(TargetType),
  DefaultBase(TokenStream),
  AssignFields(MappingAssignFields),
  IgnoreFields(MappingIgnoreFields),
}

#[derive(Debug, Clone)]
enum TargetType {
  Path(Path),
  Tuple(TypeTuple),
  Reference(TypeReference),
}

impl TargetType {
  fn from_lit(v: &Lit) -> Self {
    match v {
      Lit::Str(lit) => {
        let src = lit.value();
        let ty: Type = syn::parse_str(&src)
          .map_err(|err| diagnostic!(lit, Level::Error, "{}: {}", err, src))
          .expect_or_abort("Not a type");
        // if src.trim_start().starts_with("(") {
        //   FromType::Tuple(
        //     syn::parse_str(&src)
        //       .map_err(|err| diagnostic!(lit, Level::Error, err))
        //       .expect_or_abort("Not a tuple type"),
        //   )
        // } else {
        //   FromType::Path(
        //     syn::parse_str(&src)
        //       .map_err(|err| diagnostic!(lit, Level::Error, err))
        //       .expect_or_abort("Not a type"),
        //   )
        // }
        match ty {
          Type::Path(path) => TargetType::Path(path.path),
          Type::Reference(v) => TargetType::Reference(v),
          Type::Tuple(v) => TargetType::Tuple(v),
          _ => abort!(v, "Unsupported type."),
        }
      }
      _ => abort!(v, "Invalid syntax."),
    }
  }
}

impl ToTokens for TargetType {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match *self {
      TargetType::Path(ref v) => v.to_tokens(tokens),
      TargetType::Tuple(ref v) => v.to_tokens(tokens),
      TargetType::Reference(ref v) => v.to_tokens(tokens),
    }
  }
}

impl MappingOpts {
  fn from_nested_meta(meta: &NestedMeta) -> Self {
    match meta {
      NestedMeta::Meta(ref meta) => {
        match meta {
          Meta::Path(_) => {
            abort!(meta, "Invalid syntax.")
          }
          // fields(..)
          Meta::List(ref v) => {
            if let Some(ident) = v.path.get_ident() {
              if ident == "fields" {
                Self::AssignFields(MappingAssignFields::from_meta_list(v))
              } else if ident == "ignore" {
                Self::IgnoreFields(MappingIgnoreFields::from_meta_list(v))
              } else {
                abort!(v, "Unknown option: {}", ident)
              }
            } else {
              abort!(v, "Unknown option.")
            }
          }
          // from_type = ".."
          Meta::NameValue(ref v) => match v.path.get_ident() {
            Some(ident) => {
              let ident = ident.to_string();
              match ident.as_str() {
                "from_type" => Self::FromType(TargetType::from_lit(&v.lit)),
                "into_type" => Self::IntoType(TargetType::from_lit(&v.lit)),
                "default_base" => Self::parse_default_base(&v.lit),
                _ => {
                  abort!(v, "Unknown option.")
                }
              }
            }
            None => {
              abort!(v, "Unknown option.")
            }
          },
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
struct MappingAssignFields {
  fields: Vec<MappingAssignField>,
  span: Span,
}

impl MappingAssignFields {
  fn from_meta_list(v: &MetaList) -> Self {
    Self {
      fields: v
        .nested
        .iter()
        .map(MappingAssignField::from_nested_meta)
        .collect(),
      span: v.span(),
    }
  }
}

#[derive(Debug)]
struct MappingAssignField {
  ident: Ident,
  lit: Lit,
}

impl MappingAssignField {
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
          Meta::NameValue(ref v) => match v.path.get_ident() {
            Some(ident) => Self {
              ident: ident.clone(),
              lit: v.lit.clone(),
            },
            None => {
              abort!(v, ABORT_MESSAGE)
            }
          },
        }
      }
      NestedMeta::Lit(_) => {
        abort!(meta, ABORT_MESSAGE)
      }
    }
  }
}

impl MappingAssignField {
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
    let value_tokens = expr
      .into_tokens(base_tokens)
      .map_err(|err| diagnostic!(self.lit, Level::Error, err))
      .expect_or_abort("Value expression transform");

    let ident = &self.ident;
    quote! {
      #ident : #value_tokens
    }
  }
}

#[derive(Debug)]
struct MappingIgnoreFields {
  idents: Vec<Ident>,
  span: Span,
}

impl MappingIgnoreFields {
  fn from_meta_list(list: &MetaList) -> Self {
    let idents = list.nested.iter().map(|item| {
      match item {
        NestedMeta::Meta(Meta::Path(ref path)) => {
          if let Some(ident) = path.get_ident() {
            ident.clone()
          } else {
            abort!(item, "Expected a identifier");
          }
        }
        _ => {
          abort!(item, "Expected a identifier");
        }
      }
    }).collect();
    Self {
      idents,
      span: list.span().clone(),
    }
  }
}

fn expand_value(ty: &syn::Type, tokens: TokenStream) -> TokenStream {
  match ty {
    Type::Path(syn::TypePath {
      qself: None,
      path: syn::Path {
        leading_colon: None,
        ref segments,
        ..
      }
    }) => {
      if let Some(syn::PathSegment {
        ident,
        arguments: syn::PathArguments::AngleBracketed(_)
      }) = segments.first() { {
        if ident == "Vec" {
          return quote! {
            #tokens .into_iter().map(Into::into).collect()
          }
        } else if ident == "Option" {
          return quote! {
            #tokens .map(Into::into)
          }
        }
      }}
    },
    _ => {}
  }
  quote! {
    #tokens .into()
  }
}
