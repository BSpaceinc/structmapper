use darling::ast::Data;
use darling::util::Ignored;
use darling::error::Result;
use quote::ToTokens;
use proc_macro2::TokenStream;
use darling::FromMeta;

#[derive(Debug, Default, FromMeta)]
#[darling(default)]
struct FromOpts {
  #[darling(rename = "sit")]
  ipsum: bool,
  dolor: Option<String>,
}

struct ValueExpr(syn::Expr);

impl FromMeta for ValueExpr {
  fn from_string(value: &str) -> Result<Self> {

  }
}

#[derive(Debug, FromField)]
#[darling(attributes(struct_mapper))]
struct FieldReceiver {
  ident: Option<syn::Ident>,
  ty: syn::Type,
  #[darling(default)]
  from_value: Option<syn::LitStr>,
  #[darling(default)]
  to_value: Option<syn::LitStr>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(struct_mapper), forward_attrs(allow, doc, cfg),  supports(struct_named))]
pub struct Receiver {
  ident: syn::Ident,
  attrs: Vec<syn::Attribute>,
  data: Data<Ignored ,FieldReceiver>,
  #[darling(default)]
  from: Option<FromOpts>,
}

impl ToTokens for Receiver {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let data = self.data.as_ref().take_struct().unwrap();
    for field in data.fields {
      panic!("{:?}", field.from_value.as_ref().unwrap())
    }

    unimplemented!()
  }
}