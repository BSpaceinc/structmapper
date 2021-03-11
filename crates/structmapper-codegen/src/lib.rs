extern crate proc_macro;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

mod derive;
mod value_expr;

use derive::Derive;

#[proc_macro_derive(StructMapper, attributes(struct_mapper))]
#[proc_macro_error::proc_macro_error]
pub fn derive_mapper(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as DeriveInput);
  let derive: Derive = Derive::from_derive_input(&input);
  let tokens = quote!(#derive);
  tokens.into()
}