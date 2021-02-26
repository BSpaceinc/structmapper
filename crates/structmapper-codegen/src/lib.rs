extern crate proc_macro;
#[macro_use]
extern crate darling;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};
use crate::darling::FromDeriveInput;

mod receiver;
use receiver::Receiver;

#[proc_macro_derive(StructMapper, attributes(struct_mapper))]
#[proc_macro_error::proc_macro_error]
pub fn derive_mapper(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as DeriveInput);
  let receiver: Receiver = Receiver::from_derive_input(&input).unwrap();
  let tokens = quote!(#receiver);
  tokens.into()
}