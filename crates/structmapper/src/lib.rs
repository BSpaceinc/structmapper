//! # StructMapper
//!
//! A library to help you generate code that mapped one struct to another.
//!
//! # Example
//! ```
//!   use structmapper::StructMapper;
//!
//!   struct From1 {
//!     value: i32,
//!   }
//!
//!   struct From2 {
//!     value: i32,
//!   }
//!
//!   struct From3 {
//!     value: i32,
//!   }
//!
//!   #[derive(StructMapper)]
//!   #[struct_mapper(from_type = "From1")]
//!   #[struct_mapper(from_type = "From2")]
//!   #[struct_mapper(
//!     from_type = "(From1, From2, From3)",
//!     default_base = "{0}",
//!     fields(
//!       value = "{0.value} + {1.value} + {2.value}"
//!     )
//!   )]
//!   struct To {
//!     value: i32
//!   }
//!
//!   let to = To::from(From1 { value: 1 });
//!   assert_eq!(to.value, 1);
//!
//!   let to = To::from(From2 { value: 2 });
//!   assert_eq!(to.value, 2);
//!
//!   let to = To::from((From1 { value: 1 }, From2 { value: 2 }, From3 { value: 3 }));
//!   assert_eq!(to.value, 1 + 2 + 3);
//! ```

pub use structmapper_codegen::StructMapper;