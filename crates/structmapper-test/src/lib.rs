use structmapper_codegen::StructMapper;

#[test]
fn test_derive() {
  #[derive(StructMapper)]
  struct From {
    #[struct_mapper(from_value = "{0}")]
    value: i32,
  }

  #[derive(StructMapper)]
  struct To {
    #[struct_mapper(from_value = "{0}")]
    value: i32
  }
}