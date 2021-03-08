#[test]
fn test_derive() {
  use structmapper_codegen::StructMapper;

  struct From {
    value: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(
    from_type = "(T1, T2)",
    self_value = "{0}",
    fields(
      value = "{value} * 2"
    )
  )]
  #[struct_mapper(from_type = "From2")]
  #[struct_mapper(from_type = "From2")]
  struct To {
    value: i32
  }
}