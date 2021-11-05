#[test]
fn test_from() {
  use structmapper::StructMapper;

  struct From1 {
    base: i32,
    value: i32,
  }

  struct From2 {
    value: i32,
  }

  struct From3 {
    value: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(from_type = "From1", fields(base = "Default::default()"))]
  #[struct_mapper(from_type = "From2", fields(base = "Default::default()"))]
  #[struct_mapper(
    from_type = "(From1, From2, From3)",
    default_base = "{0}",
    fields(value = "{0.value} + {1.value} + {2.value}")
  )]
  struct To {
    base: i32,
    value: i32,
  }

  let to = To::from(From1 { base: 0, value: 1 });
  assert_eq!(to.value, 1);

  let to = To::from(From2 { value: 2 });
  assert_eq!(to.value, 2);

  let to = To::from((
    From1 { base: 42, value: 1 },
    From2 { value: 2 },
    From3 { value: 3 },
  ));
  assert_eq!(to.base, 42);
  assert_eq!(to.value, 1 + 2 + 3);
}

#[test]
fn test_ref() {
  use structmapper::StructMapper;

  #[allow(unused)]
  struct From {
    v: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(from_type = "&From")]
  struct To {
    v: i32,
  }

  let to = To::from(&From { v: 1 });
  assert_eq!(to.v, 1);
}

#[test]
fn test_field_override() {
  use structmapper::StructMapper;

  #[allow(unused)]
  struct From {
    a: i32,
    b: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(from_type = "From", fields(a = "1234"))]
  struct To {
    a: i32,
    b: i32,
  }

  let to = To::from(From { a: 1, b: 2 });
  assert_eq!(to.a, 1234);
  assert_eq!(to.b, 2);
}

#[test]
fn test_into() {
  use structmapper::StructMapper;

  struct Into1 {
    base: i32,
    value: i32,
  }

  struct Into2 {
    base: i32,
    value: i32,
  }

  struct Into3 {
    value: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(into_type = "Into1")]
  #[struct_mapper(into_type = "Into2", fields(base = "Default::default()"))]
  #[struct_mapper(into_type = "Into3", ignore(base))]
  struct From {
    base: i32,
    value: i32,
  }

  let to: Into1 = From {
    base: 1111,
    value: 1,
  }
  .into();
  assert_eq!(to.base, 1111);
  assert_eq!(to.value, 1);

  let to: Into2 = From {
    base: 123,
    value: 1,
  }
  .into();
  assert_eq!(to.base, 0);
  assert_eq!(to.value, 1);

  let to: Into3 = From { base: 0, value: 2 }.into();
  assert_eq!(to.value, 2);
}

#[test]
fn test_nested() {
  use structmapper::StructMapper;

  struct Inner {
    value: i32,
  }

  impl Inner {
    fn new(value: i32) -> Self {
      Inner { value }
    }
  }

  struct Outer {
    value: i32,
    inner: Inner,
    list: Vec<Inner>,
  }

  #[derive(StructMapper)]
  #[struct_mapper(into_type = "Inner")]
  #[struct_mapper(into_type = "Outer", fields(
    inner = "Inner::new({value} * 2)",
    list = "vec![]"
  ))]
  struct FromInner {
    value: i32,
  }

  #[derive(StructMapper)]
  #[struct_mapper(into_type = "Outer")]
  struct FromOuter {
    value: i32,
    inner: FromInner,
    list: Vec<FromInner>,
  }

  let to: Outer = FromOuter {
    value: 1,
    inner: FromInner { value: 2 },
    list: vec![
      FromInner { value: 3 },
    ]
  }
  .into();
  assert_eq!(to.value, 1);
  assert_eq!(to.inner.value, 2);
  assert_eq!(to.list[0].value, 3);

  let to: Outer = FromInner { value: 4 }.into();
  assert_eq!(to.value, 4);
  assert_eq!(to.inner.value, 8);
}

#[test]
fn test_enum() {
  use structmapper::StructMapper;

  #[derive(Debug, PartialEq, Eq)]
  enum Enum1 {
    A,
    B,
  }

  #[derive(Debug, PartialEq, Eq, StructMapper)]
  #[struct_mapper(from_type = "Enum1")]
  #[struct_mapper(into_type = "Enum1")]
  enum Enum2 {
    A,
    B,
  }

  let v: Enum1 = Enum2::A.into();
  assert_eq!(v, Enum1::A); 

  let v: Enum2 = Enum2::from(Enum1::B);
  assert_eq!(v, Enum2::B); 
}