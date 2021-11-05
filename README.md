# StructMapper

A library to help you generate code that mapped one struct to another.

# Example

## Generate `From<T>`

```rust
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
```

## Generate `Into<T>`

```rust
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

let to: Into1 = From { base: 1111, value: 1 }.into();
assert_eq!(to.base, 1111);
assert_eq!(to.value, 1);

let to: Into2 = From { base: 123, value: 1 }.into();
assert_eq!(to.base, 0);
assert_eq!(to.value, 1);

let to: Into3 = From { base: 0, value: 2 }.into();
assert_eq!(to.value, 2);
```

## Enums

```rust
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
```