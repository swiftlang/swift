// RUN: %swift -emit-silgen -verify %s

struct SelfRecursiveStruct { // expected-error{{recursive value type}}
  let a: SelfRecursiveStruct
}

struct OptionallyRecursiveStruct { // expected-error{{recursive value type}}
  let a: OptionallyRecursiveStruct?
}

struct IndirectlyRecursiveStruct1 { // expected-error{{recursive value type}}
  let a: IndirectlyRecursiveStruct2
}
struct IndirectlyRecursiveStruct2 {
  let a: IndirectlyRecursiveStruct1
}

enum NonterminatingSelfRecursiveEnum { // expected-error{{recursive value type}}
  case A(NonterminatingSelfRecursiveEnum)
}

enum TerminatingSelfRecursiveEnum { // expected-error{{recursive value type}}
  case A(TerminatingSelfRecursiveEnum)
  case B
}

enum IndirectlyRecursiveEnum1 { // expected-error{{recursive value type}}
  case A(IndirectlyRecursiveEnum2)
}
enum IndirectlyRecursiveEnum2 {
  case A(IndirectlyRecursiveEnum1)
}

enum RecursiveByGenericSubstitutionEnum<T> {
  case A(T)
}
struct RecursiveByGenericSubstitutionStruct { // expected-error{{recursive value type}}
  let a: RecursiveByGenericSubstitutionEnum<RecursiveByGenericSubstitutionStruct>
}
