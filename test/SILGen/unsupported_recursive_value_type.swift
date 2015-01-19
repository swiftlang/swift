// RUN: %target-swift-frontend -emit-sil -verify %s

struct SelfRecursiveStruct { // expected-error{{recursive value type}}
  let a: SelfRecursiveStruct
}

struct OptionallyRecursiveStruct { // expected-error{{recursive value type}}
  let a: OptionallyRecursiveStruct?

  init() { a = OptionallyRecursiveStruct() }
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

struct RecursiveWithLocal { // expected-error{{recursive value type 'RecursiveWithLocal' is not allowed}}
  init(t: Local) { self.t = t }
  struct Local {
    init(s: RecursiveWithLocal) { self.s = s }
    var s: RecursiveWithLocal
  }
  var t: Local
}
