// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s

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

enum NonterminatingSelfRecursiveEnum { // expected-error{{recursive enum 'NonterminatingSelfRecursiveEnum' is not marked 'indirect'}} {{1-1=indirect }}
  case A(NonterminatingSelfRecursiveEnum)
}

enum TerminatingSelfRecursiveEnum { // expected-error{{recursive enum 'TerminatingSelfRecursiveEnum' is not marked 'indirect'}} {{1-1=indirect }}
  case A(TerminatingSelfRecursiveEnum)
  case B
}

enum IndirectlyRecursiveEnum1 { // expected-error{{recursive enum 'IndirectlyRecursiveEnum1' is not marked 'indirect'}} {{1-1=indirect }}
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
