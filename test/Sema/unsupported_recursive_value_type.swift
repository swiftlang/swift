// RUN: %target-typecheck-verify-swift

struct SelfRecursiveStruct { // expected-error{{value type 'SelfRecursiveStruct' cannot have a stored property that references itself}}
  let a: SelfRecursiveStruct
}

struct OptionallyRecursiveStruct { // expected-error{{value type 'OptionallyRecursiveStruct' cannot have a stored property that references itself}}
  let a: OptionallyRecursiveStruct?

  init() { a = OptionallyRecursiveStruct() }
}

struct IndirectlyRecursiveStruct1 { // expected-error{{value type 'IndirectlyRecursiveStruct1' cannot have a stored property that references itself}}
  let a: IndirectlyRecursiveStruct2
}
struct IndirectlyRecursiveStruct2 { // expected-error{{value type 'IndirectlyRecursiveStruct2' cannot have a stored property that references itself}}
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
enum IndirectlyRecursiveEnum2 { // expected-error{{recursive enum 'IndirectlyRecursiveEnum2' is not marked 'indirect'}}
  case A(IndirectlyRecursiveEnum1)
}

enum RecursiveByGenericSubstitutionEnum<T> {
  case A(T)
}

struct RecursiveByBeingInTupleStruct { // expected-error{{value type 'RecursiveByBeingInTupleStruct' cannot have a stored property that references itself}}
  let a: (Int, RecursiveByBeingInTupleStruct)
}

struct OptionallySelfRecursiveStruct { // expected-error{{value type 'OptionallySelfRecursiveStruct' cannot have a stored property that references itself}}
  let a: Optional<OptionallyRecursiveStruct>
}

enum OptionallySelfRecursiveEnum { // expected-error{{recursive enum 'OptionallySelfRecursiveEnum' is not marked 'indirect'}}
  case A(Optional<OptionallySelfRecursiveEnum>)
}

// self-recursive struct with self as member's type argument, a proper name would
// be too long.
struct X<T> { // expected-error{{value type 'X<T>' cannot have a stored property that references itself}}
  let s: X<X>
}

// self-recursive enum with self as generic argument associated type, a proper
// name would be too long
enum Y<T> { // expected-error{{recursive enum 'Y<T>' is not marked 'indirect'}}
    case A(Int, Y<Y>)
}

// ultra super nest-acular type
struct Z<T, U> { // expected-error{{value type 'Z<T, U>' cannot have a stored property that references itself}}
    let a: Z<Optional<Z<Z<Z, Z>, X<Z>>>, (Int, Z)>
}

struct RecursiveByGenericSubstitutionStruct { // expected-error{{value type 'RecursiveByGenericSubstitutionStruct' cannot have a stored property that references itself}}
  let a: RecursiveByGenericSubstitutionEnum<RecursiveByGenericSubstitutionStruct>
}

struct RecursiveWithLocal { // expected-error{{value type 'RecursiveWithLocal' cannot have a stored property that references itself}}
  init(t: Local) { self.t = t }
  struct Local { // expected-error{{value type 'RecursiveWithLocal.Local'}}
    init(s: RecursiveWithLocal) { self.s = s }
    var s: RecursiveWithLocal
  }
  var t: Local
}

struct R<T> { let t: () -> T }
struct S { let r: R<S> } // S should be valid in this scenario


// It's valid for ParentStruct.B to contain ParentStruct.A
struct ParentStruct {
    struct A {}
    struct B {
        let s: A
    }
}

// another valid case
struct Outer {
    struct Inner {
        var o: Outer
    }
}

// nested generic parameters are valid
struct NestedGenericParamStruct {
    let n: [[Int]]
}
struct NestedGenericParamEnum {
    let n: Int??
}

// Neither 'Bad' nor 'Holder' appear in generic parameter of 'Bad', but
// recursion happens anyways.
protocol Holdable {
    associatedtype Holding
}

struct Holder<T : Holdable> {
    let x: T.Holding
}

struct NoStorage : Holdable {
    typealias Holding = Bad
}

struct Bad { // expected-error{{value type 'Bad' cannot have a stored property that references itself}}
    var s: Holder<NoStorage>
}
