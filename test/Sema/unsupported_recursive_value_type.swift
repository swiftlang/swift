// RUN: %target-typecheck-verify-swift

struct SelfRecursiveStruct {
  let a: SelfRecursiveStruct // expected-error{{value type 'SelfRecursiveStruct' cannot have a stored property that recursively contains it}}
}

struct OptionallyRecursiveStruct {
  let a: OptionallyRecursiveStruct? // expected-error{{value type 'OptionallyRecursiveStruct' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{cycle beginning here: OptionallyRecursiveStruct? -> (some(_:): OptionallyRecursiveStruct)}}

  init() { a = OptionallyRecursiveStruct() }
}

struct IndirectlyRecursiveStruct1 {
  let a: IndirectlyRecursiveStruct2 // expected-error{{value type 'IndirectlyRecursiveStruct1' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{cycle beginning here: IndirectlyRecursiveStruct2 -> (a: IndirectlyRecursiveStruct1)}}
}
struct IndirectlyRecursiveStruct2 {
  let a: IndirectlyRecursiveStruct1 // expected-error{{value type 'IndirectlyRecursiveStruct2' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{cycle beginning here: IndirectlyRecursiveStruct1 -> (a: IndirectlyRecursiveStruct2)}}
}

enum NonterminatingSelfRecursiveEnum { // expected-error{{recursive enum 'NonterminatingSelfRecursiveEnum' is not marked 'indirect'}} {{1-1=indirect }}
  case A(NonterminatingSelfRecursiveEnum) // expected-note {{recursive case here}}
}

enum TerminatingSelfRecursiveEnum { // expected-error{{recursive enum 'TerminatingSelfRecursiveEnum' is not marked 'indirect'}} {{1-1=indirect }}
  case A(TerminatingSelfRecursiveEnum) // expected-note {{recursive case here}}
  case B
}

enum IndirectlyRecursiveEnum1 { // expected-error{{recursive enum 'IndirectlyRecursiveEnum1' is not marked 'indirect'}} {{1-1=indirect }}
  case A(IndirectlyRecursiveEnum2)
  // expected-note@-1 {{cycle beginning here: IndirectlyRecursiveEnum2 -> (A(_:): IndirectlyRecursiveEnum1)}}
}
enum IndirectlyRecursiveEnum2 { // expected-error{{recursive enum 'IndirectlyRecursiveEnum2' is not marked 'indirect'}}
  case A(IndirectlyRecursiveEnum1)
  // expected-note@-1 {{cycle beginning here: IndirectlyRecursiveEnum1 -> (A(_:): IndirectlyRecursiveEnum2)}}
}

enum RecursiveByGenericSubstitutionEnum<T> {
  case A(T)
}

enum InconstructibleEnum1 { // expected-warning {{enum containing only recursive cases is impossible to instantiate}}
  indirect case A(InconstructibleEnum1)
}
enum InconstructibleEnum2 { // OK
  indirect case A(InconstructibleEnum2)
  case B(Bool)
  indirect case C(Int, InconstructibleEnum2)
}
enum InconstructibleEnum3 { // expected-warning {{enum containing only recursive cases is impossible to instantiate}}
  indirect case B(Int, InconstructibleEnum3)
}
indirect enum InconstructibleEnum4 {
  // expected-warning@-1 {{enum containing only recursive cases is impossible to instantiate}}
  case A(InconstructibleEnum4)
}
indirect enum InconstructibleEnum5 {
  // expected-warning@-1 {{enum containing only recursive cases is impossible to instantiate}}
  case B(Int, InconstructibleEnum5)
}
indirect enum InconstructibleEnum6 { // OK
  case A(InconstructibleEnum6)
  case B(Bool)
  case C(Int, InconstructibleEnum6)
}

struct RecursiveByBeingInTupleStruct {
  let a: (Int, RecursiveByBeingInTupleStruct) // expected-error{{value type 'RecursiveByBeingInTupleStruct' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{cycle beginning here: (Int, RecursiveByBeingInTupleStruct) -> (.1: RecursiveByBeingInTupleStruct)}}
}

struct OptionallySelfRecursiveStruct { // expected-error{{value type 'OptionallySelfRecursiveStruct' has infinite size}}
  let a: Optional<OptionallyRecursiveStruct>
  // expected-note@-1 {{cycle beginning here: Optional<OptionallyRecursiveStruct> -> (some(_:): OptionallyRecursiveStruct) -> (a: OptionallyRecursiveStruct?)}}
}

enum OptionallySelfRecursiveEnum { // expected-error{{recursive enum 'OptionallySelfRecursiveEnum' is not marked 'indirect'}}
  case A(Optional<OptionallySelfRecursiveEnum>)
  // expected-note@-1 {{cycle beginning here: Optional<OptionallySelfRecursiveEnum> -> (some(_:): OptionallySelfRecursiveEnum)}}
}

// self-recursive struct with self as member's type argument, a proper name would
// be too long.
struct X<T> { // expected-error{{value type 'X<T>' has infinite size}}
  let s: X<X>
  // expected-note@-1 {{cycle beginning here: X<X<T>> -> (s: X<X<X<T>>>) -> (s: X<X<X<X<T>>>>) -> (s: X<X<X<X<X<T>>>>>) -> (s: X<X<X<X<X<X<T>>>>>>) -> (s: X<X<X<X<X<X<X<T>>>>>>>) -> ...}}
}

// self-recursive enum with self as generic argument associated type, a proper
// name would be too long
enum Y<T> { // expected-error{{value type 'Y<T>' has infinite size}}
    case A(Int, Y<Y>)
  // expected-note@-1 {{cycle beginning here: (Int, Y<Y<T>>) -> (.1: Y<Y<T>>) -> (A(_:_:): (Int, Y<Y<Y<T>>>)) -> (.1: Y<Y<Y<T>>>) -> (A(_:_:): (Int, Y<Y<Y<Y<T>>>>)) -> (.1: Y<Y<Y<Y<T>>>>) -> ...}}
}

// ultra super nest-acular type
struct Z<T, U> { // expected-error{{value type 'Z<T, U>' has infinite size}}
    let a: Z<Optional<Z<Z<Z, Z>, X<Z>>>, (Int, Z)>
    // expected-note@-1 {{cycle beginning here}}
}

struct RecursiveByGenericSubstitutionStruct {
  let a: RecursiveByGenericSubstitutionEnum<RecursiveByGenericSubstitutionStruct>
  // expected-error@-1{{value type 'RecursiveByGenericSubstitutionStruct' cannot have a stored property that recursively contains it}}
  // expected-note@-2 {{cycle beginning here: RecursiveByGenericSubstitutionEnum<RecursiveByGenericSubstitutionStruct> -> (A(_:): RecursiveByGenericSubstitutionStruct)}}
}

struct RecursiveWithLocal {
  init(t: Local) { self.t = t }
  struct Local {
    init(s: RecursiveWithLocal) { self.s = s }
    var s: RecursiveWithLocal // expected-error{{value type 'RecursiveWithLocal.Local' cannot have a stored property that recursively contains it}}
    // expected-note@-1 {{RecursiveWithLocal -> (t: RecursiveWithLocal.Local)}}
  }
  var t: Local // expected-error{{value type 'RecursiveWithLocal' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{RecursiveWithLocal.Local -> (s: RecursiveWithLocal)}}
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

struct Bad {
    var s: Holder<NoStorage> // expected-error{{value type 'Bad' cannot have a stored property that recursively contains it}}
    // expected-note@-1 {{cycle beginning here: Holder<NoStorage> -> (x: NoStorage.Holding)}}
}

// FIXME: this diagnostic is unnecessary
struct Test1 { // expected-error {{value type 'Test1' has infinite size}}
  var test1: StructCyclesWithEnum<Int>
  // expected-note@-1 {{cycle beginning here: StructCyclesWithEnum<Int> -> (sField: EnumCyclesWithStruct<Int>) -> (eCase(_:): StructCyclesWithEnum<Int>)}}
}

struct StructCyclesWithEnum<T> {
  var sField: EnumCyclesWithStruct<T> // expected-error {{value type 'StructCyclesWithEnum<T>' cannot have a stored property that recursively contains it}}
  // expected-note@-1 {{cycle beginning here: EnumCyclesWithStruct<T> -> (eCase(_:): StructCyclesWithEnum<T>)}}
}

enum EnumCyclesWithStruct<T> { // expected-error {{recursive enum 'EnumCyclesWithStruct<T>' is not marked 'indirect'}}
  case eCase(StructCyclesWithEnum<T>)
  // expected-note@-1 {{cycle beginning here: StructCyclesWithEnum<T> -> (sField: EnumCyclesWithStruct<T>)}}
}

struct Test2 {
  var test1: Test1
}

struct IB<T> { // expected-error {{value type 'IB<T>' has infinite size}}
  var member: IB<T?>
  // expected-note@-1 {{cycle beginning here: IB<T?> -> (member: IB<T??>) -> (member: IB<T???>) -> (member: IB<T????>) -> (member: IB<T?????>) -> (member: IB<T??????>) -> ...}}
}
