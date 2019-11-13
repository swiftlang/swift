// RUN: %target-typecheck-verify-swift

protocol P1 { associatedtype AssocType }
protocol P2 : P1 { }
protocol P3 { }

struct X<T : P1, U : P2, V> { 
  struct Inner<A, B : P3> { }

  struct NonGenericInner { }
}

extension Int : P1 {
  typealias AssocType = Int
}

extension Double : P2 { 
  typealias AssocType = Double
}

extension X<Int, Double, String> {
// expected-error@-1{{constrained extension must be declared on the unspecialized generic type 'X' with constraints specified by a 'where' clause}}
  let x = 0
  // expected-error@-1 {{extensions must not contain stored properties}}
  static let x = 0
  // expected-error@-1 {{static stored properties not supported in generic types}}
  func f() -> Int {}
  class C<T> {}
}

typealias GGG = X<Int, Double, String>

extension GGG { } // okay through a typealias

// Lvalue check when the archetypes are not the same.
struct LValueCheck<T> {
  let x = 0
}

extension LValueCheck {
  init(newY: Int) {
    x = 42
  }
}

// Member type references into another extension.
struct MemberTypeCheckA<T> { }

protocol MemberTypeProto {
  associatedtype AssocType

  func foo(_ a: AssocType)
  init(_ assoc: MemberTypeCheckA<AssocType>)
}

struct MemberTypeCheckB<T> : MemberTypeProto {
  func foo(_ a: T) {}

  typealias Element = T
  var t1: T
}

extension MemberTypeCheckB {
  typealias Underlying = MemberTypeCheckA<T>
}

extension MemberTypeCheckB {
  init(_ x: Underlying) { }
}

extension MemberTypeCheckB {
  var t2: Element { return t1 }  
}

// rdar://problem/19795284
extension Array {
  var pairs: [(Element, Element)] {
    get {
      return []
    }
  }
}

// rdar://problem/21001937
struct GenericOverloads<T, U> {
  var t: T
  var u: U

  init(t: T, u: U) { self.t = t; self.u = u }

  func foo() { }

  var prop: Int { return 0 }

  subscript (i: Int) -> Int { return i }
}

extension GenericOverloads where T : P1, U : P2 {
  init(t: T, u: U) { self.t = t; self.u = u }

  func foo() { }

  var prop: Int { return 1 }

  subscript (i: Int) -> Int { return i }
}

extension Array where Element : Hashable { // expected-note {{where 'Element' = 'T'}}
  var worseHashEver: Int {
    var result = 0
    for elt in self {
      result = (result << 1) ^ elt.hashValue
    }
    return result
  }
}

func notHashableArray<T>(_ x: [T]) {
  x.worseHashEver // expected-error{{property 'worseHashEver' requires that 'T' conform to 'Hashable'}}
}

func hashableArray<T : Hashable>(_ x: [T]) {
  // expected-warning @+1 {{unused}}
  x.worseHashEver // okay
}

func intArray(_ x: [Int]) {
  // expected-warning @+1 {{unused}}
  x.worseHashEver
}

class GenericClass<T> { }

extension GenericClass where T : Equatable { // expected-note {{where 'T' = 'T'}}
  func foo(_ x: T, y: T) -> Bool { return x == y }
}

func genericClassEquatable<T : Equatable>(_ gc: GenericClass<T>, x: T, y: T) {
  _ = gc.foo(x, y: y)
}

func genericClassNotEquatable<T>(_ gc: GenericClass<T>, x: T, y: T) {
  gc.foo(x, y: y) // expected-error{{referencing instance method 'foo(_:y:)' on 'GenericClass' requires that 'T' conform to 'Equatable'}}
}


extension Array where Element == String { }

extension GenericClass : P3 where T : P3 { }

extension GenericClass where Self : P3 { }
// expected-error@-1{{covariant 'Self' can only appear as the type of a property, subscript or method result; did you mean 'GenericClass'?}} {{30-34=GenericClass}}
// expected-error@-2{{'GenericClass<T>' in conformance requirement does not refer to a generic parameter or associated type}}

protocol P4 {
  associatedtype T
  init(_: T)
}

protocol P5 { }

struct S4<Q>: P4 {
  init(_: Q) { }
}

extension S4 where T : P5 {}

struct S5<Q> {
  init(_: Q) { }
}

extension S5 : P4 {}

// rdar://problem/21607421
public typealias Array2 = Array
extension Array2 where QQQ : VVV {}
// expected-error@-1 {{use of undeclared type 'QQQ'}}
// expected-error@-2 {{use of undeclared type 'VVV'}}

// https://bugs.swift.org/browse/SR-9009
func foo() {
  extension Array where Element : P1 {
  // expected-error@-1 {{declaration is only valid at file scope}}
    func foo() -> Element.AssocType {}
  }
}

// Deeply nested
protocol P6 {
  associatedtype Assoc1
  associatedtype Assoc2
}

struct A<T, U, V> {
  struct B<W, X, Y> {
    struct C<Z: P6> {
    }
  }
}

extension A.B.C where T == V, X == Z.Assoc2 {
  func f() { }
}

// Extensions of nested non-generics within generics.
extension A.B {
  struct D { }
}

extension A.B.D {
  func g() { }
}

// rdar://problem/43955962
struct OldGeneric<T> {}
typealias NewGeneric<T> = OldGeneric<T>

extension NewGeneric {
  static func oldMember() -> OldGeneric {
    return OldGeneric()
  }

  static func newMember() -> NewGeneric {
    return NewGeneric()
  }
}
