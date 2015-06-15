// RUN: %target-parse-verify-swift

protocol P1 { typealias AssocType }
protocol P2 : P1 { }
protocol P3 { }

struct X<T : P1, U : P2, V> { 
  struct Inner<A, B : P3> { } // expected-error{{generic type 'Inner' nested in type}}

  struct NonGenericInner { } // expected-error{{nested in generic type}}
}

struct Y {
  struct Inner<A, B : P3> { } // expected-error{{generic type 'Inner' nested in type}}

  struct NonGenericInner { } 
}

struct Z<T : P1 where T.AssocType : P3> { }

extension Int : P1 {
  typealias AssocType = Int
}

extension Double : P2 { 
  typealias AssocType = Double
}

extension X<Int, Double, String> { } // expected-error{{constrained extension must be declared on the unspecialized generic type 'X' with constraints specified by a 'where' clause}}

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
  typealias AssocType

  func foo(a: AssocType)
  init(_ assoc: MemberTypeCheckA<AssocType>)
}

struct MemberTypeCheckB<T> : MemberTypeProto {
  func foo(a: T) {}

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
  var pairs: [(T,T)] {
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

extension Array where T : Hashable {
  var worseHashEver: Int {
    var result = 0
    for elt in self {
      result = (result << 1) ^ elt.hashValue
    }
    return result
  }
}

func notHashableArray<T>(x: [T]) {
  x.worseHashEver // expected-error{{'Int' is not convertible to 'Hashable'}}
}

func hashableArray<T : Hashable>(x: [T]) {
  x.worseHashEver // okay
}

func intArray(x: [Int]) {
  x.worseHashEver
}

class GenericClass<T> { }

extension GenericClass where T : Equatable {
  func foo(x: T, y: T) -> Bool { return x == y }
}

func genericClassEquatable<T : Equatable>(gc: GenericClass<T>, x: T, y: T) {
  gc.foo(x, y: y)
}

func genericClassNotEquatable<T>(gc: GenericClass<T>, x: T, y: T) {
  gc.foo(x, y: y) // expected-error{{cannot invoke 'foo' with an argument list of type '(T, y: T)'}}
}


// FIXME: Future direction
extension Array where T == String { } // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}

extension GenericClass : P3 where T : P3 { } // expected-error{{extension of type 'GenericClass' with constraints cannot have an inheritance clause}}

extension GenericClass where Self : P3 { }
// expected-error@-1{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'GenericClass'?}}
// expected-error@-2{{type 'GenericClass' in conformance requirement does not refer to a generic parameter or associated type}}
