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

extension X<Int, Double, String> { } // expected-error{{cannot extend a specialization of 'X'}}

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

// FIXME: Future direction
extension Array where T == String { } // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}
