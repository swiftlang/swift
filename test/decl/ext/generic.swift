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

// Okay: exact match.
extension X<T : P1, U : P2, V> { }
extension Z<T : P1 where T.AssocType : P3> { }

// Okay: infer missing requirements
extension X<T, U, V> { }
extension X<T : P1, U, V> { }
extension X<T, U : P2, V> { }
extension Z<T : P1> { }
extension Z<T> { }

// Bad: extra requirements.
extension X<T : P2, U, V> { } // expected-error{{extension of generic type 'X' cannot add requirements}}
extension X<A, B, C: P3> { } // expected-error{{extension of generic type 'X' cannot add requirements}}

// Bad: wrong number of generic parameters.
extension X<T> { } // expected-error{{extension of generic type 'X' has too few generic parameters (have 1, expected 3)}}
extension X<A, B, C, D> { } // expected-error{{extension of generic type 'X' has too many generic parameters (have 4, expected 3)}}

// Name lookup of generic parameters.
extension X<A, B, C> {
  // Okay: generic parameters from the extension.
  func foo(x: A) -> (B, C) { }

  // Okay: associated types of the generic parameters 
  func bar(x: A.AssocType) { }

  // Ill-formed: generic parameters from the extended type.
  func wibble(x: T) { } // expected-error{{use of undeclared type 'T'}}
}

// Using generic extensions (basic).
func f1<A, B, C, D, E>(x: X<A, B, C>, a: A, assoc: A.AssocType) {
  var (b, c): (B, C) = x.foo(a)
  x.bar(assoc)
}

// Lvalue check when the archetypes are not the same.
struct LValueCheck<T> {
  let x = 0
}

extension LValueCheck<A> {
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

extension MemberTypeCheckB<T> {
  typealias Underlying = MemberTypeCheckA<T>
}

extension MemberTypeCheckB<T> {
  init(_ x: Underlying) { }
}

extension MemberTypeCheckB<T> {
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
