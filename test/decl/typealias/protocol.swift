// RUN: %target-typecheck-verify-swift

// Tests for typealias inside protocols

protocol Bad {
  associatedtype X<T>  // expected-error {{associated types may not have a generic parameter list}}
  typealias Y<T>       // expected-error {{expected '=' in typealias declaration}}
}

protocol Col {
  associatedtype Elem
  typealias E = Elem?
  var elem: Elem { get }
}

protocol CB {
  associatedtype C : Col

  // Self at top level
  typealias S = Self
  func cloneDefinitely() -> S

  // Self in bound generic position
  typealias OS = Self?
  func cloneMaybe() -> OS

  // Associated type of archetype
  typealias E = C.Elem
  func setIt(_ element: E)

  // Typealias of archetype
  typealias EE = C.E
  func setItSometimes(_ element: EE)

  // Associated type in bound generic position
  typealias OE = C.Elem?
  func setItMaybe(_ element: OE)

  // Complex type expression
  typealias FE = (C) -> (C.Elem) -> Self
  func teleport(_ fn: FE)
}

// Generic signature requirement involving protocol typealias
func go1<T : CB, U : Col>(_ col: U, builder: T) where U.Elem == T.E { // OK
  builder.setIt(col.elem)
}
func go2<T : CB, U : Col>(_ col: U, builder: T) where U.Elem == T.C.Elem { // OK
  builder.setIt(col.elem)
}

// Test for same type requirement with typealias == concrete
func go3<T : CB>(_ builder: T) where T.E == Int {
  builder.setIt(1)
}

// Test for conformance to protocol with associatedtype and another with
// typealias with same name
protocol MyIterator {
  associatedtype Elem

  func next() -> Elem?
}

protocol MySeq {
  associatedtype I : MyIterator
  typealias Elem = Self.I.Elem

  func makeIterator() -> I
  func getIndex(_ i: Int) -> Elem
  func first() -> Elem
}

extension MySeq where Self : MyIterator {
  func makeIterator() -> Self {
    return self
  }
}

func plusOne<S: MySeq>(_ s: S, i: Int) -> Int where S.Elem == Int {
  return s.getIndex(i) + 1
}

struct OneIntSeq: MySeq, MyIterator {
  let e : Float

  func next() -> Float? {
    return e
  }

  func getIndex(_ i: Int) -> Float {
    return e
  }
}

// test for conformance correctness using typealias in extension
extension MySeq {
  func first() -> Elem {
    return getIndex(0)
  }
}

// Specific diagnosis for trying to use complex typealiases in generic constraints
protocol P1 {
    associatedtype A
    typealias F = (A) -> ()
}

protocol P2 {
    associatedtype B
}

func go3<T : P1, U : P2>(_ x: T) -> U where T.F == U.B {
}

// Specific diagnosis for things that look like Swift 2.x typealiases
protocol P3 {
  typealias T // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}
  typealias U : P2 // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}

  associatedtype V : P2 = // expected-error {{expected type in associatedtype declaration}}
}

// Test for not crashing on recursive aliases
protocol Circular {
  typealias Y = Self.Y // expected-error {{type alias 'Y' is not a member type of 'Self'}}

  typealias Y2 = Y2 // expected-error {{type alias 'Y2' references itself}}
  // expected-note@-1 {{type declared here}}

  typealias Y3 = Y4 // expected-note {{type declared here}}
  typealias Y4 = Y3 // expected-error {{type alias 'Y3' references itself}}
}

// Qualified and unqualified references to protocol typealiases from concrete type
protocol P5 {
  associatedtype A
  typealias X = Self
  typealias T1 = Int
  typealias T2 = A
  var a: T2 { get }
}

protocol P6 {
  typealias A = Int
  typealias B = Self
}

struct T5 : P5 {
  // This is OK -- the typealias is fully concrete
  var a: P5.T1 // OK

  // Invalid -- cannot represent associated type of existential
  var v2: P5.T2 // expected-error {{typealias 'T2' can only be used with a concrete type or generic parameter base}}
  var v3: P5.X // expected-error {{typealias 'X' can only be used with a concrete type or generic parameter base}}

  // Unqualified reference to typealias from a protocol conformance
  var v4: T1 // OK
  var v5: T2 // OK

  // Qualified reference
  var v6: T5.T1 // OK
  var v7: T5.T2 // OK

  var v8 = P6.A.self
  var v9 = P6.B.self // expected-error {{typealias 'B' can only be used with a concrete type or generic parameter base}}
}

// Unqualified lookup finds typealiases in protocol extensions, though
protocol P7 {
  associatedtype A
}

extension P7 {
  typealias Y = A
}

struct S7 : P7 {
  typealias A = Int

  func inTypeContext(y: Y) { }

  func inExpressionContext() {
    _ = Y.self
  }
}

protocol P8 {
  associatedtype B

  @available(*, unavailable, renamed: "B")
  typealias A = B // expected-note{{'A' has been explicitly marked unavailable here}}
}

func testP8<T: P8>(_: T) where T.A == Int { } // expected-error{{'A' has been renamed to 'B'}}{{34-35=B}}
