// RUN: %target-parse-verify-swift -enable-protocol-typealiases

// Tests for typealias inside protocols

protocol Col {
  associatedtype Elem
  typealias E = Elem?
  var elem: Elem { get }
}

protocol CB {
  associatedtype C : Col
  typealias OS = Self?
  typealias E = C.Elem
  typealias EE = C.E
  typealias OE = C.Elem?
  typealias FE = (C) -> (C.Elem) -> Self
 
  func cloneMaybe() -> OS
  func setIt(_ element: E)
  func setItMaybe(_ element: OE)
  func setItMaybe2(_ element: EE)
  func teleport(_ fn: FE)
}

func go1<T : CB, U : Col where U.Elem == T.E>(_ col: U, builder: T) { // OK
  builder.setIt(col.elem)
}
func go2<T : CB, U : Col where U.Elem == T.C.Elem>(_ col: U, builder: T) { // OK
  builder.setIt(col.elem)
}

// Test for same type requirement with typealias == concrete
func go3<T : CB where T.E == Int>(_ builder: T) {
  builder.setIt(1)
}

// Test for conformance to protocol with associatedtype and another with typealias with same name.
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

func plusOne<S: MySeq where S.Elem == Int>(_ s: S, i: Int) -> Int {
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

func go3<T : P1, U : P2 where T.F == U.B>(_ x: T) -> U { // expected-error {{typealias 'F' is too complex to be used as a generic constraint; use an associatedtype instead}} expected-error {{'F' is not a member type of 'T'}}
}

// Specific diagnosis for things that look like Swift 2.x typealiases
protocol P3 {
  typealias T // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}
  typealias U : P2 // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}
  
  associatedtype V : P2 = // expected-error {{expected type in associatedtype declaration}}
}

// Test for not crashing on self and recursive aliases
protocol P4 {
  typealias X = Self
  typealias Y = Self.Y // expected-error {{type alias 'Y' circularly references itself}}
  
  func getSelf() -> X
}

// Availability of typealiases in protocols for nested type lookup
protocol P5 {
  associatedtype A
  typealias T1 = Int
  typealias T2 = A
  var a: T2 { get }
}

struct T5 : P5 {
  var a: P5.T1 // OK
  var v2: P5.T2 // expected-error {{cannot use typealias 'T2' of associated type 'A' outside of its protocol}}
  var v3: P4.X // expected-error {{cannot use typealias 'X' of associated type 'Self' outside of its protocol}}

  // Unqualified reference to typealias from a protocol conformance
  var v4: T1 // OK
  var v5: T2 // OK

  // Qualified reference
  var v6: T5.T1 // OK
  var v7: T5.T2 // OK
}

