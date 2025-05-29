// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx12 %s
// REQUIRES: OS=macosx


protocol P { }
extension Int: P { }

protocol P1<A, B> {
  associatedtype A

  @available(macOS 13, *)
  associatedtype B: P
}

@available(macOS 13, *)
struct ModelP1<A, B: P>: P1 {
}

// Associated types in where clauses
func testWhereBad<T: P1, U>(_: T) where T.B == U { }
// expected-error@-1{{'B' is only available in macOS 13 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

@available(macOS 13, *)
func testWhereGood<T: P1, U>(_: T) where T.B == U { }

// Associated types in opaque parameter position
func testPrimaryOpaqueParamBad<U>(_: some P1<some Any, U>) {}
// expected-error@-1 2{{'B' is only available in macOS 13 or newer}}
// expected-note@-2 2{{add '@available' attribute to enclosing global function}}

@available(macOS 13, *)
func testPrimaryOpaqueParamGood<U: P>(_: some P1<some Any, U>) {}

// Associated types in opaque result position
func testPrimaryOpaqueResultBad<U: P>() -> some P1<String, U> {
// expected-error@-1{{'B' is only available in macOS 13 or newer}}
// expected-note@-2 2{{add '@available' attribute to enclosing global function}}
  return ModelP1<String, U>()
  // expected-error@-1{{'ModelP1' is only available in macOS 13 or newer}}
  // expected-note@-2{{add 'if #available' version check}}
}

@available(macOS 13, *)
func testPrimaryOpaqueResultGood<U: P>() -> some P1<String, U> {
  return ModelP1<String, U>()
}

// Associated types in existentials
func testPrimaryExistentialBad<U>(_: any P1<Int, U>) {}
// expected-error@-1{{'B' is only available in macOS 13 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

@available(macOS 13, *)
func testPrimaryExistentialGood<U>(_: any P1<Int, U>) {}


@available(macOS 13, *)
protocol P2 {
  associatedtype A

  @available(macOS 14, *)
  associatedtype B

  var a: A { get }

  @available(macOS 14, *)
  var b: B { get }
}

struct ModelP2: P2 {
  var a: Int
  var b: Double
}

extension ModelP2 {
  // expected-note@-1{{add '@available' attribute to enclosing extension}}

  // Ok, the inferred typealias for A is always available.
  func takesA(_ a: A) {}

  // Bad, the inferred typealias for B is introduced with associated type B.
  func takesB(_ b: B) {}
  // expected-error@-1{{'B' is only available in macOS 14 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing instance method}}
}

protocol P3 {
  @available(macOS, obsoleted: 12) // expected-error{{associated type cannot be marked unavailable with '@available'}}
  associatedtype A1

  @available(macOS, obsoleted: 99)
  associatedtype A2
}
