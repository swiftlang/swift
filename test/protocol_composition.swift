// RUN: %swift %s -verify

protocol P1 { func p1() }
protocol P2 : P1 { func p2() }
protocol P3 { func p3() }
protocol P4 : P3 { func p4() }

typealias Any = protocol<>
typealias Any2 = protocol< >

func testEquality() {
  // Remove duplicates from protocol-conformance types. 
  var x1 : (_ : protocol<P2, P4>) -> ()
  var x2 : (_ : protocol<P3, P4, P2, P1>) -> ()
  x1 = x2

  // Singleton protocol-conformance types, after duplication, are the same as
  // simply naming the protocol type.
  var x3 : (_ : protocol<P2, P1>) -> ()
  var x4 : (_ : P2) -> ()
  x3 = x4

  // Empty protocol-conformance types are empty.
  var x5 : (_ : Any) -> ()
  var x6 : (_ : Any2) -> ()
  x5 = x6

  var x7 : (_ : protocol<P1, P3>) -> ()
  var x8 : (_ : protocol<P2>) -> ()
  x7 = x8 // expected-error{{invalid conversion from type '(_ : protocol<P2>) -> ()' to '(_ : protocol<P1, P3>) -> ()'}}
}

typealias Bogus = protocol<P1, Int> // expected-error{{non-protocol type 'Int' cannot be used within 'protocol<...>'}}
