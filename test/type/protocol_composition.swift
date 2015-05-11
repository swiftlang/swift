// RUN: %target-parse-verify-swift

func canonical_empty_protocol() -> protocol<> {
  return 1
}

protocol P1 {
  func p1()
  func f(_: Int) -> Int
}

protocol P2 : P1 {
  func p2()
}

protocol P3 {
  func p3()
}

protocol P4 : P3 {
  func p4()
  func f(_: Double) -> Double
}

typealias Any = protocol<>
typealias Any2 = protocol< >

// Okay to inherit a typealias for a protocol<> type.
protocol P5 : Any { }

extension Int : P5 { }

typealias Bogus = protocol<P1, Int> // expected-error{{non-protocol type 'Int' cannot be used within 'protocol<...>'}}

func testEquality() {
  // Remove duplicates from protocol-conformance types.
  let x1 : (_ : protocol<P2, P4>) -> ()
  let x2 : (_ : protocol<P3, P4, P2, P1>) -> ()
  x1 = x2
  _ = x1

  // Singleton protocol-conformance types, after duplication, are the same as
  // simply naming the protocol type.
  let x3 : (_ : protocol<P2, P1>) -> ()
  let x4 : (_ : P2) -> ()
  x3 = x4
  _ = x3
  
  // Empty protocol-conformance types are empty.
  let x5 : (_ : Any) -> ()
  let x6 : (_ : Any2) -> ()
  x5 = x6
  _ = x5

  let x7 : (_ : protocol<P1, P3>) -> ()
  let x8 : (_ : protocol<P2>) -> ()
  x7 = x8 // expected-error{{cannot assign a value of type '(P2) -> ()' to a value of type '(protocol<P1, P3>) -> ()'}}
  _ = x7
}

// Name lookup into protocol-conformance types
func testLookup() {
  let x1 : protocol<P2, P1, P4>
  x1.p1()
  x1.p2()
  x1.p3()
  x1.p4()
  var _ : Int = x1.f(1)
  var _ : Double = x1.f(1.0)
}

protocol REPLPrintable {
  func replPrint()
}

protocol SuperREPLPrintable : REPLPrintable {
  func superReplPrint()
}

protocol FooProtocol {
  func format(kind: UnicodeScalar, layout: String) -> String
}

struct SuperPrint : REPLPrintable, FooProtocol, SuperREPLPrintable {
  func replPrint() {}
  func superReplPrint() {}
  func format(kind: UnicodeScalar, layout: String) -> String {}
}

struct Struct1 {}
extension Struct1 : REPLPrintable, FooProtocol {
  func replPrint() {}
  func format(kind: UnicodeScalar, layout: String) -> String {}
}

func accept_manyPrintable(_: protocol<REPLPrintable, FooProtocol>) {}

func return_superPrintable() -> protocol<FooProtocol, SuperREPLPrintable> {}

func testConversion() {
  // Conversions for literals.
  var x : protocol<REPLPrintable, FooProtocol> = Struct1()
  accept_manyPrintable(Struct1())

  // Conversions for nominal types that conform to a number of protocols.
  let sp : SuperPrint
  x = sp
  accept_manyPrintable(sp)

  // Conversions among existential types.
  var x2 : protocol<SuperREPLPrintable, FooProtocol>
  x2 = x // expected-error{{cannot assign a value of type 'protocol<FooProtocol, REPLPrintable>' to a value of type 'protocol<FooProtocol, SuperREPLPrintable>'}}
  x = x2

  // Subtyping
  var _ : () -> protocol<FooProtocol, SuperREPLPrintable> = return_superPrintable

  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  var _ : () -> protocol<FooProtocol, REPLPrintable> = { return_superPrintable($0) }
}

// Test the parser's splitting of >= into > and =.
var x : protocol<P5>=17

