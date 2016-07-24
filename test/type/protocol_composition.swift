// RUN: %target-parse-verify-swift

func canonical_empty_protocol() -> Any {
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

typealias Any1 = protocol<> // expected-warning {{'protocol<>' syntax is deprecated; use 'Any' instead}}
typealias Any2 = protocol< > // expected-warning {{'protocol<>' syntax is deprecated; use 'Any' instead}}

// Okay to inherit a typealias for Any type.
protocol P5 : Any { }
protocol P6 : protocol<> { } // expected-warning {{'protocol<>' syntax is deprecated; use 'Any' instead}}
                             // expected-error@-1 {{protocol composition is neither allowed nor needed here}}
typealias P7 = Any & Any1

extension Int : P5 { }

typealias Bogus = P1 & Int // expected-error{{non-protocol type 'Int' cannot be used within a protocol composition}}

func testEquality() {
  // Remove duplicates from protocol-conformance types.
  let x1 : (_ : P2 & P4) -> ()
  let x2 : (_ : P3 & P4 & P2 & P1) -> ()
  x1 = x2
  _ = x1

  // Singleton protocol-conformance types, after duplication, are the same as
  // simply naming the protocol type.
  let x3 : (_ : P2 & P1) -> ()
  let x4 : (_ : P2) -> ()
  x3 = x4
  _ = x3
  
  // Empty protocol-conformance types are empty.
  let x5 : (_ : Any) -> ()
  let x6 : (_ : Any2) -> ()
  x5 = x6
  _ = x5

  let x7 : (_ : P1 & P3) -> ()
  let x8 : (_ : P2) -> ()
  x7 = x8 // expected-error{{cannot assign value of type '(P2) -> ()' to type '(P1 & P3) -> ()'}}
  _ = x7
}

// Name lookup into protocol-conformance types
func testLookup() {
  let x1 : P2 & P1 & P4
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
  func format(_ kind: UnicodeScalar, layout: String) -> String
}

struct SuperPrint : REPLPrintable, FooProtocol, SuperREPLPrintable {
  func replPrint() {}
  func superReplPrint() {}
  func format(_ kind: UnicodeScalar, layout: String) -> String {}
}

struct Struct1 {}
extension Struct1 : REPLPrintable, FooProtocol {
  func replPrint() {}
  func format(_ kind: UnicodeScalar, layout: String) -> String {}
}

func accept_manyPrintable(_: REPLPrintable & FooProtocol) {}

func return_superPrintable() -> FooProtocol & SuperREPLPrintable {}

func testConversion() {
  // Conversions for literals.
  var x : REPLPrintable & FooProtocol = Struct1()
  accept_manyPrintable(Struct1())

  // Conversions for nominal types that conform to a number of protocols.
  let sp : SuperPrint
  x = sp
  accept_manyPrintable(sp)

  // Conversions among existential types.
  var x2 : protocol<SuperREPLPrintable, FooProtocol> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{12-53=SuperREPLPrintable & FooProtocol}}
  x2 = x // expected-error{{value of type 'FooProtocol & REPLPrintable' does not conform to 'FooProtocol & SuperREPLPrintable' in assignment}}
  x = x2

  // Subtyping
  var _ : () -> FooProtocol & SuperREPLPrintable = return_superPrintable

  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  var _ : () -> protocol<FooProtocol, REPLPrintable> = { return_superPrintable() } // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{17-53=FooProtocol & REPLPrintable}}
}

// Test the parser's splitting of >= into > and =.
var x : protocol<P5>= 17 // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}} {{9-22=P5}}

typealias A = protocol<> // expected-warning {{'protocol<>' syntax is deprecated; use 'Any' instead}} {{15-25=Any}}
typealias B1 = protocol<P1,P2> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{16-31=P1 & P2}}
typealias B2 = protocol<P1, P2> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{16-32=P1 & P2}}
typealias B3 = protocol<P1 ,P2> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{16-32=P1 & P2}}
typealias B4 = protocol<P1 , P2> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}} {{16-33=P1 & P2}}
typealias C1 = protocol<Any, P1> // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}} {{16-33=P1}}
typealias C2 = protocol<P1, Any> // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}} {{16-33=P1}}
typealias D = protocol<P1> // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}} {{15-27=P1}}
