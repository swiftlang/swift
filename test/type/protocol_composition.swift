// RUN: %swift %s -verify

def canonical_empty_protocol() -> protocol<> {
  return 1
}

protocol P1 { 
  def p1()
  def f(_: Int) -> Int
}

protocol P2 : P1 { 
  def p2()
}

protocol P3 { 
  def p3()
}

protocol P4 : P3 { 
  def p4()
  def f(_: Double) -> Double
}

typealias Any = protocol<>
typealias Any2 = protocol< >

// Okay to inherit a typealias for a protocol<> type.
protocol P5 : Any { }

extension Int : P5 { }

typealias Bogus = protocol<P1, Int> // expected-error{{non-protocol type 'Int' cannot be used within 'protocol<...>'}}

def testEquality() {
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
  x7 = x8 // expected-error{{does not type-check}}
}

// Name lookup into protocol-conformance types
def testLookup() {
  var x1 : protocol<P2, P1, P4>
  x1.p1()
  x1.p2()
  x1.p3()
  x1.p4()
  var i1 : Int = x1.f(1)
  var d1 : Double = x1.f(1.0)
}

protocol REPLPrintable {
  def replPrint()
}

protocol SuperREPLPrintable : REPLPrintable {
  def superReplPrint()
}

struct SuperPrint : REPLPrintable, FormattedPrintable, SuperREPLPrintable {
  def replPrint() {}
  def superReplPrint() {}
  def format(kind: Char, layout: String) -> String {}
}

extension Int : REPLPrintable, FormattedPrintable { }

def accept_manyPrintable(_: protocol<REPLPrintable, FormattedPrintable>) {}

def return_superPrintable() -> protocol<FormattedPrintable, SuperREPLPrintable> {}

def testConversion() {
  // Conversions for literals.
  var x : protocol<REPLPrintable, FormattedPrintable> = 1
  accept_manyPrintable(1)

  // Conversions for nominal types that conform to a number of protocols.
  var sp : SuperPrint
  x = sp
  accept_manyPrintable(sp)
  
  // Conversions among existential types.
  var x2 : protocol<SuperREPLPrintable, FormattedPrintable>
  x2 = x // expected-error{{'protocol<REPLPrintable, FormattedPrintable>' does not conform to protocol 'SuperREPLPrintable'}}
  x = x2

  // Subtyping
  var f1 : () -> protocol<FormattedPrintable, SuperREPLPrintable> = return_superPrintable
  var f2 : () -> protocol<FormattedPrintable, REPLPrintable> = return_superPrintable
}

// Test the parser's splitting of >= into > and =.
var x : protocol<P5>=17
