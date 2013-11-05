// RUN: %swift -parse %s -verify

protocol Q { }

protocol P {
  def foo()
  def bar()
}

struct S1 : Q { // expected-note{{specify explicit conformance to protocol 'P'}}{{14-14=, P}}
  def foo() { }
  def bar() { }
}

struct S2 { }

extension S2 {  // expected-note{{specify explicit conformance to protocol 'P'}}{{13-13= : P}}
  def foo() { }
  def bar() { }
}

struct S3 {  // expected-note{{specify explicit conformance to protocol 'P'}}{{10-10= : P}}
  def foo() { }
}

extension S3 {
  def bar() { }
}

var p1 : P = S1() // expected-error{{type 'S1' does not explicitly conform to protocol 'P'}}
var p2 : P = S2() // expected-error{{type 'S2' does not explicitly conform to protocol 'P'}}
var p3 : P = S3() // expected-error{{type 'S3' does not explicitly conform to protocol 'P'}}
