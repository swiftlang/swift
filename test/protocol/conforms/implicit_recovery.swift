// RUN: %swift -parse %s -verify

protocol Q { }

protocol P {
  func foo()
  func bar()
}

struct S1 : Q { // expected-note{{introduce explicit conformance to protocol 'P'}}{{14-14=, P}}
  func foo() { }
  func bar() { }
}

struct S2 { }

extension S2 {  // expected-note{{introduce explicit conformance to protocol 'P'}}{{13-13= : P}}
  func foo() { }
  func bar() { }
}

struct S3 {  // expected-note{{introduce explicit conformance to protocol 'P'}}{{10-10= : P}}
  func foo() { }
}

extension S3 {
  func bar() { }
}

var p1 : P = S1() // expected-error{{type 'S1' does not explicitly conform to protocol 'P'}} expected-note{{while converting 'var' initializer to declared type 'P'}}
var p2 : P = S2() // expected-error{{type 'S2' does not explicitly conform to protocol 'P'}} expected-note{{while converting 'var' initializer to declared type 'P'}}
var p3 : P = S3() // expected-error{{type 'S3' does not explicitly conform to protocol 'P'}} expected-note{{while converting 'var' initializer to declared type 'P'}}
