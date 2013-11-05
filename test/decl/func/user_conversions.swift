// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Successful user-defined conversions
//===----------------------------------------------------------------------===//

struct X {
  @conversion def __conversion(value: Int = 17) -> Y { }
}

struct Y {
  @conversion def __conversion() -> String {}
}

def produce_X() -> X {}

def produce_X(x: Int) -> X {}
def produce_X(x: X) -> X {}

def accept_y(y: Y) {}

def test(x: X, y: Y) {
  accept_y(x);
  accept_y(y)
  y = x
  var y2 : Y = x
  var y3 : Y = (x)
  var y4 : Y = produce_X();
  var y5 : Y = produce_X(1);
  var s : String = "ab\(y)yz";
}

//===----------------------------------------------------------------------===//
// Test string conversions
//===----------------------------------------------------------------------===//
struct OtherString {
  @conversion def __conversion() -> String { }

  static def convertFromStringLiteral(s: String) -> OtherString {
    return s
  }
}

extension String {
  @conversion def __conversion() -> OtherString { }
}

def acceptOtherString(other: OtherString) {}
def acceptString(s: String) {}

def testStrings(s: String, other: OtherString) {
  acceptOtherString(other)
  acceptOtherString(s)
  acceptString(s)
  acceptString(other)
  acceptString("foo")
  acceptOtherString("bar")
}

//===----------------------------------------------------------------------===//
// Errors in the declaration of conversion functions
//===----------------------------------------------------------------------===//
struct Z {
  @conversion static def conv() -> Int {} // expected-error{{conversion function 'conv' is not an instance method}}

  @conversion def conv(i: Int) -> Float {} // expected-error{{conversion function 'conv' has non-defaulted parameters}}
}

//===----------------------------------------------------------------------===//
// Errors in the semantic analysis of conversions
//===----------------------------------------------------------------------===//
struct Chain1 {
  @conversion def __conversion() -> Chain2 {}
}

struct Chain2 {
  @conversion def __conversion() -> Chain3 {}
}

struct Chain3 {
}

def getChain1() -> Chain1 {}

def test_chain(c1: Chain1, c2: Chain2, c3: Chain3) {
  c2 = c1; // okay
  c3 = c1; // expected-error{{'Chain2' is not a subtype of 'Chain3'}}
  var c3a : Chain2 = c1; // okay
  var c3b : Chain3 = c1; // expected-error{{'Chain2' is not a subtype of 'Chain3'}}
  c2 = getChain1(); // okay
  c3 = getChain1(); // expected-error{{'Chain2' is not a subtype of 'Chain3'}}
}

def acceptYReference(y : @inout Y) {} // expected-note{{in initialization of parameter 'y'}}

def testInOut(x: X) {
  acceptYReference(x) // expected-error{{expression does not type-check}}
  acceptYReference(&x) // expected-error{{'X' is not identical to 'Y'}}
}
