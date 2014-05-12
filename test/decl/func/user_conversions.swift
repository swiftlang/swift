// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Successful user-defined conversions
//===----------------------------------------------------------------------===//

struct X {
  @conversion func __conversion(value: Int = 17) -> Y { }
}

struct Y {
  @conversion func __conversion() -> String {}
}

func produce_X() -> X {}

func produce_X(x: Int) -> X {}
func produce_X(x: X) -> X {}

func accept_y(y: Y) {}

func test(x: X, inout y: Y) {
  accept_y(x)
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
  @conversion func __conversion() -> String { }

  static func convertFromStringLiteral(s: String) -> OtherString {
    return s
  }
}

extension String {
  @conversion func __conversion() -> OtherString { }
}

func acceptOtherString(other: OtherString) {}
func acceptString(s: String) {}

func testStrings(s: String, other: OtherString) {
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
  @conversion static func conv() -> Int {} // expected-error{{conversion function 'conv' is not an instance method}}

  @conversion func conv(i: Int) -> Float {} // expected-error{{conversion function 'conv' has non-defaulted parameters}}
}

//===----------------------------------------------------------------------===//
// Errors in the semantic analysis of conversions
//===----------------------------------------------------------------------===//
struct Chain1 {
  @conversion func __conversion() -> Chain2 {}
}

struct Chain2 {
  @conversion func __conversion() -> Chain3 {}
}

struct Chain3 {
}

func getChain1() -> Chain1 {}

func test_chain(var c1: Chain1, inout c2: Chain2, inout c3: Chain3) {
  c2 = c1; // okay
  c3 = c1; // expected-error{{cannot convert the expression's type '()' to type 'Chain3'}}
  var c3a : Chain2 = c1; // okay
  var c3b : Chain3 = c1; // expected-error{{cannot convert the expression's type '@lvalue Chain1' to type 'Chain3'}}
  c2 = getChain1(); // okay
  c3 = getChain1(); // expected-error{{'Chain2' is not a subtype of 'Chain3'}}
}

func acceptYReference(inout y: Y) {} // expected-note {{in initialization of parameter 'y'}}

func testInOut(x: X) {
  var x2 = x
  acceptYReference(x2) // expected-error{{cannot convert the expression's type '()' to type 'inout Y'}}
  acceptYReference(&x2) // expected-error{{'X' is not identical to 'Y'}}
}
