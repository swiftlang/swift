// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Successful user-defined conversions
//===----------------------------------------------------------------------===//

struct X {
  func [conversion] __conversion(value : Int = 17) -> Y { }
}

struct Y {
  func [conversion] __conversion() -> String {}
}

func produce_X() -> X {}

func produce_X(x: Int) -> X {}
func produce_X(x : X) -> X {}

func accept_y(y : Y) {}

func test(x : X, y : Y) {
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
func acceptNSString(ns : NSString) {}
func acceptString(s : String) {}

func testStrings(s : String, ns : NSString) {
  acceptNSString(ns)
  acceptNSString(s)
  acceptString(s)
  acceptString(ns)
  acceptString("foo")
  acceptNSString("bar")
}

//===----------------------------------------------------------------------===//
// Errors in the declaration of conversion functions
//===----------------------------------------------------------------------===//
struct Z {
  static func [conversion] conv() -> Int {} // expected-error{{conversion function 'conv' is not an instance method}}

  func [conversion] conv(i : Int) -> Float {} // expected-error{{conversion function 'conv' has non-defaulted parameters}}
}

//===----------------------------------------------------------------------===//
// Errors in the semantic analysis of conversions
//===----------------------------------------------------------------------===//
struct Chain1 {
  func [conversion] __conversion() -> Chain2 {}
}

struct Chain2 {
  func [conversion] __conversion() -> Chain3 {}
}

struct Chain3 {
}

func getChain1() -> Chain1 {}

func test_chain(c1 : Chain1, c2 : Chain2, c3 : Chain3) {
  c2 = c1; // okay
  c3 = c1; // expected-error{{invalid conversion from type 'Chain1' to 'Chain3'}}
  var c3a : Chain2 = c1; // okay
  var c3b : Chain3 = c1; // expected-error{{invalid conversion from type 'Chain1' to 'Chain3'}} expected-note{{while converting 'var' initializer to declared type 'Chain3'}}
  c2 = getChain1(); // okay
  c3 = getChain1(); // expected-error{{invalid conversion from type 'Chain1' to 'Chain3'}}
}

func acceptYReference(y : [byref] Y) {}

func testByRef(x : X) {
  acceptYReference(x) // expected-error{{passing 'X' variable by reference as 'Y' variable}} expected-note{{while converting function argument to expected type}}
  acceptYReference(&x) // expected-error{{passing 'X' variable by reference as 'Y' variable}} expected-note{{while converting function argument to expected type}}
}
