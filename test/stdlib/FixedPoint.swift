// RUN: %swift -verify -parse %s

func testUnaryMinusInUnsigned() {
  var a: UInt8 = -(1) // expected-error {{expression does not type-check}}
  var b: UInt16 = -(1) // expected-error {{expression does not type-check}}
  var c: UInt32 = -(1) // expected-error {{expression does not type-check}}
  var d: UInt64 = -(1) // expected-error {{expression does not type-check}}
}
