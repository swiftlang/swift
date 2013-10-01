// RUN: %swift %s -o /dev/null -verify

// These are tests for diagnostics produced by constant propagation pass.

func testArithmeticOverflow() {
  var xu8 : UInt8 = 250
  var yu8 : UInt8 = 250
  var zu8 = xu8 + yu8 // expected-error {{arithmetic operation results in an overflow}}
  var xpyu8 : UInt8   = 250 + 250 // expected-error {{arithmetic operation results in an overflow}}
  var xpyi8 : Int8    = 126 + 126 // expected-error {{arithmetic operation results in an overflow}}
  var xmyu16 : UInt16 = 65000 * 2 // expected-error {{arithmetic operation results in an overflow}}
  var xmyi16 : Int16  = 32000 * 2 // expected-error {{arithmetic operation results in an overflow}}
  var xmyu32 : UInt32 = 4294967295 * 30 // expected-error {{arithmetic operation results in an overflow}}
  var xpyi32 : Int32 = 2147483647 + 30 // expected-error {{arithmetic operation results in an overflow}}
  var xpyu64 : UInt64 = 9223372036854775807 * 30 // expected-error {{arithmetic operation results in an overflow}}
  var xpyi64 : Int64 = 9223372036854775807 + 1  // expected-error {{arithmetic operation results in an overflow}}

  // FIXME: Error message should say "underflow"
  var x : UInt8 = 230 - 240 // expected-error {{arithmetic operation results in an overflow}}

}
