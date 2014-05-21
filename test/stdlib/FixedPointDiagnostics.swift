// RUN: %swift -verify -parse %s

func testUnaryMinusInUnsigned() {
  var a: UInt8 = -(1) // expected-error {{could not find an overload for '-' that accepts the supplied arguments}}
  var b: UInt16 = -(1) // expected-error {{could not find an overload for '-' that accepts the supplied arguments}}
  var c: UInt32 = -(1) // expected-error {{could not find an overload for '-' that accepts the supplied arguments}}
  var d: UInt64 = -(1) // expected-error {{could not find an overload for '-' that accepts the supplied arguments}}
}

// Int and UInt are not identical to any fixed-size integer type
var i   :  Int   = 0
var w   :  Word  = i
var i64 :  Int64 = i // expected-error {{'Int' is not convertible to 'Int64'}}
var i32 :  Int32 = i // expected-error {{'Int' is not convertible to 'Int32'}}
var i16 :  Int16 = i // expected-error {{'Int' is not convertible to 'Int16'}}
var i8  :  Int8  = i // expected-error {{'Int' is not convertible to 'Int8'}}

var u   : UInt   = 0
var uw  : UWord  = u
var u64 : UInt64 = u // expected-error {{'UInt' is not convertible to 'UInt64'}}
var u32 : UInt32 = u // expected-error {{'UInt' is not convertible to 'UInt32'}}
var u16 : UInt16 = u // expected-error {{'UInt' is not convertible to 'UInt16'}}
var u8  : UInt8  = u // expected-error {{'UInt' is not convertible to 'UInt8'}}

func testOps<T : IntegerArithmetic>(x: T, y: T) -> T {
  let a = x + y
  let s = x - y
  let m = x * y
  let d = x / y
  let r = x % y
  return a + s + m + d + r
}

let   s_ops:    Int = testOps(5, 2)
let   u_ops:   UInt = testOps(5, 2)
let  s8_ops:   Int8 = testOps(5, 2)
let  u8_ops:  UInt8 = testOps(5, 2)
let s16_ops:  Int16 = testOps(5, 2)
let u16_ops: UInt16 = testOps(5, 2)
let s32_ops:  Int32 = testOps(5, 2)
let u32_ops: UInt32 = testOps(5, 2)
let s64_ops:  Int64 = testOps(5, 2)
let u64_ops: UInt64 = testOps(5, 2)
