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

func expectSameType<T>(_: T.Type, _: T.Type) {}

func test_truncatingBitPatternAPIIsStableAcrossPlatforms() {
  // Audit and update this test when adding new integer types.
  expectSameType(Int64.self, IntMax.self)
  expectSameType(UInt64.self, UIntMax.self)

  UInt8(truncatingBitPattern: UInt(0))
  UInt16(truncatingBitPattern: UInt(0))
  UInt32(truncatingBitPattern: UInt(0))
  UInt64(truncatingBitPattern: UInt(0)) // expected-error {{extraneous argument label 'truncatingBitPattern:' in call}}
  UInt(truncatingBitPattern: UInt(0))   // expected-error {{cannot invoke 'init' with an argument list of type}}

  Int8(truncatingBitPattern: UInt(0))
  Int16(truncatingBitPattern: UInt(0))
  Int32(truncatingBitPattern: UInt(0))
  Int64(truncatingBitPattern: UInt(0)) // expected-error {{extraneous argument label 'truncatingBitPattern:' in call}}
  Int(truncatingBitPattern: UInt(0))   // expected-error {{cannot invoke 'init' with an argument list of type}}

  UInt8(truncatingBitPattern: Int(0))
  UInt16(truncatingBitPattern: Int(0))
  UInt32(truncatingBitPattern: Int(0))
  UInt64(truncatingBitPattern: Int(0)) // expected-error {{extraneous argument label 'truncatingBitPattern:' in call}}
  UInt(truncatingBitPattern: Int(0))   // expected-error {{cannot invoke 'init' with an argument list of type}}

  Int8(truncatingBitPattern: Int(0))
  Int16(truncatingBitPattern: Int(0))
  Int32(truncatingBitPattern: Int(0))
  Int64(truncatingBitPattern: Int(0)) // expected-error {{extraneous argument label 'truncatingBitPattern:' in call}}
  Int(truncatingBitPattern: Int(0))   // expected-error {{cannot invoke 'init' with an argument list of type}}

  UInt(truncatingBitPattern: UInt8(0))  // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: UInt16(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: UInt32(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: UInt64(0))

  Int(truncatingBitPattern: UInt8(0))  // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: UInt16(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: UInt32(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: UInt64(0))

  UInt(truncatingBitPattern: Int8(0))  // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: Int16(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: Int32(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  UInt(truncatingBitPattern: Int64(0))

  Int(truncatingBitPattern: Int8(0))  // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: Int16(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: Int32(0)) // expected-error {{cannot invoke 'init' with an argument list of type}}
  Int(truncatingBitPattern: Int64(0))
}

func testOps<T : IntegerArithmeticType>(x: T, y: T) -> T {
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
