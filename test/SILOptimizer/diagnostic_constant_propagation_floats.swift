// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations

import StdlibUnittest

func testFloatConvertOverflow() {
  let f1: Float = 1E38
  _blackHole(f1)
  let f2: Float = 1E39 // expected-warning {{floating-point literal '9.99999999999999999993E+38' overflows 32-bit floating-point type}}
  _blackHole(f2)
  let f3: Float = 123456789101234567891234567123456123451234123121.0 // expected-warning {{floating-point literal '1.23456789101234567893E+47' overflows 32-bit floating-point type}}
  _blackHole(f3)
  let f4: Float = 0.123456789101234567891234567123456123451234123121
  _blackHole(f4)

  let f5: Float32 = 3.4028236e+38 // expected-warning {{floating-point literal '3.40282360000000000005E+38' overflows 32-bit floating-point type}}
  _blackHole(f5)
  let f6: Float32 = -3.4028236e+38 // expected-warning {{floating-point literal '-3.40282360000000000005E+38' overflows 32-bit floating-point type}}
  _blackHole(f6)
  let f7: Float32 = 1.0e999 // expected-warning {{floating-point literal '1.00000000000000000003E+999' overflows 32-bit floating-point type}}
  _blackHole(f7)
  let f8: Float32 = -1.0e999 // expected-warning {{floating-point literal '-1.00000000000000000003E+999' overflows 32-bit floating-point type}}
  _blackHole(f8)

  _blackHole(Float(1E38))
  _blackHole(Float(1E39)) // expected-warning {{floating-point literal '9.9999999999999994E+38' overflows 32-bit floating-point type}}
  _blackHole(Float(123456789101234567891234567123456123451234123121.0)) // expected-warning {{floating-point literal '1.2345678910123456E+47' overflows 32-bit floating-point type}}
  _blackHole(Float(100000000000000000000000000000000000000000000000.0)) // expected-warning {{floating-point literal '1.0E+47' overflows 32-bit floating-point type}}

  let d1: Double = 1E308
  _blackHole(d1)
  let d2: Double = 123456789101234567891234567123456123451234123121.0
  _blackHole(d2)
  let d3: Double = 0.123456789101234567891234567123456123451234123121
  _blackHole(d3)
  let d4: Double = 1E309 // expected-warning {{floating-point literal '9.99999999999999999986E+308' overflows 64-bit floating-point type}}
  _blackHole(d4)

  let d5: Float64 = 1.0e999 // expected-warning {{floating-point literal '1.00000000000000000003E+999' overflows 64-bit floating-point type}}
  _blackHole(d5)
  let d6: Float64 = -1.0e999 // expected-warning {{floating-point literal '-1.00000000000000000003E+999' overflows 64-bit floating-point type}}
  _blackHole(d6)
  let d7: Float64 = 1.7976931348623159e+308 // expected-warning {{floating-point literal '1.79769313486231590003E+308' overflows 64-bit floating-point type}}
  _blackHole(d7)
  let d8: Float64 = -1.7976931348623159e+308 // expected-warning {{floating-point literal '-1.79769313486231590003E+308' overflows 64-bit floating-point type}}
  _blackHole(d8)

  // Double.init() is getting optimized away when using `let _`.
  // Why is this behavior specific to Double Inits?
  _blackHole(Double(1E308))
  _blackHole(Double(1E309)) // expected-warning {{floating-point literal '9.99999999999999999986E+308' overflows 64-bit floating-point type}}

  // In the following cases, the input literal is bigger than what can be
  // represented in Swift and hence is represented as Inf

  let e1: Float80 = 1E6000 // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
  _blackHole(Float80(1E6000)) // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
  let e2: Float80 = 1.18973149535723176515e+4932 // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
  let e3: Float80 = -1.18973149535723176515e+4932 // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
  _blackHole(e1)
  _blackHole(e2)
  _blackHole(e3)

  _blackHole(Float(1E6000)) // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
  _blackHole(Double(1E6000)) // expected-warning {{input literal is outside the range of largest representable floating-point literal}}
}

func testFloatToIntConversion() {
  _blackHole(Int8(-1.28E2))
  _blackHole(Int8(-1.27E2))
  _blackHole(Int8(-0))
  _blackHole(Int8(3.33333))
  _blackHole(Int8(-2E2)) // expected-error {{floating-point literal '-200' overflows 8-bit signed integer type}}

  _blackHole(UInt8(2E2))
  _blackHole(UInt8(3E2)) // expected-error {{floating-point literal '300' overflows 8-bit unsigned integer type}}
  _blackHole(UInt8(-0E0))
  _blackHole(UInt8(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int16(3.2767E4))
  _blackHole(Int16(3.2768E4)) // expected-error {{floating-point literal '32768' overflows 16-bit signed integer type}}
  _blackHole(Int16(-4E4)) // expected-error {{floating-point literal '-4.0E+4' overflows 16-bit signed integer type}}

  _blackHole(UInt16(6.5535E4))
  _blackHole(UInt16(6.5536E4)) // expected-error {{floating-point literal '65536' overflows 16-bit unsigned integer type}}0
  _blackHole(UInt16(7E4)) // expected-error {{floating-point literal '7.0E+4' overflows 16-bit unsigned integer type}}
  _blackHole(UInt16(-0E0))
  _blackHole(UInt16(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int32(-2.147483648E9))
  _blackHole(Int32(-2.147483649E9)) // expected-error {{floating-point literal '-2147483649' overflows 32-bit signed integer type}}
  _blackHole(Int32(3E9)) // expected-error {{floating-point literal '3.0E+9' overflows 32-bit signed integer type}}

  _blackHole(UInt32(4.294967295E9))
  _blackHole(UInt32(4.294967296E9)) // expected-error {{floating-point literal '4294967296' overflows 32-bit unsigned integer type}}
  _blackHole(UInt32(5E9)) // expected-error {{floating-point literal '5.0E+9' overflows 32-bit unsigned integer type}}
  _blackHole(UInt32(-0E0))
  _blackHole(UInt32(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int64(9.223372036854775E18))

  // note in the following case, double looses precision on Int64.max and
  // results in an overflow, whereas Float80 can accurately represent the number
  _blackHole(Int64(9.223372036854775807E18)) // expected-error {{floating-point literal '9.2233720368547758E+18' overflows 64-bit signed integer type}}
  let i64max: Float80 = 9.223372036854775807E18
  _blackHole(Int64(i64max))

  _blackHole(Int64(9.223372036854775808E18)) // expected-error {{floating-point literal '9.2233720368547758E+18' overflows 64-bit signed integer type}}
  let j: Float80 = 9.223372036854775808E18
  _blackHole(Int64(j)) // expected-error {{floating-point literal '9223372036854775808' overflows 64-bit signed integer type}}
  _blackHole(Int64(1E19)) // expected-error {{floating-point literal '1.0E+19' overflows 64-bit signed integer type}}

  _blackHole(UInt64(1.844674407370955E19))
  _blackHole(UInt64(1.8446744073709551615E19)) // expected-error {{floating-point literal '1.8446744073709552E+19' overflows 64-bit unsigned integer type}}
  let u64max: Float80 = 1.8446744073709551615E19
  _blackHole(UInt64(u64max))

  let uj: Float80 = 1.8446744073709551616E19
  _blackHole(UInt64(uj)) // expected-error {{floating-point literal '18446744073709551616' overflows 64-bit unsigned integer type}}

  _blackHole(UInt64(2E19)) // expected-error {{floating-point literal '2.0E+19' overflows 64-bit unsigned integer type}}
  _blackHole(UInt64(-0E0))
  _blackHole(UInt64(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}
}

func testFloatConvertUnderflow() {
  let f1: Float = 1E-37
  _blackHole(f1)
  let f2: Float = 1E-39 // expected-warning {{floating-point literal '1.00000000000000000003E-39' is subnormal and is not precisely representable in a 32-bit floating-point type}}
  _blackHole(f2)
  let f3: Float = 0x0.800000p-126 // a precisely represented subnormal number. The underflow flag will not be set here.
  _blackHole(f3)
  let f4: Float = 0x0.000002p-126 // smallest Float subnormal number
  _blackHole(f4)
  let f5: Float = 1E-45 // expected-warning {{floating-point literal '9.99999999999999999981E-46' is subnormal and is not precisely representable in a 32-bit floating-point type}}
  _blackHole(f5)

  let f6: Float = 0x1.000000p-126 // Smallest normal number, which is
                                  // approximately 1.1754943508
  _blackHole(f6)
  let f7: Float = 1.1754943e-38 // This number is smaller than the smallest
    // normal number and is imprecise. Nonetheless, underflow wont be detected
    // here as underflow detection happens after rounding. This is due to
    // LLVM's APFloat behavior. We do not warn in these cases.
  _blackHole(f7)

  _blackHole(Float(1E-37))
  _blackHole(Float(1E-39)) // expected-warning {{floating-point literal '9.9999999999999992E-40' is subnormal and is not precisely representable in a 32-bit floating-point type}}
  _blackHole(Float(0x0.000002p-126))
  _blackHole(Float(1E-45)) // expected-warning {{floating-point literal '9.9999999999999998E-46' is subnormal and is not precisely representable in a 32-bit floating-point type}}

  let d1: Double = 1E-307
  _blackHole(d1)
  let d2: Double = 1E-309 // expected-warning {{floating-point literal '9.99999999999999999974E-310' is subnormal and is not precisely representable in a 64-bit floating-point type}}
  _blackHole(d2)
  let d3: Double = 0x0.0000000000004p-1024 // smallest Double subnormal number
  _blackHole(d3)
  let d4: Double = 5e-324 // expected-warning {{floating-point literal '4.99999999999999999976E-324' is subnormal and is not precisely representable in a 64-bit floating-point type}}
  _blackHole(d4)

  _blackHole(Double(1E-307))
  _blackHole(Double(1E-309)) // expected-warning {{floating-point literal '9.99999999999999999974E-310' is subnormal and is not precisely representable in a 64-bit floating-point type}}
  _blackHole(Double(0x0.0000000000004p-1024))
  _blackHole(Double(5e-324)) // expected-warning {{floating-point literal '4.99999999999999999976E-324' is subnormal and is not precisely representable in a 64-bit floating-point type}})
}

func testFloatArithmetic() {
  // Ignore Inf and Nan during arithmetic operations.
  // This may become a warning in the future.
  let infV: Float = 3.0 / 0.0
  _blackHole(infV)

  let a: Float = 1E38
  let b: Float = 10.0
  _blackHole(a * b)
}
