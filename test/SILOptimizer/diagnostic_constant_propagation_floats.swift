// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations

import StdlibUnittest

func testFPToIntConversion() {
  _blackHole(Int8(-1.28E2))
  _blackHole(Int8(-128.5)) // the result is -128 and is not an overflow
  _blackHole(Int8(1.27E2))
  _blackHole(Int8(-0))
  _blackHole(Int8(3.33333))
  _blackHole(Int8(-2E2)) // expected-error {{invalid conversion: '-2E2' overflows 'Int8'}}

  _blackHole(UInt8(2E2))
  _blackHole(UInt8(3E2)) // expected-error {{invalid conversion: '3E2' overflows 'UInt8'}}
  _blackHole(UInt8(-0E0))
  _blackHole(UInt8(-2E2)) // expected-error {{negative literal '-2E2' cannot be converted to 'UInt8'}}

  // FIXME: False negative: overflows in the implicit coversion to 'Double'
  // halts constant folding and prevents detection of following errors.
  _blackHole(Int8(1E309))
  _blackHole(UInt8(-1E309))

  _blackHole(Int8(1E6000)) // expected-error {{invalid conversion: '1E6000' overflows 'Int8'}}
  _blackHole(UInt8(1E6000)) // expected-error {{invalid conversion: '1E6000' overflows 'UInt8'}}

  _blackHole(Int16(3.2767E4))
  _blackHole(Int16(3.2768E4)) // expected-error {{invalid conversion: '3.2768E4' overflows 'Int16'}}
  _blackHole(Int16(-4E4)) // expected-error {{invalid conversion: '-4E4' overflows 'Int16'}}

  _blackHole(UInt16(6.5535E4))
  _blackHole(UInt16(6.5536E4)) // expected-error {{invalid conversion: '6.5536E4' overflows 'UInt16'}}
  _blackHole(UInt16(7E4)) // expected-error {{invalid conversion: '7E4' overflows 'UInt16'}}
  _blackHole(UInt16(-0E0))
  _blackHole(UInt16(-2E2)) // expected-error {{negative literal '-2E2' cannot be converted to 'UInt16'}}

  _blackHole(Int32(-2.147483648E9))
  _blackHole(Int32(-2.147483649E9)) // expected-error {{invalid conversion: '-2.147483649E9' overflows 'Int32'}}
  _blackHole(Int32(3E9)) // expected-error {{invalid conversion: '3E9' overflows 'Int32'}}

  _blackHole(UInt32(4.294967295E9))
  _blackHole(UInt32(4.294967296E9)) // expected-error {{invalid conversion: '4.294967296E9' overflows 'UInt32'}}
  _blackHole(UInt32(5E9)) // expected-error {{invalid conversion: '5E9' overflows 'UInt32'}}
  _blackHole(UInt32(-0E0))
  _blackHole(UInt32(-2E2)) // expected-error {{negative literal '-2E2' cannot be converted to 'UInt32'}}

  _blackHole(Int64(9.223372036854775E18))

  // A case where implicit conversion of the float literal to 'Double'
  // results in an unintuitive overflow during conversion to 'Int64''
  _blackHole(Int64(9.223372036854775807E18)) // expected-error {{invalid conversion: '9.223372036854775807E18' overflows 'Int64'}}
  let i64max: Float80 = 9.223372036854775807E18
  _blackHole(Int64(i64max))

  // A case where implicit conversion of the float literal to 'Double'
  // elides an overflow that one would expect during conversion to 'Int64'.
  _blackHole(Int64(-9.223372036854775809E18))
  let i64overflow: Float80 = -9.223372036854775809E18
  _blackHole(Int64(i64overflow)) // expected-error {{invalid conversion: '-9.223372036854775809E18' overflows 'Int64'}}

  // Cases of definite overflow.
  _blackHole(Int64(9.223372036854775808E18)) // expected-error {{invalid conversion: '9.223372036854775808E18' overflows 'Int64'}}
  let j: Float80 = 9.223372036854775808E18
  _blackHole(Int64(j)) // expected-error {{invalid conversion: '9.223372036854775808E18' overflows 'Int64'}}
  _blackHole(Int64(1E19)) // expected-error {{invalid conversion: '1E19' overflows 'Int64'}}

  // A case where implicit conversion of the float literal to 'Double'
  // results in an unintuitive overflow during conversion to 'UInt64''.
  _blackHole(UInt64(1.844674407370955E19))
  _blackHole(UInt64(1.8446744073709551615E19)) // expected-error {{invalid conversion: '1.8446744073709551615E19' overflows 'UInt64'}}
  let u64max: Float80 = 1.8446744073709551615E19
  _blackHole(UInt64(u64max))

  let uj: Float80 = 1.8446744073709551616E19
  _blackHole(UInt64(uj)) // expected-error {{invalid conversion: '1.8446744073709551616E19' overflows 'UInt64'}}

  _blackHole(UInt64(2E19)) // expected-error {{invalid conversion: '2E19' overflows 'UInt64'}}
  _blackHole(UInt64(-0E0))
  _blackHole(UInt64(-2E2)) // expected-error {{negative literal '-2E2' cannot be converted to 'UInt64'}}

  // FIXME: False negative: overflows in the implicit coversion to 'Double'
  // halts constant folding and prevents detection of following errors.
  _blackHole(Int64(1E309))
  _blackHole(UInt64(-1E309))

  _blackHole(Int64(1E6000)) // expected-error {{invalid conversion: '1E6000' overflows 'Int64'}}
  _blackHole(UInt64(1E6000)) // expected-error {{invalid conversion: '1E6000' overflows 'UInt64'}}
}
