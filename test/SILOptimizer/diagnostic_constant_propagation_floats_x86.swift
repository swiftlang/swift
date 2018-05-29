// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: CPU=i386 || CPU=x86_64
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations that are specific to x86 architectures,
// which support Float80.

import StdlibUnittest

func testFPToIntConversion() {
  let i64max: Float80 = 9.223372036854775807E18
  _blackHole(Int64(i64max))

  let i64overflow: Float80 = -9.223372036854775809E18
  _blackHole(Int64(i64overflow)) // expected-error {{invalid conversion: '-9.223372036854775809E18' overflows 'Int64'}}

  let j: Float80 = 9.223372036854775808E18
  _blackHole(Int64(j)) // expected-error {{invalid conversion: '9.223372036854775808E18' overflows 'Int64'}}

  let u64max: Float80 = 1.8446744073709551615E19
  _blackHole(UInt64(u64max))

  let uj: Float80 = 1.8446744073709551616E19
  _blackHole(UInt64(uj)) // expected-error {{invalid conversion: '1.8446744073709551616E19' overflows 'UInt64'}}

  // FIXME: False negative: overflows in the implicit coversion to 'Double'
  // halts constant folding and prevents detection of following errors.
  _blackHole(Int8(1E309))
  _blackHole(UInt8(-1E309))
  _blackHole(Int64(1E309))
  _blackHole(UInt64(-1E309))
}
