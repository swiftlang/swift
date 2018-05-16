// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: !(CPU=i386 || CPU=x86_64)
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations that are specific to non-x86 architectures,
// which do not support Float80.

import StdlibUnittest

func testFPToIntConversion() {
  _blackHole(Int8(1E309)) // expected-error {{invalid conversion: '1E309' overflows 'Int8'}}
  _blackHole(UInt8(-1E309)) // expected-error {{negative literal '-1E309' cannot be converted to 'UInt8'}}

  _blackHole(Int64(1E309)) // expected-error {{invalid conversion: '1E309' overflows 'Int64'}}
  _blackHole(UInt64(-1E309)) // expected-error {{negative literal '-1E309' cannot be converted to 'UInt64'}}
}
