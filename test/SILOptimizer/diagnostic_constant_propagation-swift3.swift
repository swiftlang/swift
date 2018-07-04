// RUN: %target-swift-frontend -swift-version 3 -emit-sil -primary-file %s -o /dev/null -verify
//
// These are tests for diagnostics produced by constant propagation pass.
// These tests are specific to swift 3.

func testArithmeticOverflowSwift3() {
  // FIXME: Should expect an integer overflow error but none happens now
  // (see <rdar://problem/39120081>)
  var _ = Int8(126) + (1 + 1) // expected-warning {{'+' is deprecated: Please use explicit type conversions or Strideable methods for mixed-type arithmetics.}}
}
