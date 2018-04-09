// RUN: %target-swift-frontend -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify
//
// These are tests for diagnostics produced by constant propagation pass.
// These are specific to Swift 4.

func testArithmeticOverflowSwift4() {
  var _ = Int8(126) + (1 + 1)  //  FIXME: Should expect an integer overflow
    // error but none happens now (see <rdar://problem/39120081>)
}
