// RUN: %target-swift-frontend -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify
// RUN: %target-swift-frontend -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify -enable-ownership-stripping-after-serialization
//
// These are tests for diagnostics produced by constant propagation pass.
// These are specific to Swift 4.

func testArithmeticOverflowSwift4() {
  var _ = Int8(126) + (1 + 1) // expected-error {{arithmetic operation '126 + 2' (on type 'Int8') results in an overflow}}
}
