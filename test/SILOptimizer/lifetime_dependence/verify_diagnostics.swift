// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

// Test that conditionally returning an Optional succeeds.
//
// See scope_fixup.sil: testReturnPhi.
@available(SwiftStdlib 6.2, *)
extension Array {
  @lifetime(&self)
  mutating func dumb() -> MutableSpan<Element>? {
    if count == 0 {
      return nil
    }
    return mutableSpan
  }
}
