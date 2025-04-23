// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

// Test that conditionally returning an Optional succeeds.
//
// See scope_fixup.sil: testReturnPhi.
@available(Span 0.1, *)
extension Array {
  @lifetime(&self)
  mutating func getOptionalMutableSpan() -> MutableSpan<Element>? {
    if count == 0 {
      return nil
    }
    return mutableSpan
  }
}

// Test that returning an immutable Span from an inout Array.
//
// See scope_fixup.sil: testNestedModRead.
@available(Span 0.1, *)
@inline(never)
@lifetime(&array)
func getImmutableSpan(_ array: inout [Int]) -> Span<Int> {
 return array.span
}
