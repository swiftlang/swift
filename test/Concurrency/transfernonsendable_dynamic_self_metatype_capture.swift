// RUN: %target-swift-frontend -emit-sil -parse-as-library -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -parse-as-library -strict-concurrency=complete -target %target-swift-5.1-abi-triple %s -o - | %FileCheck %s

// REQUIRES: concurrency

// The region-based isolation checker used to report an "unknown pattern"
// false positive for a `sending` closure that calls `Self.foo { ... }` from
// within a non-final class method. The outer closure's `partial_apply`
// captures a `@dynamic_self` metatype, which causes `self` to appear as a
// type-defs operand on the PA. The region analysis merged every operand
// including type-dependent ones, which incorrectly pulled `self` into the
// closure's region and tripped the unknown-pattern fallback.

func withSendingFree<R: Sendable>(
  _ body: sending () async throws -> sending R
) async throws -> R {
  try await body()
}

class DynamicSelfCapture {
  // Verify that SILGen actually emits the `@dynamic_self` metatype capture
  // that this test is meant to exercise. If a future SILGen change stops
  // emitting the capture, this CHECK will fail and the test should be
  // updated to construct the pattern another way.
  // CHECK-LABEL: sil hidden @$s49transfernonsendable_dynamic_self_metatype_capture18DynamicSelfCaptureC4testyyYaKF :
  // CHECK: partial_apply {{.*}} : $@convention(thin) {{.*}}(@thick @dynamic_self DynamicSelfCapture.Type) ->
  // CHECK: } // end sil function '$s49transfernonsendable_dynamic_self_metatype_capture18DynamicSelfCaptureC4testyyYaKF'
  // expected-no-diagnostics
  func test() async throws {
    try await withSendingFree {
      try await Self.withSending {
        return ()
      }
    }
  }

  static func withSending<R: Sendable>(
    _ body: sending () async throws -> sending R
  ) async throws -> R {
    try await body()
  }
}
