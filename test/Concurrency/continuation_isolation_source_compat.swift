// RUN: %target-swift-frontend -typecheck -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -parse-as-library
// REQUIRES: concurrency

func testWithUnsafeContinuation(isolation: isolated (any Actor)?) async -> Int {
  await withUnsafeContinuation(isolation: isolation) { continuation in // expected-warning {{'withUnsafeContinuation(isolation:_:)' is deprecated: Replaced by nonisolated(nonsending) overload}}
    continuation.resume(returning: 42)
  }
}

func testWithUnsafeThrowingContinuation(isolation: isolated (any Actor)?) async throws -> Int {
  try await withUnsafeThrowingContinuation(isolation: isolation) { continuation in // expected-warning {{'withUnsafeThrowingContinuation(isolation:_:)' is deprecated: Replaced by nonisolated(nonsending) overload}}
    continuation.resume(returning: 42)
  }
}

func testWithCheckedContinuation(isolation: isolated (any Actor)?) async -> Int {
  await withCheckedContinuation(isolation: isolation) { continuation in // expected-warning {{'withCheckedContinuation(isolation:function:_:)' is deprecated: Replaced by nonisolated(nonsending) overload}}
    continuation.resume(returning: 42)
  }
}

func testWithCheckedThrowingContinuation(isolation: isolated (any Actor)?) async throws -> Int {
  try await withCheckedThrowingContinuation(isolation: isolation) { continuation in // expected-warning {{'withCheckedThrowingContinuation(isolation:function:_:)' is deprecated: Replaced by nonisolated(nonsending) overload}}
    continuation.resume(returning: 42)
  }
}
