// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

// This test is meant to verify that a @MainActor constraint is accepted without
// any availability restrictions for all targets.

@MainActor
struct AlwaysAvailable {}

@MainActor(unsafe)
struct AlwaysAvailableUnsafe {}

@available(SwiftStdlib 5.1, *)
@MainActor
struct AvailableSwift5_1 {}

@available(SwiftStdlib 5.1, *)
@MainActor(unsafe)
struct AvailableSwift5_1Unsafe {}
