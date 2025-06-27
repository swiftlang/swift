// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

// This test is meant to verify that a @MainActor constraint is accepted without
// any availability restrictions for all targets.

@MainActor
struct AlwaysAvailable {}

@preconcurrency @MainActor
struct AlwaysAvailableUnsafe {}

@available(SwiftStdlib 5.1, *)
@MainActor
struct AvailableSwift5_1 {}

@available(SwiftStdlib 5.1, *)
@preconcurrency @MainActor
struct AvailableSwift5_1Unsafe {}
