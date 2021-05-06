// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

// Make sure the import succeeds
import _Concurrency

// Make sure the type shows up (including under its old name, for
// short-term source compatibility)
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension PartialAsyncTask {
}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnownedJob {
}
