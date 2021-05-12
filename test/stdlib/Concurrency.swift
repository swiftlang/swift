// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

// Make sure the import succeeds
import _Concurrency

// Make sure the type shows up (including under its old name, for
// short-term source compatibility)
@available(SwiftStdlib 5.5, *)
extension PartialAsyncTask {
}
@available(SwiftStdlib 5.5, *)
extension UnownedJob {
}
