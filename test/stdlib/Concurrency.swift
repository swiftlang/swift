// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

// Make sure the import succeeds
import _Concurrency

// Make sure the type shows up
extension PartialAsyncTask {
}
