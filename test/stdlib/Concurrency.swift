// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

// Make sure the import succeeds
import _Concurrency

// Make sure the type shows up (including under its old name, for
// short-term source compatibility)
@available(SwiftStdlib 5.1, *)
extension PartialAsyncTask {
  // expected-warning@-1 {{'PartialAsyncTask' is deprecated: renamed to 'UnownedJob'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-2 {{use 'UnownedJob' instead}}
}
@available(SwiftStdlib 5.1, *)
extension UnownedJob {
}
