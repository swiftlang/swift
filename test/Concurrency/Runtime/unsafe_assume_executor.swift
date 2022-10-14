// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking %import-libdispatch) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: single_threaded_concurrency

import Foundation

enum SomeError: Error {
  case whatever
}

@MainActor func expectOnMainThreadIs(_ expectation: Bool) {
  assert(expectation == Thread.isMainThread)
}

@main
struct Main {
  static func main() async {
    do {
      try _unsafeAssumeOnMainActor {
        expectOnMainThreadIs(true)
        throw SomeError.whatever
      }
      fatalError("bug")
    } catch {}

    _ = await (Task.detached {
      // CHECK: warning: data race detected: @MainActor function at {{.*}}unsafe_assume_executor.swift:34 was not called on the main thread
      _unsafeAssumeOnMainActor {
        expectOnMainThreadIs(false)
      }
    }).result
    // CHECK-NOT: warning

  }
}