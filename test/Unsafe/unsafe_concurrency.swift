// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -enable-experimental-feature StrictConcurrency

// REQUIRES: concurrency

// expected-warning@+1{{@unchecked conformance involves unsafe code}}
class C: @unchecked Sendable {
  var counter: Int = 0
}

@available(SwiftStdlib 5.1, *)
func f() async {
  // expected-warning@+1{{nonisolated(unsafe) involves unsafe code}}
  nonisolated(unsafe) var counter = 0
  Task.detached {
    counter += 1
  }
  counter += 1
  print(counter)
}

