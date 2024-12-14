// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -enable-experimental-feature StrictConcurrency

// REQUIRES: concurrency
// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_StrictConcurrency
// REQUIRES: swift_feature_WarnUnsafe

// expected-warning@+2{{@unchecked conformance involves unsafe code}}
// expected-note@+1{{make the enclosing class @unsafe to allow unsafe conformance to protocol 'Sendable'}}{{1-1=@unsafe }}
class C: @unchecked Sendable {
  var counter: Int = 0
}

@available(SwiftStdlib 5.1, *)
func f() async {
  // expected-warning@+2{{nonisolated(unsafe) involves unsafe code}}
  // expected-note@+1{{make var 'counter' @unsafe to indicate that its use is not memory-safe}}
  nonisolated(unsafe) var counter = 0
  Task.detached {
    counter += 1
  }
  counter += 1
  print(counter)
}

