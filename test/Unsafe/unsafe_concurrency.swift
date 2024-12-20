// RUN: %target-typecheck-verify-swift -enable-experimental-feature WarnUnsafe -enable-experimental-feature StrictConcurrency

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictConcurrency
// REQUIRES: swift_feature_WarnUnsafe

// expected-warning@+2{{@unchecked conformance involves unsafe code}}
// expected-note@+1{{make the enclosing class @unsafe to allow unsafe conformance to protocol 'Sendable'}}{{1-1=@unsafe }}
class C: @unchecked Sendable {
  var counter: Int = 0
}

nonisolated(unsafe) var globalCounter = 0

@available(SwiftStdlib 5.1, *)
func f() async { // expected-warning{{global function 'f' involves unsafe code; use '@safe(unchecked)' to assert that the code is memory-safe}}
  nonisolated(unsafe) var counter = 0
  Task.detached {
    counter += 1 // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  }
  counter += 1 // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  print(counter) // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  print(globalCounter) // expected-note{{reference to nonisolated(unsafe) var 'globalCounter' is unsafe in concurrently-executing code}}
}
