// RUN: %target-swift-frontend -typecheck -verify %s
// REQUIRES: concurrency

@preconcurrency @MainActor func f() { }
// expected-note@-1 2{{calls to global function 'f()' from outside of its actor context are implicitly asynchronous}}

@preconcurrency typealias FN = @Sendable () -> Void

struct Outer {
  @preconcurrency typealias FN = @Sendable () -> Void
}

func test() {
  var _: Outer.FN = {
    f()
  }

  var _: FN = {
    f()
    print("Hello")
  }
}

@available(SwiftStdlib 5.1, *)
func testAsync() async {
  var _: Outer.FN = {
    f() // expected-error{{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
  }

  var _: FN = {
    f() // expected-error{{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
    print("Hello")
  }
}
