// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -verify-additional-prefix complete-tns- -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -verify-additional-prefix complete-tns- -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

@preconcurrency @MainActor func f() { }
// expected-note @-1 2{{calls to global function 'f()' from outside of its actor context are implicitly asynchronous}}
// expected-complete-tns-note @-2 2{{calls to global function 'f()' from outside of its actor context are implicitly asynchronous}}

@preconcurrency typealias FN = @Sendable () -> Void

struct Outer {
  @preconcurrency typealias FN = @Sendable () -> Void
}

@preconcurrency func preconcurrencyFunc(callback: FN) {}

func test() {
  var _: Outer.FN = {
    f() // expected-complete-tns-warning {{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
  }

  var _: FN = {
    f() // expected-complete-tns-warning {{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
    print("Hello")
  }

  var mutableVariable = 0
  preconcurrencyFunc {
    mutableVariable += 1 // no sendable warning unless we have complete
    // expected-complete-tns-warning @-1 {{mutation of captured var 'mutableVariable' in concurrently-executing code; this is an error in the Swift 6 language mode}}
  }
  mutableVariable += 1
}

@available(SwiftStdlib 5.1, *)
func testAsync() async {
  var _: Outer.FN = {
    f() // expected-warning{{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
  }

  var _: FN = {
    f() // expected-warning{{call to main actor-isolated global function 'f()' in a synchronous nonisolated context}}
    print("Hello")
  }

  var mutableVariable = 0
  preconcurrencyFunc {
    mutableVariable += 1 // expected-warning{{mutation of captured var 'mutableVariable' in concurrently-executing code; this is an error in the Swift 6 language mode}}
  }
  mutableVariable += 1
}

// rdar://99518344 - @Sendable in nested positions
@preconcurrency typealias OtherHandler = @Sendable () -> Void
@preconcurrency typealias Handler = (@Sendable () -> OtherHandler?)?
@preconcurrency func f(arg: Int, withFn: Handler?) {}

class C { // expected-complete-tns-note {{class 'C' does not conform to the 'Sendable' protocol}}
  func test() {
    f(arg: 5, withFn: { [weak self] () -> OtherHandler? in
        _ = self // expected-complete-tns-warning {{capture of 'self' with non-sendable type 'C?' in a `@Sendable` closure}}
        return nil
      })
  }
}
