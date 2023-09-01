// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=complete -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: asserts

@Sendable func globalFunc() { }

@available(SwiftStdlib 5.1, *)
actor A {
  var state: Bool = false
  
  @Sendable func f() { // expected-warning{{actor-isolated synchronous instance method 'f()' cannot be marked as '@Sendable'}}
    state = true
  }

  @Sendable nonisolated func g() { }

  @Sendable func fAsync() async {
    state = true
  }
}

@available(SwiftStdlib 5.1, *)
@MainActor @Sendable func globalActorFunc() { } // expected-warning{{main actor-isolated synchronous global function 'globalActorFunc()' cannot be marked as '@Sendable'}}

@available(SwiftStdlib 5.1, *)
@MainActor @Sendable func globalActorFuncAsync() async { }
