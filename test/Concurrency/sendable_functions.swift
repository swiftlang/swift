// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency


@Sendable func globalFunc() { }

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

@MainActor @Sendable func globalActorFunc() { } // expected-warning{{main actor-isolated synchronous global function 'globalActorFunc()' cannot be marked as '@Sendable'}}

@MainActor @Sendable func globalActorFuncAsync() async { }
