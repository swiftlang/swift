// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=complete

// REQUIRES: concurrency

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

class NonSendableC { // expected-note{{class 'NonSendableC' does not conform to the 'Sendable' protocol}}
    var x: Int = 0

    @Sendable func inc() { // expected-warning{{instance method of non-Sendable type 'NonSendableC' cannot be marked as '@Sendable'}}
        x += 1
    }
}

struct S<T> { // expected-note{{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  let t: T

  @Sendable func test() {} // expected-warning{{instance method of non-Sendable type 'S<T>' cannot be marked as '@Sendable'}}
}

extension S: Sendable where T: Sendable {
  @Sendable func test2() {}
}

@available(SwiftStdlib 5.1, *)
@MainActor @Sendable func globalActorFunc() { } // expected-warning{{main actor-isolated synchronous global function 'globalActorFunc()' cannot be marked as '@Sendable'}}

@available(SwiftStdlib 5.1, *)
@MainActor @Sendable func globalActorFuncAsync() async { }
