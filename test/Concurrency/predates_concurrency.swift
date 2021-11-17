// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

@_predatesConcurrency func unsafelySendableClosure(_ closure: @Sendable () -> Void) { }

@_predatesConcurrency func unsafelyMainActorClosure(_ closure: @MainActor () -> Void) { }

@_predatesConcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }

struct X {
  @_predatesConcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }
}


func testInAsync(x: X) async {
  let _: Int = unsafelySendableClosure // expected-error{{type '(@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(@MainActor () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
}

func testElsewhere(x: X) {
  let _: Int = unsafelySendableClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (() -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(() -> Void) -> ()'}}
}

@MainActor func onMainActor() { }

@MainActor @_predatesConcurrency func onMainActorAlways() { }

@_predatesConcurrency @MainActor class MyModelClass {
  func f() { }
}

func testCalls(x: X) {
  unsafelyMainActorClosure {
    onMainActor()
  }

  unsafelyDoEverythingClosure {
    onMainActor()
  }

  x.unsafelyDoEverythingClosure {
    onMainActor()
    onMainActorAlways()
  }
  (X.unsafelyDoEverythingClosure)(x)( {
    onMainActor()
    })

  onMainActorAlways() // okay, haven't adopted concurrency

  let _: () -> Void = onMainActorAlways

  // both okay
  let c = MyModelClass()
  c.f()
}

func testCallsWithAsync() async {
  onMainActorAlways() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to global function 'onMainActorAlways()' from outside of its actor context are implicitly asynchronous}}

  let _: () -> Void = onMainActorAlways // expected-error{{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  c.f() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}
