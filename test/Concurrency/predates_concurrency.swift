// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

@_predatesConcurrency func unsafelySendableClosure(_ closure: @Sendable () -> Void) { }

@_predatesConcurrency func unsafelyMainActorClosure(_ closure: @MainActor () -> Void) { }

@_predatesConcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }

struct X {
  @_predatesConcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }

  @_predatesConcurrency var sendableVar: @Sendable () -> Void
  @_predatesConcurrency var mainActorVar: @MainActor () -> Void

  @_predatesConcurrency
  subscript(_: @MainActor () -> Void) -> (@Sendable () -> Void) { {} }

  @_predatesConcurrency
  static subscript(statically _: @MainActor () -> Void) -> (@Sendable () -> Void) { { } }
}

@MainActor func onMainActor() { }

func testInAsync(x: X) async {
  let _: Int = unsafelySendableClosure // expected-error{{type '(@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(@MainActor () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}

  let _: Int = x.sendableVar // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '@MainActor () -> Void'}}

  let _: Int = x[{ onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
}

func testElsewhere(x: X) {
  let _: Int = unsafelySendableClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (() -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = x.sendableVar // expected-error{{type '() -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '() -> Void'}}
  let _: Int = x[{ onMainActor() }] // expected-error{{type '() -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '() -> Void'}}
}

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

// ---------------------------------------------------------------------------
// Protocols that inherit Sendable and predate concurrency.
// ---------------------------------------------------------------------------
@_predatesConcurrency protocol P: Sendable { }
protocol Q: P { }

class NS { } // expected-note{{class 'NS' does not conform to the 'Sendable' protocol}}

struct S1: P {
  var ns: NS
}

struct S2: Q {
  var ns: NS
}

struct S3: Q, Sendable {
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'S3' has non-sendable type 'NS'}}
}

// ---------------------------------------------------------------------------
// Historical attribute names do nothing (but are permitted)
// ---------------------------------------------------------------------------
func aFailedExperiment(@_unsafeSendable _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeSendable' attribute has been removed in favor of @_predatesConcurrency}}

func anothingFailedExperiment(@_unsafeMainActor _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeMainActor' attribute has been removed in favor of @_predatesConcurrency}}
