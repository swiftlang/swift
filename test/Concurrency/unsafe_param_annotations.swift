// RUN: %target-typecheck-verify-swift  -disable-availability-checking

func unsafelySendableClosure(@_unsafeSendable _ closure: () -> Void) { }

func unsafelyMainActorClosure(@_unsafeMainActor _ closure: () -> Void) { }

func unsafelyDoEverythingClosure(@_unsafeSendable @_unsafeMainActor _ closure: () -> Void) { }

struct X {
  func unsafelyDoEverythingClosure(@_unsafeSendable @_unsafeMainActor _ closure: () -> Void) { }
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

func testCalls(x: X) {
  unsafelyMainActorClosure {
    onMainActor()
  }

  unsafelyDoEverythingClosure {
    onMainActor()
  }

  x.unsafelyDoEverythingClosure {
    onMainActor()
  }
  (X.unsafelyDoEverythingClosure)(x)( {
    onMainActor()
    })
}
