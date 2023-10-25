// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -verify-additional-prefix complete- -verify-additional-prefix typechecker-only- -DTYPECHECKER_ONLY %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature SendNonSendable -disable-availability-checking -verify -verify-additional-prefix sns-  %s -o /dev/null

// This run validates that for specific test cases around closures, we properly
// emit errors in the type checker before we run sns. This ensures that we know that
// these cases can't happen when SNS is enabled.
//
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature SendNonSendable -disable-availability-checking -verify -verify-additional-prefix typechecker-only- -DTYPECHECKER_ONLY %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

/// Classes are always non-sendable, so this is non-sendable
class NonSendableKlass { // expected-complete-note 9{{}}
  var field: NonSendableKlass? = nil

  func asyncCall() async {}
}

actor Actor {
  var klass = NonSendableKlass()
  // expected-typechecker-only-note @-1 7{{property declared here}}
  final var finalKlass = NonSendableKlass()

  func useKlass(_ x: NonSendableKlass) {}
}

final actor FinalActor {
  var klass = NonSendableKlass()
  func useKlass(_ x: NonSendableKlass) {}
}

func useInOut<T>(_ x: inout T) {}
func useValue<T>(_ x: T) {}

@MainActor func transferToMain<T>(_ t: T) async {}

var booleanFlag: Bool { false }

////////////////////////////
// MARK: Actor Self Tests //
////////////////////////////

extension Actor {
  func warningIfCallingGetter() async {
    await self.klass.asyncCall() // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' outside of actor-isolated context may introduce data races}}
  }

  func warningIfCallingAsyncOnFinalField() async {
    // Since we are calling finalKlass directly, we emit a warning here.
    await self.finalKlass.asyncCall() // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' outside of actor-isolated context may introduce data races}}
  }

  // We do not warn on this since we warn in the caller of our getter instead.
  var klassGetter: NonSendableKlass {
    self.finalKlass
  }
}

extension FinalActor {
  func warningIfCallingAsyncOnFinalField() async {
    // Since our whole class is final, we emit the error directly here.
    await self.klass.asyncCall() // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' outside of actor-isolated context may introduce data races}}
  }
}

/////////////////////////////
// MARK: Closure Formation //
/////////////////////////////

// This test makes sure that we can properly pattern match project_box.
func formClosureWithoutCrashing() {
  var c = NonSendableKlass() // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}
  let _ = { print(c) }
}

// In this test, closure is not promoted into its box form. As a result, we
// treat assignments into contents as a merge operation rather than an assign.
func closureInOut(_ a: Actor) async {
  var contents = NonSendableKlass()
  let ns0 = NonSendableKlass()
  let ns1 = NonSendableKlass()

  contents = ns0
  contents = ns1

  var closure = {}
  closure = { useInOut(&contents) }

  await a.useKlass(ns0)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}
  // expected-sns-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  await a.useKlass(ns1) // expected-sns-note {{access here could race}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure() // expected-sns-note {{access here could race}}
}

func closureInOut2(_ a: Actor) async {
  var contents = NonSendableKlass()
  let ns0 = NonSendableKlass()
  let ns1 = NonSendableKlass()

  contents = ns0
  contents = ns1

  var closure = {}

  await a.useKlass(ns0) // expected-sns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure = { useInOut(&contents) } // expected-sns-note {{access here could race}}

  await a.useKlass(ns1) // expected-sns-note {{access here could race}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure()
}

func closureNonInOut(_ a: Actor) async {
  var contents = NonSendableKlass()
  let ns0 = NonSendableKlass()
  let ns1 = NonSendableKlass()

  contents = ns0
  contents = ns1

  var closure = {}

  await a.useKlass(ns0)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure = { useValue(contents) }

  await a.useKlass(ns1) // expected-sns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure() // expected-sns-note {{access here could race}}
}

func transferNonIsolatedNonAsyncClosureTwice() async {
  let a = Actor()

  // This is non-isolated and non-async... we can transfer it safely.
  var actorCaptureClosure = { print(a) }
  await transferToMain(actorCaptureClosure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  actorCaptureClosure = { print(a) }
  await transferToMain(actorCaptureClosure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

extension Actor {
  // Simple just capture self and access field.
  func simpleClosureCaptureSelfAndTransfer() async {
    let closure: () -> () = {
      print(self.klass)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughTuple() async {
    let closure: () -> () = {
      print(self.klass)
    }
    let x = (1, closure)
    await transferToMain(x) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, () -> ())' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughTupleBackwards() async {
    let closure: () -> () = {
      print(self.klass)
    }
    // NOTE: We do not error on this today since we assign into 1 and that makes
    // x assign fresh. It will be fixed in a forthcoming commit.
    let x = (closure, 1)
    await transferToMain(x)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '(() -> (), Int)' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughOptional() async {
    let closure: () -> () = {
      print(self.klass)
    }
    let x: Any? = (1, closure)
    await transferToMain(x) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'Any?' into main actor-isolated context may introduce data races}}
  }

  func simpleClosureCaptureSelfAndTransferThroughOptionalBackwards() async {
    let closure: () -> () = {
      print(self.klass)
    }
    // In contrast to the tuple backwards case, we do error here since the value
    // we are forming is an Any?  so we actually form the tuple as an object and
    // store it all as once.
    let x: Any? = (closure, 1)
    await transferToMain(x) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'Any?' into main actor-isolated context may introduce data races}}
  }

  func simpleClosureCaptureSelfWithReinit() async {
    var closure: () -> () = {
      print(self.klass)
    }

    // Error here.
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    closure = {}

    // But not here.
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfWithReinit2() async {
    var closure: () -> () = {}

    // No error here.
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    closure = {
      print(self.klass)
    }

    // Error here.
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfWithReinit3() async {
    var closure: () -> () = {}

    // We get a transfer after use error.
    await transferToMain(closure) // expected-sns-warning {{passing argument of non-sendable type '() -> ()' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    if await booleanFlag {
      closure = {
        print(self.klass)
      }
    }

    await transferToMain(closure) // expected-sns-note {{access here could race}}
    // expected-sns-warning @-1 {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  // In this case, we reinit along both paths, but only one has an actor derived
  // thing.
  func simpleClosureCaptureSelfWithReinit4() async {
    var closure: () -> () = {}

    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    if await booleanFlag {
      closure = {
        print(self.klass)
      }
    } else {
      closure = {}
    }

    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  #if TYPECHECKER_ONLY

  func simpleClosureCaptureSelfThroughTupleWithFieldAccess() async {
    // In this case, we erase that we accessed self so we hit a type checker
    // error. We could make this a SNS error, but since in the other cases where
    // we have a non-isolated non-async we are probably going to change it to be
    // async as well making these errors go away.
    let x = (self, 1)
    let closure: () -> () = {
      print(x.0.klass) // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
    }
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  #endif

  func simpleClosureCaptureSelfThroughTuple() async {
    let x = (self, self.klass)
    let closure: () -> () = {
      print(x.1)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfThroughTuple2() async {
    let x = (2, self.klass)
    let closure: () -> () = {
      print(x.1)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfThroughOptional() async {
    let x: Any? = (2, self.klass)
    let closure: () -> () = {
      print(x as Any)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }
}

func testSimpleLetClosureCaptureActor() async {
  let a = Actor()
  let closure = { print(a) }
  await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

#if TYPECHECKER_ONLY

func testSimpleLetClosureCaptureActorField() async {
  let a = Actor()
  let closure = { print(a.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleLetClosureCaptureActorFieldThroughTuple() async {
  let a = (Actor(), 0)
  let closure = { print(a.0.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleLetClosureCaptureActorFieldThroughOptional() async {
  let a: Actor? = Actor()
  let closure = { print(a!.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

#endif

func testSimpleVarClosureCaptureActor() async {
  let a = Actor()
  var closure = {}
  closure = { print(a) }
  await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

#if TYPECHECKER_ONLY

func testSimpleVarClosureCaptureActorField() async {
  let a = Actor()
  var closure = {}
  closure = { print(a.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleVarClosureCaptureActorFieldThroughTuple() async {
  let a = (Actor(), 0)
  var closure = {}
  closure = { print(a.0.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleVarClosureCaptureActorFieldThroughOptional() async {
  let a: Actor? = Actor()
  var closure = {}
  closure = { print(a!.klass) } // expected-typechecker-only-error {{actor-isolated property 'klass' can not be referenced from a non-isolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

#endif

extension Actor {
  // Make sure that we properly propagate actor derived from klass into field's
  // value.
  func simplePropagateNonSendableThroughMultipleProperty() async {
    let f = self.klass.field!
    let closure: () -> () = {
      print(f)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  // Make sure that we properly propagate actor derived from klass into field's
  // value.
  func simplePropagateNonSendableThroughMultipleProperty2() async {
    let f = self.klass.field!
    let closure: () -> () = {
      print(f.field!)
    }
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }
}

extension Actor {
  func testVarReassignStopActorDerived() async {
    var closure: () -> () = {
      print(self)
    }

    // This should error.
    await transferToMain(closure) // expected-sns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // This doesnt since we re-assign
    closure = {}
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // This re-assignment shouldn't error.
    closure = {}
    await transferToMain(closure) // expected-sns-warning {{passing argument of non-sendable type '() -> ()' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // But this will error since we race.
    closure() // expected-sns-note {{access here could race}}
  }
}
