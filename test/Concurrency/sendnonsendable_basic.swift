// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -verify-additional-prefix complete- -verify-additional-prefix typechecker-only- -DTYPECHECKER_ONLY %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify -verify-additional-prefix tns-  %s -o /dev/null

// This run validates that for specific test cases around closures, we properly
// emit errors in the type checker before we run sns. This ensures that we know that
// these cases can't happen when SNS is enabled.
//
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify -verify-additional-prefix typechecker-only- -DTYPECHECKER_ONLY %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

/// Classes are always non-sendable, so this is non-sendable
class NonSendableKlass { // expected-complete-note 25{{}}
  var field: NonSendableKlass? = nil

  func asyncCall() async {}
}

class SendableKlass : @unchecked Sendable {}

actor Actor {
  var klass = NonSendableKlass()
  // expected-typechecker-only-note @-1 7{{property declared here}}
  final var finalKlass = NonSendableKlass()

  func useKlass(_ x: NonSendableKlass) {}

  func useSendableFunction(_: @Sendable () -> Void) {}
  func useNonSendableFunction(_: () -> Void) {}
}

final actor FinalActor {
  var klass = NonSendableKlass()
  func useKlass(_ x: NonSendableKlass) {}
}

func useInOut<T>(_ x: inout T) {}
func useValue<T>(_ x: T) {}

@MainActor func transferToMain<T>(_ t: T) async {}

var booleanFlag: Bool { false }

struct SingleFieldKlassBox { // expected-complete-note 2{{consider making struct 'SingleFieldKlassBox' conform to the 'Sendable' protocol}}
  var k = NonSendableKlass()
}

struct TwoFieldKlassBox {
  var k1 = NonSendableKlass()
  var k2 = NonSendableKlass()
}

////////////////////////////
// MARK: Actor Self Tests //
////////////////////////////

extension Actor {
  func warningIfCallingGetter() async {
    await self.klass.asyncCall() // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' outside of actor-isolated context may introduce data races}}
  }

  func warningIfCallingAsyncOnFinalField() async {
    // Since we are calling finalKlass directly, we emit a warning here.
    await self.finalKlass.asyncCall() // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await self.klass.asyncCall() // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
  // expected-tns-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}

  // We only emit a warning on the first use we see, so make sure we do both
  // klass and the closure.
  if await booleanFlag {
    await a.useKlass(ns1) // expected-tns-note {{access here could race}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}
  } else {
    closure() // expected-tns-note {{access here could race}}
  }
}

func closureInOut2(_ a: Actor) async {
  var contents = NonSendableKlass()
  let ns0 = NonSendableKlass()
  let ns1 = NonSendableKlass()

  contents = ns0
  contents = ns1

  var closure = {}

  await a.useKlass(ns0) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure = { useInOut(&contents) } // expected-tns-note {{access here could race}}

  await a.useKlass(ns1) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure() // expected-tns-note {{access here could race}}
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

  await a.useKlass(ns1) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass'}}

  closure() // expected-tns-note {{access here could race}}
}

func transferNonIsolatedNonAsyncClosureTwice() async {
  let a = Actor()

  // This is non-isolated and non-async... we can transfer it safely.
  var actorCaptureClosure = { print(a) }
  await transferToMain(actorCaptureClosure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  actorCaptureClosure = { print(a) }
  await transferToMain(actorCaptureClosure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

extension Actor {
  // Simple just capture self and access field.
  func simpleClosureCaptureSelfAndTransfer() async {
    let closure: () -> () = {
      print(self.klass)
    }
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughTuple() async {
    let closure: () -> () = {
      print(self.klass)
    }
    let x = (1, closure)
    await transferToMain(x) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, () -> ())' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughTupleBackwards() async {
    let closure: () -> () = {
      print(self.klass)
    }

    let x = (closure, 1)
    await transferToMain(x) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '(() -> (), Int)' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfAndTransferThroughOptional() async {
    let closure: () -> () = {
      print(self.klass)
    }
    let x: Any? = (1, closure)
    await transferToMain(x) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(x) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'Any?' into main actor-isolated context may introduce data races}}
  }

  func simpleClosureCaptureSelfWithReinit() async {
    var closure: () -> () = {
      print(self.klass)
    }

    // Error here.
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfWithReinit3() async {
    var closure: () -> () = {}

    // We get a transfer after use error.
    await transferToMain(closure) // expected-tns-warning {{passing argument of non-sendable type '() -> ()' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    if await booleanFlag {
      closure = {
        print(self.klass)
      }
    }

    await transferToMain(closure) // expected-tns-note {{access here could race}}
    // expected-tns-warning @-1 {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
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

    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfThroughTuple2() async {
    let x = (2, self.klass)
    let closure: () -> () = {
      print(x.1)
    }
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  func simpleClosureCaptureSelfThroughOptional() async {
    let x: Any? = (2, self.klass)
    let closure: () -> () = {
      print(x as Any)
    }
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }
}

func testSimpleLetClosureCaptureActor() async {
  let a = Actor()
  let closure = { print(a) }
  await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
  await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
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
    await transferToMain(closure) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // This doesnt since we re-assign
    closure = {}
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // This re-assignment shouldn't error.
    closure = {}
    await transferToMain(closure) // expected-tns-warning {{passing argument of non-sendable type '() -> ()' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}

    // But this will error since we race.
    closure() // expected-tns-note {{access here could race}}
  }
}

/////////////////////////////
// Sendable Function Tests //
/////////////////////////////

// Make sure that we do not error on function values that are Sendable... even
// if the function is converted to a non-Sendable form by a function conversion.
func testConversionsAndSendable(a: Actor, f: @Sendable () -> Void, f2: () -> Void) async {
  // No function conversion.
  await a.useSendableFunction(f)

  // Function conversion.
  await a.useNonSendableFunction(f)

  // Show that we error if we are not sendable.
  await a.useNonSendableFunction(f2) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '() -> Void' into actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

///////////////////////////////////////////////
// Multiple Field Var Assignment Merge Tests //
///////////////////////////////////////////////

func singleFieldVarMergeTest() async {
  var box = SingleFieldKlassBox()
  box = SingleFieldKlassBox()

  // This transfers the entire region.
  await transferToMain(box.k)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // But since box has only a single element, this doesn't race.
  box.k = NonSendableKlass()
  useValue(box.k)
  useValue(box)

  // We transfer the box back to main.
  await transferToMain(box)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'SingleFieldKlassBox' into main actor-isolated context may introduce data races}}

  // And reassign over the entire box, so again we can use it again.
  box = SingleFieldKlassBox()

  useValue(box)
  useValue(box.k)

  await transferToMain(box) // expected-tns-warning {{passing argument of non-sendable type 'SingleFieldKlassBox' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'SingleFieldKlassBox' into main actor-isolated context may introduce data races}}
  

  // But if we use box.k here, we emit an error since we didn't reinitialize at
  // all.
  useValue(box.k) // expected-tns-note {{access here could race}}
}

func multipleFieldVarMergeTest1() async {
  var box = TwoFieldKlassBox()
  box = TwoFieldKlassBox()

  // This transfers the entire region.
  await transferToMain(box.k1) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  

  // So even if we reassign over k1, since we did a merge, this should error.
  box.k1 = NonSendableKlass() // expected-tns-note {{access here could race}}
  useValue(box)
}

func multipleFieldVarMergeTest2() async {
  var box = TwoFieldKlassBox()
  box = TwoFieldKlassBox()

  // This transfers the entire region.
  await transferToMain(box.k1)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // But if we assign over box completely, we can use it again.
  box = TwoFieldKlassBox()

  useValue(box.k1)
  useValue(box.k2)
  useValue(box)

  await transferToMain(box.k2)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // But if we assign over box completely, we can use it again.
  box = TwoFieldKlassBox()

  useValue(box.k1)
  useValue(box.k2)
  useValue(box)
}

func multipleFieldTupleMergeTest1() async {
  var box = (NonSendableKlass(), NonSendableKlass())
  box = (NonSendableKlass(), NonSendableKlass())

  // This transfers the entire region.
  await transferToMain(box.0) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // So even if we reassign over k1, since we did a merge, this should error.
  box.0 = NonSendableKlass() // expected-tns-note {{access here could race}}
  useValue(box)
}

// TODO: Tuples today do not work since I need to change how we assign into
// tuple addresses to use a single instruction. Today SILGen always uses
// multiple values. I am going to fix this in a subsequent commit.
func multipleFieldTupleMergeTest2() async {
  var box = (NonSendableKlass(), NonSendableKlass())

  // This transfers the entire region.
  await transferToMain(box.0)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let box2 = (NonSendableKlass(), NonSendableKlass())
  // But if we assign over box completely, we can use it again.
  box = box2

  useValue(box.0)
  useValue(box.1)
  useValue(box)

  await transferToMain(box.1)
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // But if we assign over box completely, we can use it again.
  box = (NonSendableKlass(), NonSendableKlass())

  useValue(box.0)
  useValue(box.1)
  useValue(box)
}

///////////////////////////
// MARK: ClassFieldTests //
///////////////////////////

class ClassFieldTests { // expected-complete-note 6{{}}
  let letSendableTrivial = 0
  let letSendableNonTrivial = SendableKlass()
  let letNonSendableNonTrivial = NonSendableKlass()
  var varSendableTrivial = 0
  var varSendableNonTrivial = SendableKlass()
  var varNonSendableNonTrivial = NonSendableKlass()  
}

func letSendableTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letSendableNonTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}

  _ = test.letSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letNonSendableNonTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letNonSendableNonTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

func varSendableTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

func varSendableNonTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableNonTrivial  // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialClassFieldTest() async {
  let test = ClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'ClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'ClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varNonSendableNonTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

////////////////////////////////
// MARK: FinalClassFieldTests //
////////////////////////////////

final class FinalClassFieldTests { // expected-complete-note 6 {{}}
  let letSendableTrivial = 0
  let letSendableNonTrivial = SendableKlass()
  let letNonSendableNonTrivial = NonSendableKlass()
  var varSendableTrivial = 0
  var varSendableNonTrivial = SendableKlass()
  var varNonSendableNonTrivial = NonSendableKlass()  
}

func letSendableTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letSendableNonTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letNonSendableNonTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letNonSendableNonTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

func varSendableTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

func varSendableNonTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableNonTrivial  // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialFinalClassFieldTest() async {
  let test = FinalClassFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'FinalClassFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'FinalClassFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varNonSendableNonTrivial // expected-tns-note {{access here could race}}
  useValue(test)
}

////////////////////////////
// MARK: StructFieldTests //
////////////////////////////

struct StructFieldTests { // expected-complete-note 31 {{}}
  let letSendableTrivial = 0
  let letSendableNonTrivial = SendableKlass()
  let letNonSendableNonTrivial = NonSendableKlass()
  var varSendableTrivial = 0
  var varSendableNonTrivial = SendableKlass()
  var varNonSendableNonTrivial = NonSendableKlass()  
}

func letSendableTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letSendableNonTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letNonSendableNonTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.letNonSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func letSendableTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letSendableNonTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.letSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func letNonSendableNonTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.letNonSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

// Lets can access sendable let/var even if captured in a closure.
func letNonSendableNonTrivialLetStructFieldClosureTest() async {
  let test = StructFieldTests()
  let cls = {
    print(test)
  }
  _ = cls
  var cls2 = {}
  cls2 = {
    print(test)
  }
  _ = cls2
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.letSendableNonTrivial
  _ = z
  let z2 = test.varSendableNonTrivial
  _ = z2
  useValue(test) // expected-tns-note {{access here could race}}
}

func varSendableTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func varSendableNonTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialLetStructFieldTest() async {
  let test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.varNonSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func varSendableTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func varSendableNonTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  _ = test.varSendableNonTrivial
  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialVarStructFieldTest() async {
  var test = StructFieldTests() 
  test = StructFieldTests()
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.varNonSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

// vars cannot access sendable let/var if captured in a closure.
func varNonSendableNonTrivialLetStructFieldClosureTest1() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  let cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.letSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest2() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  let cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.varSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest3() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  let cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

// vars cannot access sendable let/var if captured in a closure.
func varNonSendableNonTrivialLetStructFieldClosureTest4() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.letSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest5() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  let z = test.varSendableNonTrivial // expected-tns-note {{access here could race}}
  _ = z
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest6() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    test = StructFieldTests()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest7() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    test.varSendableNonTrivial = SendableKlass()
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest8() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    useInOut(&test)
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureTest9() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}
  cls = {
    useInOut(&test.varSendableNonTrivial)
  }
  _ = cls
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive1() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  if await booleanFlag {
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls
  } else {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}

    test.varSendableNonTrivial = SendableKlass()
  }

  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive2() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  if await booleanFlag {
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls

    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  }

  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)
}

// We do not error when accessing the sendable field in this example since the
// transfer is not reachable from the closure. Instead we emit an error on test.
func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive3() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  if await booleanFlag {
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls
  } else {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  }

  test.varSendableNonTrivial = SendableKlass()
  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive4() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  for _ in 0..<1024 {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
    test = StructFieldTests() // expected-tns-note {{access here could race}}
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls
  }

  test.varSendableNonTrivial = SendableKlass()
  useValue(test)
}

func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive5() async {
  var test = StructFieldTests()
  test = StructFieldTests()

  // The reason why we error here is that even though we reassign at the end of
  // the for loop and currently understand that test has a new value different
  // from the value assigned to test at the beginning of the for loop, when we
  // merge back through the for loop, we have to merge the regions due to the
  // union operation we perform. So the conservatism of the dataflow creates
  // this. This is a case where we are going to need to be able to have the
  // compiler explain the regions well.
  for _ in 0..<1024 {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-tns-note @-1 {{access here could race}}
    // expected-complete-warning @-2 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
    test = StructFieldTests()
  }

  test.varSendableNonTrivial = SendableKlass()
  useValue(test)  // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive6() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  // In this test case, the first transfer errors on the
  // test.varSendableNonTrivial b/c of the cls. Since we don't have cls on the
  // else path, it emits on useValue(test).
  if await booleanFlag {
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  } else {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  }

  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test)  // expected-tns-note {{access here could race}}
}

// In this case since we are tracking the transfer from the else statement, we
// track the closure.
func varNonSendableNonTrivialLetStructFieldClosureFlowSensitive7() async {
  var test = StructFieldTests()
  test = StructFieldTests()
  var cls = {}

  if await booleanFlag {
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  } else {
    cls = {
      useInOut(&test.varSendableNonTrivial)
    }
    _ = cls
    await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type 'StructFieldTests' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'StructFieldTests' into main actor-isolated context may introduce data races}}
  }

  test.varSendableNonTrivial = SendableKlass() // expected-tns-note {{access here could race}}
  useValue(test) // expected-tns-note {{access here could race}}
}

////////////////////////////
// MARK: TupleFieldTests //
////////////////////////////

func varSendableTrivialLetTupleFieldTest() async {
  let test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  let z = test.0
  useValue(z)
  useValue(test) // expected-tns-note {{access here could race}}
}

func varSendableNonTrivialLetTupleFieldTest() async {
  let test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  let z = test.1
  useValue(z)
  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialLetTupleFieldTest() async {
  let test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  let z = test.2
  // The SIL emitted for the assignment of the tuple is just a jumble of
  // instructions that are not semantically significant. The result is that we
  // need a useValue here to actual provide something for the pass to chew on.
  useValue(z)  // expected-tns-note {{access here could race}}
  useValue(test)
}

func varSendableTrivialVarTupleFieldTest() async {
  var test = (0, SendableKlass(), NonSendableKlass()) 
  test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  _ = test.0
  useValue(test) // expected-tns-note {{access here could race}}
}

func varSendableNonTrivialVarTupleFieldTest() async {
  var test = (0, SendableKlass(), NonSendableKlass()) 
  test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  _ = test.1
  useValue(test) // expected-tns-note {{access here could race}}
}

func varNonSendableNonTrivialVarTupleFieldTest() async {
  var test = (0, SendableKlass(), NonSendableKlass()) 
  test = (0, SendableKlass(), NonSendableKlass())
  await transferToMain(test) // expected-tns-warning {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{passing argument of non-sendable type '(Int, SendableKlass, NonSendableKlass)' into main actor-isolated context may introduce data races}}
  let z = test.2 // expected-tns-note {{access here could race}}
  useValue(z)
  useValue(test)
}

//////////////////////////////
// MARK: Control Flow Tests //
//////////////////////////////

func controlFlowTest1() async {
  let x = NonSendableKlass()

  if await booleanFlag {
    await transferToMain(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  } else {
    await transferToMain(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }

  useValue(x) // expected-tns-note 2{{access here could race}}
}

// This test seems like something that we should not error upon. What is
// happening here is that when we get to the bottom of the for loop, we realize
// that x's old value and x/x's new value are in different regions. But when we
// merge back into the loop header, we note that the loop entry block has x/x's
// old value in the same region so when we merge, we get that x/x's old
// value/x's new value are all in the same region. We emit the error on useValue
// as well afterwards since if we exit from the loop header, we have that large
// merge region leave the for loop.
func controlFlowTest2() async {
  var x = NonSendableKlass()

  for _ in 0..<1024 {
    await transferToMain(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    // expected-tns-note @-1 {{access here could race}}
    // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    x = NonSendableKlass()
  }

  useValue(x) // expected-tns-note {{access here could race}}
}
