// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// '@oneway(never)' asks callers to wait for the call, so a 'send' of a
// call to it is a warning in the 'SentOnewayNeverCall' group, which
// '@diagnose' can suppress or turn into an error. It is a declaration
// attribute only, which does not affect the function type

// ==== -----------------------------------------------------------------------
// MARK: Declarations

@MainActor @oneway(never) func flush() {}
@MainActor @oneway(never) func flushAsync() async {}
@oneway(never) func value() -> Int { 0 }
@oneway(never) func mayThrow() throws {}

actor Worker {
  @oneway(never) func drain() {}
  func tick() {}

  // A 'send' within the actor's own isolation domain enqueues the call on
  // the actor itself, and is not diagnosed
  func sendToSelf() {
    send self.drain()
    send drain()
  }

  // A 'send' to another actor crosses isolation domains
  func sendToOther(_ other: Worker) async {
    send other.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}{{5-9=await}}
  }

  // A nonisolated method of the actor is not isolated to it
  nonisolated func sendFromNonisolated() {
    send drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}{{none}}
  }
}

func sendToIsolatedParameter(_ worker: isolated Worker) {
  send worker.drain()
}

// expected-error@+1{{'@oneway' contradicts previous attribute '@oneway(never)'}}
@MainActor @oneway(never) @oneway func both() {}

// expected-error@+1{{'@oneway(never)' contradicts previous attribute '@oneway'}}
@MainActor @oneway @oneway(never) func bothReversed() {}

// expected-error@+2{{duplicate attribute}}
// expected-note@+1{{attribute already specified here}}
@oneway(never) @oneway(never) func twice() {}

// expected-error@+1:22{{attribute can only be applied to declarations, not types}}
typealias AwaitFn = @oneway(never) () -> Void

// '@oneway' only takes 'never'
// expected-error@+1{{expected 'never' in '@oneway' attribute}}{{9-15=never}}
@oneway(always) func notAlways() {}

// ==== -----------------------------------------------------------------------
// MARK: Calls

func directCalls(_ worker: Worker) async {
  send flush() // expected-warning{{'@oneway(never)' global function 'flush()' is not intended to be called with 'send', use 'await' instead}}{{3-7=await}}
  send worker.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}
  send worker.tick()

  await flush()
  await worker.drain()
}

// A 'send' from the same global actor is not diagnosed
@MainActor func sameGlobalActor() {
  send flush()
}

@MainActor func sameGlobalActorAsync() async {
  send flush()
}

// The isolation of a closure is not known when the 'send' is checked, so a
// 'send' in a closure is diagnosed, even when it is isolated to the same
// global actor
@MainActor func sameGlobalActorInClosure() {
  let closure = {
    send flush() // expected-warning{{'@oneway(never)' global function 'flush()' is not intended to be called with 'send', use 'await' instead}}{{none}}
  }
  closure()
}

@MainActor protocol Drainable {
  @oneway(never) func drain()
}

// A plain witness of a '@oneway(never)' requirement is fine
struct PlainDrainer: Drainable {
  func drain() {}
}

nonisolated func requirementCalls(_ drainer: some Drainable, _ any: any Drainable,
                      _ plain: PlainDrainer) {
  send drainer.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}
  send any.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}
  // The witness itself is not '@oneway(never)'
  send plain.drain()
}

@MainActor class Base {
  @oneway(never) func close() {}
}

// An override of a '@oneway(never)' method is treated as '@oneway(never)'
class Derived: Base {
  override func close() {}
}

func overrideCalls(_ derived: Derived) {
  // No fix-it in a synchronous context, which cannot 'await'
  send derived.close() // expected-warning{{'@oneway(never)' instance method 'close()' is not intended to be called with 'send', use 'await' instead}}{{none}}
}

// A call through a function value is not checked, since the attribute does
// not affect the function type; 'send' cannot call a function value anyway
@MainActor func functionValueCalls() {
  let fn: () -> Void = flush
  send fn() // expected-error{{'send' can only be applied to calls of actor isolated methods}}
}

// '@diagnose' can suppress the warning
@diagnose(SentOnewayNeverCall, as: ignored)
func ignored() {
  send flush()
}

// ... or turn it into an error
@diagnose(SentOnewayNeverCall, as: error)
func escalated() {
  send flush() // expected-error{{'@oneway(never)' global function 'flush()' is not intended to be called with 'send', use 'await' instead}}
}
