// RUN: %target-swift-frontend %s -swift-version 6 -verify -verify-additional-prefix disabled- -c
// RUN: %target-swift-frontend %s -swift-version 6 -verify -enable-upcoming-feature NonisolatedNonsendingByDefault -verify-additional-prefix enable- -c -verify-additional-prefix enabled-

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// This test checks and validates that when NonisolatedNonsendingByDefault is enabled, we emit the
// appropriate diagnostics. It also runs with the mode off so we can validate
// and compare locally against the normal errors.

//////////////////
// Declarations //
//////////////////

class NonSendableKlass {
  func unspecifiedCaller() async {}
  nonisolated func nonisolatedCaller() async {}
  nonisolated(nonsending) func nonisolatedNonSendingCaller() async {}
  @concurrent func concurrentCaller() async {}
}

nonisolated class NonIsolatedNonSendableKlass {
  func unspecifiedMethod() async {}
  nonisolated func nonisolatedMethod() async {}
}

func unspecifiedSyncUse<T>(_ t: T) {}
func unspecifiedAsyncUse<T>(_ t: T) async {}
nonisolated func nonisolatedSyncUse<T>(_ t: T) {}
nonisolated func nonisolatedAsyncUse<T>(_ t: T) async {}
nonisolated(nonsending) func nonisolatedNonSendingAsyncUse<T>(_ t: T) async {}
@concurrent func concurrentAsyncUse<T>(_ t: T) async {}

func unspecifiedSyncUseWithResult<T>(_ t: T) -> T { t }
func unspecifiedAsyncUseWithResult<T>(_ t: T) async -> T { t }
nonisolated func nonisolatedSyncUseWithResult<T>(_ t: T) -> T { t }
nonisolated func nonisolatedAsyncUseWithResult<T>(_ t: T) async -> T { t }

func unspecifiedSyncResult() -> NonSendableKlass { fatalError() }
func unspecifiedAsyncResult() async -> NonSendableKlass { fatalError() }
nonisolated func nonisolatedSyncResult() -> NonSendableKlass { fatalError() }
nonisolated func nonisolatedAsyncResult() async -> NonSendableKlass { fatalError() }
func sendingParameter<T>(_ t: sending T) async {}
func useValue<T>(_ t: T) {}

@MainActor func sendToMain<T>(_ t: T) async {}

actor Custom {
}

@globalActor
struct CustomActor {
  static var shared: Custom {
    return Custom()
  }
}

@CustomActor func sendToCustom<T>(_ t: T) async {}

@MainActor
final class MainActorKlass {
  var ns = NonSendableKlass()

  func useValueAsync(_ x: NonSendableKlass) async {}
}

///////////
// Tests //
///////////

actor ActorTest {
  var ns = NonSendableKlass()

  func callNonIsolatedWithParam() async {
    unspecifiedSyncUse(ns)
    await unspecifiedAsyncUse(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'unspecifiedAsyncUse' risks causing data races between nonisolated and 'self'-isolated uses}}
    nonisolatedSyncUse(ns)
    await nonisolatedAsyncUse(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'nonisolatedAsyncUse' risks causing data races between nonisolated and 'self'-isolated uses}}
  }

  func callNonIsolatedWithResult() async {
    let x1 = unspecifiedSyncUseWithResult(ns)
    await sendToMain(x1) // expected-error {{sending 'x1' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'x1' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}

    let x2 = await unspecifiedAsyncUseWithResult(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'unspecifiedAsyncUseWithResult' risks causing data races between nonisolated and 'self'-isolated uses}}
    await sendToMain(x2) // expected-enabled-error {{sending 'x2' risks causing data races}}
    // expected-enabled-note @-1 {{sending 'self'-isolated 'x2' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}

    let x3 = nonisolatedSyncUseWithResult(ns)
    await sendToMain(x3) // expected-error {{sending 'x3' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'x3' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}

    let x4 = await nonisolatedAsyncUseWithResult(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'nonisolatedAsyncUseWithResult' risks causing data races between nonisolated and 'self'-isolated uses}}
    await sendToMain(x4) // expected-enabled-error {{sending 'x4' risks causing data races}}
    // expected-enabled-note @-1 {{sending 'self'-isolated 'x4' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }

  func callNonIsolatedWithResult2() async {
    let x1 = unspecifiedSyncResult()
    await sendToMain(x1)

    let x2 = await unspecifiedAsyncResult()
    await sendToMain(x2)

    let x3 = nonisolatedSyncResult()
    await sendToMain(x3)

    let x4 = await nonisolatedAsyncResult()
    await sendToMain(x4)
  }
}

extension MainActorKlass {
  func callNonIsolatedWithParam() async {
    unspecifiedSyncUse(ns)
    await unspecifiedAsyncUse(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending main actor-isolated 'self.ns' to nonisolated global function 'unspecifiedAsyncUse' risks causing data races between nonisolated and main actor-isolated uses}}

    nonisolatedSyncUse(ns)
    await nonisolatedAsyncUse(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending main actor-isolated 'self.ns' to nonisolated global function 'nonisolatedAsyncUse' risks causing data races between nonisolated and main actor-isolated uses}}
  }

  func callNonIsolatedWithResult() async {
    let x1 = unspecifiedSyncUseWithResult(ns)
    await sendToCustom(x1) // expected-error {{sending 'x1' risks causing data races}}
    // expected-note @-1 {{sending main actor-isolated 'x1' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}

    let x2 = await unspecifiedAsyncUseWithResult(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending main actor-isolated 'self.ns' to nonisolated global function 'unspecifiedAsyncUseWithResult' risks causing data races between nonisolated and main actor-isolated uses}}
    await sendToCustom(x2) // expected-enabled-error {{sending 'x2' risks causing data races}}
    // expected-enabled-note @-1 {{sending main actor-isolated 'x2' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}

    let x3 = nonisolatedSyncUseWithResult(ns)
    await sendToCustom(x3) // expected-error {{sending 'x3' risks causing data races}}
    // expected-note @-1 {{sending main actor-isolated 'x3' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}

    let x4 = await nonisolatedAsyncUseWithResult(ns) // expected-disabled-error {{sending 'self.ns' risks causing data races}}
    // expected-disabled-note @-1 {{sending main actor-isolated 'self.ns' to nonisolated global function 'nonisolatedAsyncUseWithResult' risks causing data races between nonisolated and main actor-isolated uses}}
    await sendToCustom(x4) // expected-enabled-error {{sending 'x4' risks causing data races}}
    // expected-enabled-note @-1 {{sending main actor-isolated 'x4' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
  }

  func callNonIsolatedWithResult2() async {
    let x1 = unspecifiedSyncResult()
    await sendToCustom(x1)

    let x2 = await unspecifiedAsyncResult()
    await sendToCustom(x2)

    let x3 = nonisolatedSyncResult()
    await sendToCustom(x3)

    let x4 = await nonisolatedAsyncResult()
    await sendToCustom(x4)
  }
}

// We should not error on either of these since c is in the main actor's region
// and our nonisolated/unspecified methods are inheriting the main actor
// isolation which is safe since they are type checked as something that cannot
// access any state that is outside of the current actor that c is reachable from.
@MainActor
func validateNonisolatedOnClassMeansCallerIsolationInheritingOnFuncDecl(
  c: NonIsolatedNonSendableKlass
) async {
  await c.unspecifiedMethod() // expected-disabled-error {{sending 'c' risks causing data races}}
  // expected-disabled-note @-1 {{sending main actor-isolated 'c' to nonisolated instance method 'unspecifiedMethod()' risks causing data races between nonisolated and main actor-isolated uses}}
  await c.nonisolatedMethod() // expected-disabled-error {{sending 'c' risks causing data races}}
  // expected-disabled-note @-1 {{sending main actor-isolated 'c' to nonisolated instance method 'nonisolatedMethod()' risks causing data races between nonisolated and main actor-isolated uses}}
}

// Shouldn't get an error here since we are not using after we send to main.
func nonisolatedCallingNonIsolated() async {
  let c = NonSendableKlass()

  await unspecifiedAsyncUse(c)
  await unspecifiedAsyncUse(c)

  await sendToMain(c)
}

func nonisolatedCallingNonIsolated2() async {
  let c = NonSendableKlass()

  await unspecifiedAsyncUse(c)
  await unspecifiedAsyncUse(c)

  await sendToMain(c) // expected-error {{sending 'c' risks causing data races}}
  // expected-note @-1 {{sending 'c' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  await unspecifiedAsyncUse(c) // expected-note {{access can happen concurrently}}
}

// We should emit an error here despite the function context being @MainActor.
@MainActor
func sendingWithMainActor() async {
  let c = NonSendableKlass()
  await sendingParameter(c) // expected-error {{sending 'c' risks causing data races}}
  // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
  useValue(c) // expected-note {{access can happen concurrently}}
}

// We should emit an error here despite the function context have an explicit
// isolated parameter.
func sendingWithIsolatedParam(_ a: isolated Optional<Actor>) async {
  let c = NonSendableKlass()
  await sendingParameter(c) // expected-error {{sending 'c' risks causing data races}}
  // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
  useValue(c) // expected-note {{access can happen concurrently}}
}

// This errors only when disabled since the first time we call
// unspecifiedAsyncUse we have a disconnected value and the second time we have
// a value in a main actor isolated region and the unspecifiedAsyncUse runs on
// the main actor.
//
// TODO: This doesn't error if we have a non-final class because of a known bug
// where we infer isolation wrong for non-final class setters. That is why
// MainActorKlass is made final so we can test this appropriately.
@MainActor
func testUnrolledLoop(_ a: MainActorKlass) async {
  let k = NonSendableKlass()
  await unspecifiedAsyncUse(k)
  a.ns = k
  await unspecifiedAsyncUse(k) // expected-disabled-error {{sending 'k' risks causing data races}}
  // expected-disabled-note @-1 {{sending main actor-isolated 'k' to nonisolated global function 'unspecifiedAsyncUse' risks causing data races between nonisolated and main actor-isolated uses}}
}

// We emit an error in both modes since we are now in an @MainActor isolated
// function.
func testUnrolledLoop2(_ a: MainActorKlass) async {
  let k = NonSendableKlass()
  await unspecifiedAsyncUse(k)
  await a.useValueAsync(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to main actor-isolated instance method 'useValueAsync' risks causing data races between main actor-isolated and local nonisolated uses}}
  await unspecifiedAsyncUse(k) // expected-note {{access can happen concurrently}}
}

func testUnrolledLoopWithAsyncLet(_ a: MainActorKlass) async {
  let k = NonSendableKlass()
  await unspecifiedAsyncUse(k)
  // This is valid since our valid is disconnected here.
  async let value: () = await unspecifiedAsyncUse(k)
  _ = await value

  await a.useValueAsync(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to main actor-isolated instance method 'useValueAsync' risks causing data races between main actor-isolated and local nonisolated uses}}
  async let value2: () = await unspecifiedAsyncUse(k) // expected-note {{access can happen concurrently}}
  _ = await value2
}

@MainActor
func testUnrolledLoopWithAsyncLet2(_ a: MainActorKlass) async {
  let k = NonSendableKlass()
  await unspecifiedAsyncUse(k)
  // This is valid since our valid is disconnected here.
  async let value: () = await unspecifiedAsyncUse(k)
  _ = await value

  await a.useValueAsync(k)
  async let value2: () = await unspecifiedAsyncUse(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'k' into async let risks causing data races between nonisolated and main actor-isolated uses}}
  _ = await value2
}

@MainActor
func testUnrolledLoopWithAsyncLet3(_ a: MainActorKlass) async {
  let k = NonSendableKlass()
  await unspecifiedAsyncUse(k)
  // This is valid since our valid is disconnected here.
  async let value: () = await unspecifiedAsyncUse(k)
  _ = await value

  await a.useValueAsync(k)
  async let value2: () = await sendToMain(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'k' into async let risks causing data races between nonisolated and main actor-isolated uses}}
  _ = await value2
}

func unspecifiedCallingVariousNonisolated(_ x: NonSendableKlass) async {
  await x.unspecifiedCaller()
  await x.nonisolatedCaller()
  await x.nonisolatedNonSendingCaller()
  await x.concurrentCaller() // expected-enabled-error {{sending 'x' risks causing data races}}
  // expected-enabled-note @-1 {{sending task-isolated 'x' to nonisolated instance method 'concurrentCaller()' risks causing data races between nonisolated and task-isolated uses}}

  await unspecifiedAsyncUse(x)
  await nonisolatedAsyncUse(x)
  await nonisolatedNonSendingAsyncUse(x)
  await concurrentAsyncUse(x) // expected-enabled-error {{sending 'x' risks causing data races}}
  // expected-enabled-note @-1 {{sending task-isolated 'x' to nonisolated global function 'concurrentAsyncUse' risks causing data races between nonisolated and task-isolated uses}}
}

nonisolated func nonisolatedCallingVariousNonisolated(_ x: NonSendableKlass) async {
  await x.unspecifiedCaller()
  await x.nonisolatedCaller()
  await x.nonisolatedNonSendingCaller()
  await x.concurrentCaller() // expected-enabled-error {{sending 'x' risks causing data races}}
  // expected-enabled-note @-1 {{sending task-isolated 'x' to nonisolated instance method 'concurrentCaller()' risks causing data races between nonisolated and task-isolated uses}}

  await unspecifiedAsyncUse(x)
  await nonisolatedAsyncUse(x)
  await nonisolatedNonSendingAsyncUse(x)
  await concurrentAsyncUse(x) // expected-enabled-error {{sending 'x' risks causing data races}}
  // expected-enabled-note @-1 {{sending task-isolated 'x' to nonisolated global function 'concurrentAsyncUse' risks causing data races between nonisolated and task-isolated uses}}
}

nonisolated(nonsending) func nonisolatedNonSendingCallingVariousNonisolated(_ x: NonSendableKlass) async {
  await x.unspecifiedCaller() // expected-disabled-error {{sending 'x' risks causing data races}}
  // expected-disabled-note @-1 {{sending task-isolated 'x' to nonisolated instance method 'unspecifiedCaller()' risks causing data races between nonisolated and task-isolated uses}}
  await x.nonisolatedCaller() // expected-disabled-error {{sending 'x' risks causing data races}}
  // expected-disabled-note @-1 {{sending task-isolated 'x' to nonisolated instance method 'nonisolatedCaller()' risks causing data races between nonisolated and task-isolated uses}}
  await x.nonisolatedNonSendingCaller()
  await x.concurrentCaller() // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'x' to nonisolated instance method 'concurrentCaller()' risks causing data races between nonisolated and task-isolated uses}}

  await unspecifiedAsyncUse(x) // expected-disabled-error {{sending 'x' risks causing data races}}
  // expected-disabled-note @-1 {{sending task-isolated 'x' to nonisolated global function 'unspecifiedAsyncUse' risks causing data races between nonisolated and task-isolated uses}}
  await nonisolatedAsyncUse(x) // expected-disabled-error {{sending 'x' risks causing data races}}
  // expected-disabled-note @-1 {{sending task-isolated 'x' to nonisolated global function 'nonisolatedAsyncUse' risks causing data races between nonisolated and task-isolated uses}}
  await nonisolatedNonSendingAsyncUse(x)
  await concurrentAsyncUse(x) // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'x' to nonisolated global function 'concurrentAsyncUse' risks causing data races between nonisolated and task-isolated uses}}
}

@concurrent func concurrentCallingVariousNonisolated(_ x: NonSendableKlass) async {
  await x.unspecifiedCaller()
  await x.nonisolatedCaller()
  await x.nonisolatedNonSendingCaller()
  await x.concurrentCaller()

  await unspecifiedAsyncUse(x)
  await nonisolatedAsyncUse(x)
  await nonisolatedNonSendingAsyncUse(x)
  await concurrentAsyncUse(x)
}
