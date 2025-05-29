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

class NonSendableKlass {}

nonisolated class NonIsolatedNonSendableKlass {
  func unspecifiedMethod() async {}
  nonisolated func nonisolatedMethod() async {}
}

func unspecifiedSyncUse<T>(_ t: T) {}
func unspecifiedAsyncUse<T>(_ t: T) async {}
nonisolated func nonisolatedSyncUse<T>(_ t: T) {}
nonisolated func nonisolatedAsyncUse<T>(_ t: T) async {}

func unspecifiedSyncUseWithResult<T>(_ t: T) -> T { t }
func unspecifiedAsyncUseWithResult<T>(_ t: T) async -> T { t }
nonisolated func nonisolatedSyncUseWithResult<T>(_ t: T) -> T { t }
nonisolated func nonisolatedAsyncUseWithResult<T>(_ t: T) async -> T { t }

func unspecifiedSyncResult() -> NonSendableKlass { fatalError() }
func unspecifiedAsyncResult() async -> NonSendableKlass { fatalError() }
nonisolated func nonisolatedSyncResult() -> NonSendableKlass { fatalError() }
nonisolated func nonisolatedAsyncResult() async -> NonSendableKlass { fatalError() }

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
    await sendToMain(x2) // expected-enabled-error {{sending 'x2' risks causing data races}}
    // expected-enabled-note @-1 {{sending 'self'-isolated 'x2' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}

    let x3 = nonisolatedSyncResult()
    await sendToMain(x3)

    let x4 = await nonisolatedAsyncResult()
    await sendToMain(x4) // expected-enabled-error {{sending 'x4' risks causing data races}}
    // expected-enabled-note @-1 {{sending 'self'-isolated 'x4' to main actor-isolated global function 'sendToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}

@MainActor
class MainActorKlass {
  var ns = NonSendableKlass()

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
    await sendToCustom(x2) // expected-enabled-error {{sending 'x2' risks causing data races}}
    // expected-enabled-note @-1 {{sending main actor-isolated 'x2' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}

    let x3 = nonisolatedSyncResult()
    await sendToCustom(x3)

    let x4 = await nonisolatedAsyncResult()
    await sendToCustom(x4) // expected-enabled-error {{sending 'x4' risks causing data races}}
    // expected-enabled-note @-1 {{sending main actor-isolated 'x4' to global actor 'CustomActor'-isolated global function 'sendToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
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
