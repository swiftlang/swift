// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -swift-version 6 -verify -verify-additional-prefix ni- %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -swift-version 6 -verify -verify-additional-prefix ni-ns- %s -o /dev/null -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// This test validates the behavior of transfernonsendable around initializers.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}
@MainActor func transferToMain<T>(_ t: T) async {}

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

/////////////////
// MARK: Tests //
/////////////////

actor ActorWithSynchronousNonIsolatedInit {
  let k: NonSendableKlass

  init(_ newK: NonSendableKlass) {
    k = NonSendableKlass()

    helper(newK)

    // TODO: This should say actor isolated.
    let _ = { @MainActor in
      print(newK) // expected-error {{sending 'newK' risks causing data races}}
      // expected-ni-note @-1 {{'self'-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
      // expected-ni-ns-note @-2 {{'self'-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  init(x newK: NonSendableKlass) {
    k = newK

    helper(newK)

    let _ = { @MainActor in
      print(newK) // expected-error {{sending 'newK' risks causing data races}}
      // expected-note @-1 {{'self'-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  init(ns: NonSendableKlass) async {
    self.k = NonSendableKlass()
    self.isolatedHelper(ns)
  }

  nonisolated func helper(_ newK: NonSendableKlass) {}
  func isolatedHelper(_ newK: NonSendableKlass) {}
}

func initActorWithSyncNonIsolatedInit() {
  let k = NonSendableKlass()
  _ = ActorWithSynchronousNonIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to actor initializer 'init(_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    print(k)
  }
}

func initActorWithSyncNonIsolatedInit2(_ k: NonSendableKlass) {
  _ = ActorWithSynchronousNonIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'k' to actor initializer 'init(_:)' risks causing data races between actor-isolated and task-isolated uses}}
  let _ = { @MainActor in
    print(k) // expected-error {{sending 'k' risks causing data races}}
    // expected-note @-1 {{task-isolated 'k' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
}

actor ActorWithAsyncIsolatedInit {
  init(_ newK: NonSendableKlass) async {
    let _ = { @MainActor in
      print(newK) // expected-error {{sending 'newK' risks causing data races}}
      // expected-note @-1 {{'self'-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later actor-isolated uses}}
    }
  }
}

func initActorWithAsyncIsolatedInit() async {
  let k = NonSendableKlass()
  _ = await ActorWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    print(k)
  }
}

func initActorWithAsyncIsolatedInit2(_ k: NonSendableKlass) async {
  _ = await ActorWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'k' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and task-isolated uses}}
  let _ = { @MainActor in
    print(k) // expected-error {{sending 'k' risks causing data races}}
    // expected-note @-1 {{task-isolated 'k' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
}

// Test with multiple non-sendable arguments.
actor ActorWithMultiArgAsyncInit {
  init(_ a: NonSendableKlass, _ b: NonSendableKlass) async {}
}

func initActorWithMultiArgAsyncInit() async {
  let a = NonSendableKlass()
  let b = NonSendableKlass()
  _ = await ActorWithMultiArgAsyncInit(a, b) // expected-error {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to actor-isolated initializer 'init(_:_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  // expected-error @-2 {{sending 'b' risks causing data races}}
  // expected-note @-3 {{sending 'b' to actor-isolated initializer 'init(_:_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    // expected-note @-1 {{access can happen concurrently}}
    print(a)
    print(b)
  }
}

// Test that sending the same value to two different actor async inits
// produces errors for both.
func initActorWithAsyncIsolatedInitTwice() async {
  let k = NonSendableKlass()
  _ = await ActorWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  _ = await ActorWithAsyncIsolatedInit(k) // expected-note {{access can happen concurrently}}
}

// Test use-after-send with actor async init.
func useAfterSendToActorAsyncInit() async {
  let k = NonSendableKlass()
  _ = await ActorWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  print(k) // expected-note {{access can happen concurrently}}
}

// Test explicit .init(_:) call syntax.
func initActorWithExplicitInitSyntax() async {
  let k = NonSendableKlass()
  _ = await ActorWithAsyncIsolatedInit.init(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    print(k)
  }
}

// TODO: The error should be on the use site (f(k)), not the function_ref.
// Test higher-order: assign init to variable, then call.
func initActorHigherOrderAssignAndCall() async {
  let f = ActorWithAsyncIsolatedInit.init // expected-error {{sending 'newK' risks causing data races}}
  // expected-ni-note @-1 {{sending task-isolated 'newK' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and task-isolated uses}}
  // expected-ni-ns-note @-2 {{sending @concurrent task-isolated 'newK' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and @concurrent task-isolated uses}}
  let k = NonSendableKlass()
  _ = await f(k)
}

// TODO: The error should be on the use site, not the function_ref.
// Test higher-order: pass init ref as argument.
func callWithInit(_ f: (NonSendableKlass) async -> ActorWithAsyncIsolatedInit) async {
  let k = NonSendableKlass()
  _ = await f(k)
}

func initActorHigherOrderPassAsArg() async {
  await callWithInit(ActorWithAsyncIsolatedInit.init) // expected-error {{sending 'newK' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'newK' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and task-isolated uses}}
}

////////////////////////////////
// MARK: Actor-Isolated Class //
////////////////////////////////

@CustomActor
class ClassWithSynchronousNonIsolatedInit {
  nonisolated init(_ newK: NonSendableKlass) {
    // We do not error on this since helper is nonisolated and newK is
    // considered task isolated.
    helper(newK)

    let _ = { @MainActor in
      print(newK) // expected-error {{sending 'newK' risks causing data races}}
      // expected-note @-1 {{task-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  nonisolated func helper(_ newK: NonSendableKlass) {}
}

func initClassWithSyncNonIsolatedInit() {
  let k = NonSendableKlass()
  _ = ClassWithSynchronousNonIsolatedInit(k)
  let _ = { @MainActor in
    print(k)
  }
}

func initClassWithSyncNonIsolatedInit2(_ k: NonSendableKlass) {
  _ = ClassWithSynchronousNonIsolatedInit(k)
  let _ = { @MainActor in
    print(k) // expected-error {{sending 'k' risks causing data races}}
    // expected-note @-1 {{task-isolated 'k' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
}

@CustomActor
class ClassWithAsyncIsolatedInit {
  init(_ newK: NonSendableKlass) async {
    let _ = { @MainActor in
      print(newK) // expected-error {{sending 'newK' risks causing data races}}
      // expected-note @-1 {{global actor 'CustomActor'-isolated 'newK' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later global actor 'CustomActor'-isolated uses}}
    }
  }
}

func initClassWithAsyncIsolatedInit() async {
  let k = NonSendableKlass()
  // TODO: Might make sense to emit a more specific error here since the closure
  // is MainActor isolated. The actual capture is initially not isolated to
  // MainActor.
  _ = await ClassWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to global actor 'CustomActor'-isolated initializer 'init(_:)' risks causing data races between global actor 'CustomActor'-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    print(k)
  }
}

func initClassWithAsyncIsolatedInit2(_ k: NonSendableKlass) async {
  _ = await ClassWithAsyncIsolatedInit(k) // expected-error {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'k' to global actor 'CustomActor'-isolated initializer 'init(_:)' risks causing data races between global actor 'CustomActor'-isolated and task-isolated uses}}
  let _ = { @MainActor in
    print(k) // expected-error {{sending 'k' risks causing data races}}
    // expected-note @-1 {{task-isolated 'k' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
}
