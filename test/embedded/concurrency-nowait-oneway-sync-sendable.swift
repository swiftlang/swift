// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library %s

// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnewayNowait

// In Embedded Swift 'nowait' on a synchronous 'oneway' method is lowered to a
// synchronous enqueue of a closure which captures the arguments. The region
// checker (SE-0414) must still diagnose the arguments exactly like it does for
// an 'await' of the same method: a non-Sendable value that is still used
// afterwards, or that belongs to the caller's region, cannot be sent, and a
// fresh disconnected value can. A call on the caller's own actor is not a send

import _Concurrency

class NonSendable {
  var x = 0
}

actor A {
  var state = NonSendable()

  func take(_ n: NonSendable) oneway {}
  func takeAsync(_ n: NonSendable) async {}

  // Same-actor calls: passing the actor's own state is fine
  func sendOwnState() {
    nowait self.take(state)
    nowait take(state)
    state.x += 1
  }

  // A different actor: the actor's state cannot be sent to it
  func sendOwnStateToOther(_ other: A) {
    nowait other.take(state) // expected-error{{sending 'self.state' risks causing data races}}
    // expected-note@-1{{'self'-isolated 'self.state' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later actor-isolated uses}}
  }
}

@globalActor actor GA {
  static let shared = GA()
}

@GA var globalState = NonSendable()

@GA func globalTake(_ n: NonSendable) oneway {}

@GA func sendGlobalStateOnSameGlobalActor() {
  nowait globalTake(globalState)
  globalState.x += 1
}

// ==== -----------------------------------------------------------------------
// MARK: Plain actor

func useAfterSend(_ a: A) {
  let ns = NonSendable()
  nowait a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

// The same diagnostic for an 'await' of an async method, for comparison
func useAfterAwait(_ a: A) async {
  let ns = NonSendable()
  await a.takeAsync(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{sending 'ns' to actor-isolated instance method 'takeAsync' risks causing data races between actor-isolated and local nonisolated uses}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

func sendParameter(_ a: A, _ ns: NonSendable) {
  nowait a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against code in the current isolation context}}
}

func sendFresh(_ a: A) {
  nowait a.take(NonSendable())
}

func sendDisconnected(_ a: A) {
  let ns = NonSendable()
  ns.x = 1
  nowait a.take(ns)
}

// The caller is isolated to 'a', so this is a same-actor call
func sendOnIsolatedParameter(_ a: isolated A, _ ns: NonSendable) {
  nowait a.take(ns)
  print(ns.x)
}

@GA func sendGlobalActorState(_ a: A) {
  nowait a.take(globalState) // expected-error{{sending 'globalState' risks causing data races}}
  // expected-note@-1{{global actor 'GA'-isolated 'globalState' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later global actor 'GA'-isolated uses}}
}

// ==== -----------------------------------------------------------------------
// MARK: Global actor

func globalUseAfterSend() {
  let ns = NonSendable()
  nowait globalTake(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a global actor 'GA'-isolated closure. global actor 'GA'-isolated uses in closure may race against later nonisolated uses}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

func globalSendParameter(_ ns: NonSendable) {
  nowait globalTake(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a global actor 'GA'-isolated closure. global actor 'GA'-isolated uses in closure may race against code in the current isolation context}}
}

func globalSendFresh() {
  nowait globalTake(NonSendable())
}
