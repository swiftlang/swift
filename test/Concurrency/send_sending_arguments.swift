// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -parse-as-library %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// The arguments of a 'send' call are sent to the callee while the caller
// continues, like 'sending' arguments: a non-Sendable value may not be used
// after it was sent, and a value which is not disconnected may not be sent

class NonSendable {
  var x = 0
}

actor A {
  @oneway func take(_ ns: NonSendable) {}
  @oneway func takeAsync(_ ns: NonSendable) async {}
  @oneway func takeInt(_ i: Int) {}
}

func sendDisconnected(_ a: A) {
  let ns = NonSendable()
  send a.take(ns)
}

func useAfterSend(_ a: A) {
  let ns = NonSendable()
  send a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

func sendTwice(_ a: A) {
  let ns = NonSendable()
  send a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
  send a.take(ns) // expected-note{{access can happen concurrently}}
}

func sendTaskIsolatedParameter(_ a: A, _ ns: NonSendable) {
  send a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against code in the current isolation context}}
}

// 'send' of an async '@oneway' call
func useAfterGeneralSend(_ a: A) {
  let ns = NonSendable()
  send a.takeAsync(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

func sendableArguments(_ a: A) {
  let i = 1
  send a.takeInt(i)
  print(i)
}

@MainActor
func sendMainActorIsolatedParameter(_ a: A, _ ns: NonSendable) {
  send a.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{main actor-isolated 'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later main actor-isolated uses}}
}

actor B {
  var state = NonSendable()

  @oneway func take(_ ns: NonSendable) {}

  // The task runs on this actor too, so this is a same-actor use
  func sendOwnStateToSelf() {
    send self.take(state)
    state.x += 1
  }

  func sendOwnState(_ a: A) {
    send a.take(state) // expected-error{{sending 'self.state' risks causing data races}}
    // expected-note@-1{{'self'-isolated 'self.state' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later actor-isolated uses}}
  }
}
