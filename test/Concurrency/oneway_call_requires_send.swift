// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// ==== -----------------------------------------------------------------------
// MARK: 'send' can be applied to any Void, non-throwing call

actor Counter {
  func increment() {}
}

func testGeneralSend(_ c: Counter) {
  // A call of a method which is not '@oneway'
  send c.increment()
}

// ==== -----------------------------------------------------------------------
// MARK: Calls of '@oneway' plain actor methods from within the actor

actor Worker {
  @oneway func heartbeat() {}
  @oneway func tick() async {}

  func selfCalls() async {
    // Accepted: wrapped in 'send'
    send self.heartbeat()
    send self.tick()

    // Accepted: a call from within the actor's own isolation domain is an
    // ordinary call, plain or awaited, which does not wait for another actor
    self.heartbeat()
    await self.tick()

    // expected-warning@+1{{no 'async' operations occur within 'await' expression}}
    _ = await self.heartbeat()
  }

  // A reference to a '@oneway' method is an ordinary function value, so
  // calls of it are not diagnosed. Its isolation to 'self' is not part of its
  // type, so it cannot be sent
  func functionValues() async {
    let heartbeat = self.heartbeat
    send heartbeat() // expected-error@:5{{'send' can only be applied to calls of actor isolated methods}}
    heartbeat()
  }
}
