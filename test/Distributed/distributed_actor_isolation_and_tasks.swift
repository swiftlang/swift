// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

struct SomeLogger {}
struct Logger {
  let label: String
  func info(_: String) {}
}

distributed actor Philosopher {
  let log: Logger
  // expected-note@-1{{distributed actor state is only available within the actor instance}}
  var variable = 12
  var variable_fromDetach = 12 // expected-note{{distributed actor state is only available within the actor instance}}
  let INITIALIZED: Int
  let outside: Int = 1

  init(transport: ActorTransport) {
    self.log = Logger(label: "name")
    self.INITIALIZED = 1
  }

  distributed func dist() -> Int {}

  func test() {
    _ = self.id
    _ = self.actorTransport
    Task {
      _ = self.id
      _ = self.actorTransport

      self.log.info("READY!")
      _ = self.variable
      _ = self.dist()
    }

    Task.detached {
      _ = self.id
      _ = self.actorTransport

      // This is an interesting case, since we have a real local `self` and
      // yet are not isolated to the same actor in this detached task...
      // the call to it is implicitly async, however it is NOT implicitly throwing
      // because we KNOW this is a local call -- and there is no transport in
      // between that will throw.
      _ = await self.dist() // notice lack of 'try' even though 'distributed func'
      _ = self.variable_fromDetach // expected-error{{distributed actor-isolated property 'variable_fromDetach' can only be referenced inside the distributed actor}}
    }
  }
}

func test_outside(transport: ActorTransport) async throws {
  _ = try await Philosopher(transport: transport).dist()
  _ = Philosopher(transport: transport).log // expected-error{{distributed actor-isolated property 'log' can only be referenced inside the distributed actor}}

  _ = Philosopher(transport: transport).id
  _ = Philosopher(transport: transport).actorTransport
}

func test_outside_isolated(phil: isolated Philosopher) async throws {
  phil.log.info("works on isolated")
}