// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA {

  let local: Int = 42
  // expected-note@-1{{distributed actor state is only available within the actor instance}}
  // expected-note@-2{{distributed actor state is only available within the actor instance}}

  nonisolated let nope: Int = 13
  // expected-error@-1{{'nonisolated' can not be applied to distributed actor stored properties}}

  nonisolated var computedNonisolated: Int {
    // expected-note@-1{{distributed actor state is only available within the actor instance}}

    // nonisolated computed properties are outside of the actor and as such cannot access local
    _ = self.local // expected-error{{distributed actor-isolated property 'local' can only be referenced inside the distributed actor}}

    _ = self.actorAddress // ok, special handled and always available
    _ = self.actorTransport // ok, special handled and always available
  }

  distributed func dist() {}

  nonisolated func access() async throws {
    _ = self.actorAddress // ok
    _ = self.actorTransport // ok
    
    // self is a distributed actor self is NOT isolated
    _ = self.local // expected-error{{distributed actor-isolated property 'local' can only be referenced inside the distributed actor}}
    _ = try await self.dist() // ok, was made implicitly throwing and async
    _ = self.computedNonisolated // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  }

  nonisolated distributed func nonisolatedDistributed() async {
    // expected-error@-1{{function 'nonisolatedDistributed()' cannot be both 'nonisolated' and 'distributed'}}{{3-15=}}
    fatalError()
  }

  distributed nonisolated func distributedNonisolated() async {
    // expected-error@-1{{function 'distributedNonisolated()' cannot be both 'nonisolated' and 'distributed'}}{{15-27=}}
    fatalError()
  }

}