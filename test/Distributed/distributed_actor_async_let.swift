// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

func test_compute(_ v: Int) async -> Int { v }

distributed actor Philosopher {
  typealias Transport = AnyActorTransport

  init(transport: AnyActorTransport) {
  }

  // `hi` is implicitly throwing
  distributed func hi() -> String { "Hi!" }

  distributed func compute() -> Int { 42 }

  func test() async throws {
    async let x = Philosopher(transport: self.actorTransport).hi() // Ok
    async let y = Philosopher(transport: self.actorTransport).hi() // Ok

    _ = try await y // Ok because `async let` is infer `throws` from distributed method
    _ = await x // expected-error {{reading 'async let' can throw but is not marked with 'try'}}

    async let z = test_compute(compute()) // Ok

    _ = try await z
    _ = await z // expected-error {{reading 'async let' can throw but is not marked with 'try'}}
  }
}
