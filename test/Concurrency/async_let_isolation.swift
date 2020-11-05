// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor class MyActor {
  let immutable: Int = 17
  var text: [String] = []

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 2 {{only asynchronous methods can be used outside the actor instance; do you want to add 'async'?}}
  func asynchronous() async -> String { synchronous() }

  func testAsyncLetIsolation() async {
    async let x = self.synchronous()
    // expected-error @-1{{actor-isolated instance method 'synchronous()' is unsafe to reference in code that may execute concurrently}}

    async let y = await self.asynchronous()

    async let z = synchronous()
    // expected-error @-1{{actor-isolated instance method 'synchronous()' is unsafe to reference in code that may execute concurrently}}

    var localText = text // expected-note{{var declared here}}
    async let w = localText.removeLast()
    // expected-warning@-1{{local var 'localText' is unsafe to reference in code that may execute concurrently}}

    _ = await x
    _ = await y
    _ = await z
    _ = await w
  }
}
