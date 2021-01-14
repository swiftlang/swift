// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor class MyActor {
  let immutable: Int = 17
  var text: [String] = []

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 2 {{calls to instance method 'synchronous()' from outside of its actor context are implicitly asynchronous}}
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

func outside() async {
  let a = MyActor()
  async let x = a.synchronous() // okay, await is implicit
  async let y = await a.synchronous()
  _ = await x
  _ = await y
}
