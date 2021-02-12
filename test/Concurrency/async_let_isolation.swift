// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor MyActor {
  let immutable: Int = 17
  var text: [String] = []

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 2 {{calls to instance method 'synchronous()' from outside of its actor context are implicitly asynchronous}}
  func asynchronous() async -> String { synchronous() }

  func testAsyncLetIsolation() async {
    async let x = self.synchronous()
    // expected-error @-1{{actor-isolated instance method 'synchronous()' cannot be referenced from 'async let' initializer}}

    async let y = await self.asynchronous()

    async let z = synchronous()
    // expected-error @-1{{actor-isolated instance method 'synchronous()' cannot be referenced from 'async let' initializer}}

    var localText = text
    async let w = localText.removeLast()
    // expected-error@-1{{mutation of captured var 'localText' in concurrently-executing code}}

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
