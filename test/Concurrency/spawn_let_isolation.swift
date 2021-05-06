// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor MyActor {
  let immutable: Int = 17
  var text: [String] = []

  func synchronous() -> String { text.first ?? "nothing" }
  func asynchronous() async -> String { synchronous() }

  func testAsyncLetIsolation() async {
    spawn let x = self.synchronous()

    spawn let y = await self.asynchronous()

    spawn let z = synchronous()

    var localText = text
    spawn let w = localText.removeLast() // expected-error{{mutation of captured var 'localText' in concurrently-executing code}}

    _ = await x
    _ = await y
    _ = await z
    _ = await w
  }
}

func outside() async {
  let a = MyActor()
  spawn let x = a.synchronous() // okay, await is implicit
  spawn let y = await a.synchronous()
  _ = await x
  _ = await y
}
