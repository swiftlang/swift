// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

actor MyActor {
  let immutable: Int = 17
  var text: [String] = []

  func synchronous() -> String { text.first ?? "nothing" }
  func asynchronous() async -> String { synchronous() }

  func testAsyncLetIsolation() async {
    async let x = self.synchronous()

    async let y = await self.asynchronous()

    async let z = synchronous()

    var localText = text
    async let w = localText.removeLast() // expected-error{{mutation of captured var 'localText' in concurrently-executing code}}

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
