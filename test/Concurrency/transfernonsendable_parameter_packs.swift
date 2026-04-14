// RUN: %target-swift-frontend -swift-version 5 -strict-concurrency=complete -target %target-swift-5.9-abi-triple -parse-as-library -emit-sil -o /dev/null -verify -verify-additional-prefix swift5- %s
// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.9-abi-triple -parse-as-library -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- %s

// REQUIRES: concurrency

// https://github.com/apple/swift/issues/74688
// Parameter packs should not interfere with region-based isolation.

actor PackActor<each T> {
  var messages : [(repeat each T)] = []

  func message(_ num : repeat each T) {
    messages.append((repeat each num))
  }
}

func test_parameter_pack_actor_isolation_sendable() async {
  let a = PackActor<Int>()
  let value = 10

  await a.message(value)
}

class NS {
  var n = 0
}

func test_parameter_pack_actor_isolation_nonsendable() async {
  let a = PackActor<NS>()
  let value = NS()

  await a.message(value)
}

func test_parameter_pack_actor_multi_sendable() async {
  let a = PackActor<Int, String, Double>()

  await a.message(1, "hello", 3.14)
}

func test_parameter_pack_actor_multi_nonsendable() async {
  let a = PackActor<NS, NS>()
  let p = NS()
  let q = NS()

  await a.message(p, q)
}

func test_parameter_pack_actor_mixed() async {
  let a = PackActor<Int, NS, String>()
  let ns = NS()

  await a.message(1, ns, "hello")
}

func test_parameter_pack_actor_nonsendable_not_disconnected() async {
  let a = PackActor<NS>()
  let value = NS()

  // TODO: https://github.com/swiftlang/swift/issues/88417
  // The transfer non sendable pass should be updated to be able to find the name of packs.
  await a.message(value) // expected-swift5-warning {{sending value of non-Sendable type 'NS' risks causing data races}}
  // expected-swift6-error @-1 {{sending value of non-Sendable type 'NS' risks causing data races}}
  // expected-note @-2 {{sending value of non-Sendable type 'NS' to actor-isolated instance method 'message' risks causing data races between actor-isolated and local nonisolated uses}}

  print(value) // expected-note {{access can happen concurrently}}
}

func test_parameter_pack_actor_sendable_not_disconnected() async {
  let a = PackActor<Int>()
  let value = 10

  await a.message(value)

  print(value)
}

nonisolated(nonsending) func test_parameter_pack_actor_nonsending_nonsendable_not_disconnected() async {
  let a = PackActor<NS>()
  let value = NS()

  // TODO: https://github.com/swiftlang/swift/issues/88417
  // The transfer non sendable pass should avoid saying "local caller isolation inheriting-isolated".
  await a.message(value) // expected-swift5-warning {{sending value of non-Sendable type 'NS' risks causing data races}}
  // expected-swift6-error @-1 {{sending value of non-Sendable type 'NS' risks causing data races}}
  // expected-note @-2 {{sending value of non-Sendable type 'NS' to actor-isolated instance method 'message' risks causing data races between actor-isolated and local caller isolation inheriting-isolated uses}}

  print(value) // expected-note {{access can happen concurrently}}
}

nonisolated(nonsending) func test_parameter_pack_actor_nonsending_sendable_not_disconnected() async {
  let a = PackActor<Int>()
  let value = 10

  await a.message(value)

  print(value)
}
