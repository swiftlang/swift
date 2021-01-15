// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency
actor class LocalActor_1 {
  let name: String = "alice"
  var mutable: String = ""
}

struct NotCodableParameter { }

distributed actor class DistributedActor_1 {
  let name: String = "alice" // expected-note{{mutable state is only available within the actor instance}}
  var mutable: String = "alice" // expected -note{{mutable-state is only available within the actor instance}}
  var computedMutable: String {
    get {
      "hey"
    }
    set {
      _ = newValue
    }
  }

  func sync() -> Int { // expected -note{{only asynchronous methods can be used outside the actor instance; do you want to add 'async'}}
    42
  }

  func async() async -> Int {
    42
  }

  distributed func distVoid() async throws { }

  distributed func distInt() async throws -> Int {
    42
  }

  distributed func distInt(int: Int) async throws -> Int {
    int
  }

  distributed func dist(notCodable: NotCodableParameter) async throws { }

  func test() async throws {
    _ = self.name
    _ = self.computedMutable
    _ = self.sync()
    _ = await self.async()
    try await self.distVoid()
    _ = try await self.distInt()
  }
}

func test(
  local: LocalActor_1,
  distributed: DistributedActor_1
) async throws {
  _ = local.name // ok, special case that let constants are okey
  _ = distributed.name // expected-error{{distributed actor-isolated property 'name' can only be referenced inside the distributed actor}}
  //    _ = local.mutable // expected- error{{actor-isolated property 'mutable' can only be referenced inside the actor}}
  //    _ = distributed.mutable // expected- error{{actor-isolated property 'mutable' can only be referenced inside the actor}}
  //    _ = distributed.sync() // expected- error{{actor-isolated instance method 'sync()' can only be referenced inside the actor}}
  //
  //    _ = await distributed.async() // expected -error{{actor-isolated instance method 'dist()' can only be referenced inside the distributed actor}}
  try await distributed.distVoid() // ok
}

// ==== Codable parameters and return types ------------------------------------
func test_params(
  distributed: DistributedActor_1
) async throws {
  _ = try await distributed.distInt() // ok
  _ = try await distributed.distInt(int: 42) // ok
  _ = try await distributed.dist(notCodable: .init()) // expected-error{{distributed function parameter 'notCodable' type 'NotCodableParameter' must conform to 'Codable'}}
}
