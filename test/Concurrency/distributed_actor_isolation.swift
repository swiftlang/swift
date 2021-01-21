// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor class LocalActor_1 {
  let name: String = "alice"
  var mutable: String = ""
}

struct NotCodableValue { }

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

  func hello() async {
    print("Hello, by: \(self.actorAddress)")
  }

  distributed func distVoid1() async throws { } // ok
  distributed func distVoid2() async throws -> () { } // ok
  distributed func distVoid3() async throws -> Void { } // ok

  distributed func distInt() async throws -> Int { 42 } // ok

  distributed func distInt(int: Int) async throws -> Int { int } // ok

  distributed func dist(notCodable: NotCodableValue) async throws {
    // expected-error@-1 {{distributed function parameter 'notCodable' type 'NotCodableValue' does not conform to 'Codable'}}
  }
  distributed func distBadReturn(int: Int) async throws -> NotCodableValue {
    // expected-error@-1 {{distributed function result type 'NotCodableValue' does not conform to 'Codable'}}
    fatalError()
  }

  distributed func distReturnGeneric<T: Codable>(int: Int) async throws -> T { // ok
    fatalError()
  }
  distributed func distReturnGenericWhere<T>(int: Int) async throws -> T
    where T: Codable { // ok
    fatalError()
  }
  distributed func distBadReturnGeneric<T>(int: Int) async throws -> T {
    // expected-error@-1 {{distributed function result type 'T' does not conform to 'Codable'}}
    fatalError()
  }

  func test() async throws {
    _ = self.name
    _ = self.computedMutable
    _ = self.sync()
    _ = await self.async()
    try await self.distVoid1()
    try await self.distVoid2()
    try await self.distVoid3()
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
  try await distributed.distVoid1() // ok
  
  // special: the actorAddress may always be referred to
  _ = distributed.actorAddress
}

// ==== Codable parameters and return types ------------------------------------

func test_params(
  distributed: DistributedActor_1
) async throws {
  _ = try await distributed.distInt() // ok
  _ = try await distributed.distInt(int: 42) // ok
  _ = try await distributed.dist(notCodable: .init())
}
