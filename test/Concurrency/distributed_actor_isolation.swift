// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-distributed
// REQUIRES: concurrency

actor LocalActor_1 {
  let name: String = "alice"
  var mutable: String = ""

  distributed func nope() {
    // expected-error@-1{{'distributed' function can only be declared within 'distributed actor'}}
  }
}

struct NotCodableValue { }

distributed struct StructNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}
distributed class ClassNope {} // expected-error{{'distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
distributed enum EnumNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}

distributed actor class DistributedActor_0 { // expected-warning{{'actor class' has been renamed to 'actor'}}
  distributed func okey() {}
}

distributed actor DistributedActor_1 {

  let name: String = "alice" // expected-note{{property declared here}}
  var mutable: String = "alice" // expected-note{{property declared here}}
  var computedMutable: String {
    get {
      "hey"
    }
    set {
      _ = newValue
    }
  }

  distributed let letProperty: String = "" // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed var varProperty: String = "" // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed var computedProperty: String { // expected-error{{'distributed' modifier cannot be applied to this declaration}}
    ""
  }

  distributed static func distributedStatic() {} // expected-error{{'distributed' functions cannot be 'static'}}

  func hello() {} // ok
  func helloAsync() async {} // ok
  func helloAsyncThrows() async throws {} // ok

  distributed func distHello() { } // ok
  distributed func distHelloAsync() async { } // ok
  distributed func distHelloThrows() throws { } // ok
  distributed func distHelloAsyncThrows() async throws { } // ok

  distributed func distInt() async throws -> Int { 42 } // ok
  distributed func distInt(int: Int) async throws -> Int { int } // ok

  distributed func dist(notCodable: NotCodableValue) async throws {
    // expected-error@-1 {{distributed function parameter 'notCodable' of type 'NotCodableValue' does not conform to 'Codable'}}
  }
  distributed func distBadReturn(int: Int) async throws -> NotCodableValue {
    // expected-error@-1 {{distributed function result type 'NotCodableValue' does not conform to 'Codable'}}
    fatalError()
  }

  distributed func distReturnGeneric<T: Codable>(int: Int) async throws -> T { // ok
    fatalError()
  }
  distributed func distReturnGenericWhere<T>(int: Int) async throws -> T where T: Codable { // ok
    fatalError()
  }
  distributed func distBadReturnGeneric<T>(int: Int) async throws -> T {
    // expected-error@-1 {{distributed function result type 'T' does not conform to 'Codable'}}
    fatalError()
  }

  distributed func distGenericParam<T: Codable>(value: T) async throws { // ok
    fatalError()
  }
  distributed func distGenericParamWhere<T>(value: T) async throws -> T where T: Codable { // ok
    fatalError()
  }
  distributed func distBadGenericParam<T>(int: T) async throws {
    // expected-error@-1 {{distributed function parameter 'int' of type 'T' does not conform to 'Codable'}}
    fatalError()
  }

  func test_inside() async throws {
    _ = self.name
    _ = self.computedMutable

    _ = try await self.distInt()
    _ = try await self.distInt(int: 42)

    self.hello()
    _ = await self.helloAsync()
    _ = try await self.helloAsyncThrows()

    self.distHello()
    await self.distHelloAsync()
    try self.distHelloThrows()
    try await self.distHelloAsyncThrows()
  }
}

func test_outside(
  local: LocalActor_1,
  distributed: DistributedActor_1
) async throws {
  // ==== properties
  _ = distributed.actorAddress // ok
  distributed.actorAddress = ActorAddress(parse: "mock://1.1.1.1:8080/#123121") // expected-error{{cannot assign to property: 'actorAddress' is immutable}}

  _ = local.name // ok, special case that let constants are okey
  let _: String = local.mutable // ok, special case that let constants are okey
  _ = distributed.name // expected-error{{distributed actor-isolated property 'name' can only be referenced inside the distributed actor}}
  _ = distributed.mutable // expected-error{{distributed actor-isolated property 'mutable' can only be referenced inside the distributed actor}}

  // ==== special properties (@_distributedActorIndependent)
  // the distributed actor's special fields may always be referred to
  _ = distributed.actorAddress
  _ = distributed.actorTransport

  // ==== non-distributed functions
  _ = await distributed.hello() //expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  _ = await distributed.helloAsync() //expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  _ = try await distributed.helloAsyncThrows() //expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
}

// ==== Codable parameters and return types ------------------------------------

func test_params(
  distributed: DistributedActor_1
) async throws {
  _ = try await distributed.distInt() // ok
  _ = try await distributed.distInt(int: 42) // ok
  _ = try await distributed.dist(notCodable: .init())
}
