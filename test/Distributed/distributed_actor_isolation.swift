// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
actor LocalActor_1 {
  let name: String = "alice"
  var mutable: String = ""

  distributed func nope() {
    // expected-error@-1{{'distributed' function can only be declared within 'distributed actor'}}
  }
}

struct NotCodableValue { }

@available(SwiftStdlib 5.5, *)
distributed struct StructNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}
@available(SwiftStdlib 5.5, *)
distributed class ClassNope {} // expected-error{{'distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
@available(SwiftStdlib 5.5, *)
distributed enum EnumNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}

@available(SwiftStdlib 5.5, *)
distributed actor DistributedActor_1 {

  let name: String = "alice" // expected-note{{distributed actor state is only available within the actor instance}}
  var mutable: String = "alice" // expected-note{{distributed actor state is only available within the actor instance}}
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

  func hello() {} // expected-note{{only 'distributed' functions can be called from outside the distributed actor}}
  func helloAsync() async {} // expected-note{{only 'distributed' functions can be called from outside the distributed actor}}
  func helloAsyncThrows() async throws {} // expected-note{{only 'distributed' functions can be called from outside the distributed actor}}

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

@available(SwiftStdlib 5.5, *)
extension DistributedActor_1 {
  static func _remote_distHello(actor: DistributedActor_1) async throws { }
  static func _remote_distHelloAsync(actor: DistributedActor_1) async throws  { }
  static func _remote_distHelloThrows(actor: DistributedActor_1) async throws { }
  static func _remote_distHelloAsyncThrows(actor: DistributedActor_1) async throws { }

  static func _remote_distInt(actor: DistributedActor_1) async throws -> Int { 42 }
  static func _remote_distInt(int: Int, actor: DistributedActor_1) async throws -> Int { int }

  static func _remote_distReturnGeneric<T: Codable>(int: Int, actor: DistributedActor_1) async throws -> T {
    fatalError()
  }
  static func _remote_distReturnGenericWhere<T>(int: Int, actor: DistributedActor_1) async throws -> T where T: Codable {
    fatalError()
  }

  static func _remote_distGenericParam<T: Codable>(value: T, actor: DistributedActor_1) async throws {
    fatalError()
  }
  static func _remote_distGenericParamWhere<T>(value: T, actor: DistributedActor_1) async throws -> T where T: Codable {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
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
  _ = await distributed.hello() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  _ = await distributed.helloAsync() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  _ = try await distributed.helloAsyncThrows() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
}

@available(SwiftStdlib 5.5, *)
distributed actor DistributedActor_2 {
  // TODO: should report the error on the remote function instead?
  distributed func okey() {} // expected-error{{remote function '_remote_okey()' must be static.}}
}

@available(SwiftStdlib 5.5, *)
extension DistributedActor_2 {
  func _remote_okey() {}
}

// ==== Codable parameters and return types ------------------------------------

@available(SwiftStdlib 5.5, *)
func test_params(
  distributed: DistributedActor_1
) async throws {
  _ = try await distributed.distInt() // ok
  _ = try await distributed.distInt(int: 42) // ok
  _ = try await distributed.dist(notCodable: .init())
}
