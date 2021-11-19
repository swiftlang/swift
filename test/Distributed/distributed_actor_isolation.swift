// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking -verify-ignore-unknown
// REQUIRES: concurrency
// REQUIRES: distributed

// TODO(distributed): rdar://82419661 remove -verify-ignore-unknown here, no warnings should be emitted for our
//  generated code but right now a few are, because of Sendability checks -- need to track it down more.

import _Distributed

/// Use the existential wrapper as the default actor transport.
typealias DefaultActorTransport = AnyActorTransport

struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

actor LocalActor_1 {
  let name: String = "alice"
  var mutable: String = ""

  distributed func nope() {
    // expected-error@-1{{'distributed' method can only be declared within 'distributed actor'}}
  }
}

struct NotCodableValue { }

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

  distributed static func distributedStatic() {} // expected-error{{'distributed' method cannot be 'static'}}
  distributed class func distributedClass() {}
  // expected-error@-1{{class methods are only allowed within classes; use 'static' to declare a static method}}
  // expected-error@-2{{'distributed' method cannot be 'static'}} // TODO(distributed): should call out 'class' instead?

  func hello() {} // expected-note{{distributed actor-isolated instance method 'hello()' declared here}}
  func helloAsync() async {} // expected-note{{distributed actor-isolated instance method 'helloAsync()' declared here}}
  func helloAsyncThrows() async throws {} // expected-note{{distributed actor-isolated instance method 'helloAsyncThrows()' declared here}}

  distributed func distHello() { } // ok
  distributed func distHelloAsync() async { } // ok
  distributed func distHelloThrows() throws { } // ok
  distributed func distHelloAsyncThrows() async throws { } // ok

  distributed func distInt() async throws -> Int { 42 } // ok
  distributed func distInt(int: Int) async throws -> Int { int } // ok
  distributed func distIntString(int: Int, two: String) async throws -> (String) { "\(int) + \(two)" } // ok

  distributed func dist(notCodable: NotCodableValue) async throws {
    // expected-error@-1 {{parameter 'notCodable' of type 'NotCodableValue' in distributed instance method does not conform to 'Codable'}}
  }
  distributed func distBadReturn(int: Int) async throws -> NotCodableValue {
    // expected-error@-1 {{result type 'NotCodableValue' of distributed instance method does not conform to 'Codable'}}
    fatalError()
  }

  distributed func varargs(int: Int...) {
    // expected-error@-1{{cannot declare variadic argument 'int' in distributed instance method 'varargs(int:)'}}
  }

  distributed func closure(close: () -> String) {
    // expected-error@-1{{parameter 'close' of type '() -> String' in distributed instance method does not conform to 'Codable'}}
  }

  distributed func noInout(inNOut burger: inout String) {
    // expected-error@-1{{cannot declare 'inout' argument 'burger' in distributed instance method 'noInout(inNOut:)'}}{{43-49=}}
  }

  distributed func distReturnGeneric<T: Codable & Sendable>(item: T) async throws -> T { // ok
    item
  }
  distributed func distReturnGenericWhere<T: Sendable>(item: Int) async throws -> T where T: Codable { // ok
    fatalError()
  }
  distributed func distBadReturnGeneric<T: Sendable>(int: Int) async throws -> T {
    // expected-error@-1 {{result type 'T' of distributed instance method does not conform to 'Codable'}}
    fatalError()
  }

  distributed func distGenericParam<T: Codable & Sendable>(value: T) async throws { // ok
    fatalError()
  }
  distributed func distGenericParamWhere<T: Sendable>(value: T) async throws -> T where T: Codable { // ok
    value
  }
  distributed func distBadGenericParam<T: Sendable>(int: T) async throws {
    // expected-error@-1 {{parameter 'int' of type 'T' in distributed instance method does not conform to 'Codable'}}
    fatalError()
  }

  static func staticFunc() -> String { "" } // ok

  @MainActor
  static func staticMainActorFunc() -> String { "" } // ok

  static distributed func staticDistributedFunc() -> String {
    // expected-error@-1{{'distributed' method cannot be 'static'}}{10-21=}
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

    // Hops over to the global actor.
    _ = await DistributedActor_1.staticMainActorFunc()
  }
}

func test_outside(
  local: LocalActor_1,
  distributed: DistributedActor_1
) async throws {
  // ==== properties
  _ = distributed.id // ok
  distributed.id = AnyActorIdentity(ActorAddress(parse: "mock://1.1.1.1:8080/#123121")) // expected-error{{cannot assign to property: 'id' is immutable}})

  _ = local.name // ok, special case that let constants are okey
  let _: String = local.mutable // ok, special case that let constants are okey
  _ = distributed.name // expected-error{{distributed actor-isolated property 'name' can only be referenced inside the distributed actor}}
  _ = distributed.mutable // expected-error{{distributed actor-isolated property 'mutable' can only be referenced inside the distributed actor}}

  // ==== special properties (nonisolated, implicitly replicated)
  // the distributed actor's special fields may always be referred to
  _ = distributed.id
  _ = distributed.actorTransport

  // ==== static functions
  _ = distributed.staticFunc() // expected-error{{static member 'staticFunc' cannot be used on instance of type 'DistributedActor_1'}}
  _ = DistributedActor_1.staticFunc()

  // ==== non-distributed functions
  distributed.hello() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  _ = await distributed.helloAsync() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  _ = try await distributed.helloAsyncThrows() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
}

// ==== Protocols and static (non isolated functions)

protocol P {
  static func hello() -> String
}
extension P {
  static func hello() -> String { "" }
}

distributed actor ALL: P {
}

// ==== Codable parameters and return types ------------------------------------

func test_params(
  distributed: DistributedActor_1
) async throws {
  _ = try await distributed.distInt() // ok
  _ = try await distributed.distInt(int: 42) // ok
  _ = try await distributed.dist(notCodable: .init())
}

// Actor initializer isolation (through typechecking only!)
distributed actor DijonMustard {
  nonisolated init(transport: AnyActorTransport) {} // expected-warning {{'nonisolated' on an actor's synchronous initializer is invalid; this is an error in Swift 6}} {{3-15=}}

  convenience init(conv: AnyActorTransport) {
    self.init(transport: conv)
    self.f() // expected-error {{actor-isolated instance method 'f()' can not be referenced from a non-isolated context}}
  }

  func f() {} // expected-note {{distributed actor-isolated instance method 'f()' declared here}}

  nonisolated convenience init(conv2: AnyActorTransport) { // expected-warning {{'nonisolated' on an actor's convenience initializer is redundant; this is an error in Swift 6}} {{3-15=}}
    self.init(transport: conv2)
  }
}
