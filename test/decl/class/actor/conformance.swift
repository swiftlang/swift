// RUN: %target-typecheck-verify-swift  -disable-availability-checking

// REQUIRES: concurrency

protocol AsyncProtocol {
  func asyncMethod() async -> Int
}

actor MyActor {
}

// Actors conforming to asynchronous program.
extension MyActor: AsyncProtocol {
  func asyncMethod() async -> Int { return 0 }
}

protocol SyncProtocol {
  var propertyA: Int { get } // expected-note{{'propertyA' declared here}}
  var propertyB: Int { get set } // expected-note{{'propertyB' declared here}}

  func syncMethodA() // expected-note{{mark the protocol requirement 'syncMethodA()' 'async' to allow actor-isolated conformances}}{{21-21= async}}

  func syncMethodC() -> Int

  func syncMethodE() -> Void // expected-note{{mark the protocol requirement 'syncMethodE()' 'async' to allow actor-isolated conformances}}{{21-21= async}}

  func syncMethodF(param: String) -> Int // expected-note{{mark the protocol requirement 'syncMethodF(param:)' 'async' to allow actor-isolated conformances}}{{34-34= async}}

  func syncMethodG() throws -> Void // expected-note{{mark the protocol requirement 'syncMethodG()' 'async' to allow actor-isolated conformances}}{{22-22=async }}

  subscript (index: Int) -> String { get } // expected-note{{'subscript(_:)' declared here}}

  static func staticMethod()
  static var staticProperty: Int { get }
}


actor OtherActor: SyncProtocol {
  // expected-note@-1{{add '@preconcurrency' to the 'SyncProtocol' conformance to defer isolation checking to run time}}{{19-19=@preconcurrency }}

  var propertyB: Int = 17
  // expected-error@-1{{actor-isolated property 'propertyB' cannot be used to satisfy nonisolated protocol requirement}}

  var propertyA: Int { 17 }
  // expected-error@-1{{actor-isolated property 'propertyA' cannot be used to satisfy nonisolated protocol requirement}}

  func syncMethodA() { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodA()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodA()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  // nonisolated methods are okay.
  // FIXME: Consider suggesting nonisolated if this didn't match.
  nonisolated func syncMethodC() -> Int { 5 }

  func syncMethodE() -> Void { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodE()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodE()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  func syncMethodF(param: String) -> Int { 5 }
  // expected-error@-1{{actor-isolated instance method 'syncMethodF(param:)' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodF(param:)' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  func syncMethodG() { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodG()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodG()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  subscript (index: Int) -> String { "\(index)" }
  // expected-error@-1{{actor-isolated subscript 'subscript(_:)' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'subscript(_:)' to make this subscript not isolated to the actor}}{{3-3=nonisolated }}

  // Static methods and properties are okay.
  static func staticMethod() { }
  static var staticProperty: Int = 17
}

protocol Initializers {
  init()
  init(string: String)
  init(int: Int) async
}

protocol SelfReqs {
  func withBells() async -> Self
}

actor A1: Initializers, SelfReqs {
  init() { }
  init(string: String) { }
  init(int: Int) async { }

  func withBells() async -> A1 { self }
}

actor A2: Initializers {
  init() { }
  init(string: String) { }
  init(int: Int) { }

  func withBells() async -> A2 { self }
}
