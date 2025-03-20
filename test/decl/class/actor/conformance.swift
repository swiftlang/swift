// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

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
  var propertyA: Int { get }
  var propertyB: Int { get set }

  func syncMethodA()

  func syncMethodC() -> Int

  func syncMethodE() -> Void

  func syncMethodF(param: String) -> Int

  func syncMethodG() throws -> Void

  subscript (index: Int) -> String { get }

  static func staticMethod()
  static var staticProperty: Int { get }
}


actor OtherActor: SyncProtocol {
  // expected-note@-1{{add '@preconcurrency' to the 'SyncProtocol' conformance to defer isolation checking to run time}}{{19-19=@preconcurrency }}

  var propertyB: Int = 17
  // expected-error@-1{{actor-isolated property 'propertyB' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}

  var propertyA: Int { 17 }
  // expected-error@-1{{actor-isolated property 'propertyA' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}

  func syncMethodA() { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodA()' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodA()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  // nonisolated methods are okay.
  // FIXME: Consider suggesting nonisolated if this didn't match.
  nonisolated func syncMethodC() -> Int { 5 }

  func syncMethodE() -> Void { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodE()' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodE()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  func syncMethodF(param: String) -> Int { 5 }
  // expected-error@-1{{actor-isolated instance method 'syncMethodF(param:)' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodF(param:)' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  func syncMethodG() { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodG()' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodG()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}

  subscript (index: Int) -> String { "\(index)" }
  // expected-error@-1{{actor-isolated subscript 'subscript(_:)' cannot be used to satisfy nonisolated requirement from protocol 'SyncProtocol'}}
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
