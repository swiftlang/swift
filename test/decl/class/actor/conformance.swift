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

// expected-error@+1{{conformance of 'OtherActor' to protocol 'SyncProtocol' crosses into actor-isolated code and can cause data races}}
actor OtherActor: SyncProtocol {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}{{19-19=@preconcurrency }}
  // expected-note@-2{{mark all declarations used in the conformance 'nonisolated'}}

  var propertyB: Int = 17
  // expected-note@-1{{actor-isolated property 'propertyB' cannot satisfy nonisolated requirement}}

  var propertyA: Int { 17 }
  // expected-note@-1{{actor-isolated property 'propertyA' cannot satisfy nonisolated requirement}}

  func syncMethodA() { }
  // expected-note@-1{{actor-isolated instance method 'syncMethodA()' cannot satisfy nonisolated requirement}}

  // nonisolated methods are okay.
  nonisolated func syncMethodC() -> Int { 5 }

  func syncMethodE() -> Void { }
  // expected-note@-1{{actor-isolated instance method 'syncMethodE()' cannot satisfy nonisolated requirement}}

  func syncMethodF(param: String) -> Int { 5 }
  // expected-note@-1{{actor-isolated instance method 'syncMethodF(param:)' cannot satisfy nonisolated requirement}}

  func syncMethodG() { }
  // expected-note@-1{{actor-isolated instance method 'syncMethodG()' cannot satisfy nonisolated requirement}}

  subscript (index: Int) -> String { "\(index)" }
  // expected-note@-1{{actor-isolated subscript 'subscript(_:)' cannot satisfy nonisolated requirement}}

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
