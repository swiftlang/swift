// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-async-handler

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

  func syncMethodB()

  func syncMethodC() -> Int

  subscript (index: Int) -> String { get }

  static func staticMethod()
  static var staticProperty: Int { get }
}


actor OtherActor: SyncProtocol {
  var propertyB: Int = 17
  // expected-error@-1{{actor-isolated property 'propertyB' cannot be used to satisfy a protocol requirement}}

  var propertyA: Int { 17 }
  // expected-error@-1{{actor-isolated property 'propertyA' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'propertyA' to make this property not isolated to the actor}}{{3-3=nonisolated }}

  func syncMethodA() { }
  // expected-error@-1{{actor-isolated instance method 'syncMethodA()' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'syncMethodA()' to make this instance method not isolated to the actor}}{{3-3=nonisolated }}
  // expected-note@-3{{add '@asyncHandler' to function 'syncMethodA()' to create an implicit asynchronous context}}{{3-3=@asyncHandler }}

  // Async handlers are okay.
  @asyncHandler
  func syncMethodB() { }

  // nonisolated methods are okay.
  // FIXME: Consider suggesting nonisolated if this didn't match.
  nonisolated func syncMethodC() -> Int { 5 }

  subscript (index: Int) -> String { "\(index)" }
  // expected-error@-1{{actor-isolated subscript 'subscript(_:)' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'subscript(_:)' to make this subscript not isolated to the actor}}{{3-3=nonisolated }}

  // Static methods and properties are okay.
  static func staticMethod() { }
  static var staticProperty: Int = 17
}
