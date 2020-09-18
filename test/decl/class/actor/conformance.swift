// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

protocol AsyncProtocol {
  func asyncMethod() async -> Int
}

actor class MyActor {
}

// Actors conforming to asynchronous program.
extension MyActor: AsyncProtocol {
  func asyncMethod() async -> Int { return 0 }
}

// FIXME: "Do you want to add a stub?" diagnostics should be suppressed here.
protocol SyncProtocol {
  var propertyA: Int { get }
  // expected-note@-1{{do you want to add a stub}}
  var propertyB: Int { get set }
  // expected-note@-1{{do you want to add a stub}}

  func syncMethodA()
  // expected-note@-1{{do you want to add a stub}}

  func syncMethodB()

  subscript (index: Int) -> String { get }
  // expected-note@-1{{do you want to add a stub}}

  static func staticMethod()
  static var staticProperty: Int { get }
}


actor class OtherActor: SyncProtocol { // expected-error{{type 'OtherActor' does not conform to protocol 'SyncProtocol'}}
  var propertyB: Int = 17
  // expected-note@-1{{actor-isolated property 'propertyB' cannot be used to satisfy a protocol requirement}}

  var propertyA: Int { 17 }
  // expected-note@-1{{actor-isolated property 'propertyA' cannot be used to satisfy a protocol requirement}}

  func syncMethodA() { }
  // expected-note@-1{{actor-isolated instance method 'syncMethodA()' cannot be used to satisfy a protocol requirement; did you mean to make it an asychronous handler?}}{{3-3=@asyncHandler }}

  // Async handlers are okay.
  @asyncHandler
  func syncMethodB() { }

  subscript (index: Int) -> String { "\(index)" }
  // expected-note@-1{{actor-isolated subscript 'subscript(_:)' cannot be used to satisfy a protocol requirement}}

  // Static methods and properties are okay.
  static func staticMethod() { }
  static var staticProperty: Int = 17
}
