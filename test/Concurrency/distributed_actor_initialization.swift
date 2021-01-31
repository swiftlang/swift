// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

distributed actor class OK1 {
  var x: Int = 1

  // ok, since all fields are initialized, the constructors can be synthesized
}

distributed actor class OK2 {
  var x: Int

  convenience init(x: Int, transport: ActorTransport) {
    self.init(transport: transport)
    self.x = x
  }
}

// TODO move this error message to the offending init itself
distributed actor class Bad1 { // expected-error{{'distributed actor' initializer 'init(x:transport:)' must be 'convenience' initializer. Distributed actors have an implicitly synthesized designated 'init(transport:)' local initializer, which other initializers must delegate to.}}
  var x: Int

  init(x: Int, transport: ActorTransport) { // expected-error{{designated initializer for 'Bad1' cannot delegate (with 'self.init'); did you mean this to be a convenience initializer?}}
    self.init(transport: transport) // expected-note{{delegation occurs here}}
    self.x = x
  }
}

//distributed actor class Bad2 {
//  var x: Int
//
//  init(transport: ActorTransport) {
//    self.x = 1
//  }
//}

//distributed actor class Bad1 {
//  var x: Int
//
//  // error: missed to call the `super.init(transport:)`
//
//  init(transport: ActorTransport) {
//    // super.init(transport: transport) // forgot to call super local initializer!
//    self.x = 0
//  }
//}

// TODO: subclasses?
