// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

distributed actor class OK0 { }

distributed actor class OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructors can be synthesized
}

distributed actor class Bad1 {
  init() {} // expected-error {{'distributed actor' initializer 'init()' must be 'convenience' initializer. Distributed actors have an implicitly synthesized designated 'init(transport:)' local initializer, which other initializers must delegate to}}
}

distributed actor class Bad11 {
  convenience init() {} // expected-error {{'distributed actor' initializer 'init()' must delegate to 'self.init(transport:)'}}
}

distributed actor class Bad12 {
  init(x: String) {} // expected-error {{'distributed actor' initializer 'init(x:)' must be 'convenience' initializer. Distributed actors have an implicitly synthesized designated 'init(transport:)' local initializer, which other initializers must delegate to}}
}

distributed actor class OK2 {
  var x: Int

  convenience init(x: Int, transport: ActorTransport) {
    self.init(transport: transport)
    self.x = x
  }
}

distributed actor class Bad2 {
  var x: Int

  convenience init(x: Int, transport: ActorTransport) {
    self.init(transport: transport)
    self.x = x
  }
}

distributed actor class Bad3 {
  var x: Int

  convenience init(y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(y:transport:)' must delegate to 'self.init(transport:)'}}
    // forgot to delegate to init(transport:)
    self.x = y
  }
}

distributed actor class OKMulti {
  // @derived init(transport:)

  convenience init(y: Int, transport: ActorTransport) { // ok
    self.init(transport: transport)
  }

  convenience init(x: Int, y: Int, transport: ActorTransport) {
    // ok, since we do delegate to init(transport) *eventually*
    self.init(y: y, transport: transport)
  }
}

distributed actor class BadMulti {
  // @derived init(transport:)

  convenience init(y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(y:transport:)' must (directly or indirectly) delegate to 'self.init(transport:)'}}
    // self.init(transport: transport) // forgot to delegate to local init!
  }

  convenience init(x: Int, y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(x:y:transport:)' must (directly or indirectly) delegate to 'self.init(transport:)'}}
    // ok, since we do delegate to init(transport) *eventually*
    self.init(y: y, transport: transport)
  }
}

distributed actor class BadRedeclare1 { // expected-error {{type 'BadRedeclare1' does not conform to protocol 'DistributedActor'}}
  convenience init(transport: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' local-initializer 'init(transport:)' cannot be implemented explicitly}}
  // expected-error@-2 {{invalid redeclaration of synthesized 'init(transport:)'}}
  // expected-error@-3 {{invalid redeclaration of synthesized initializer 'init(transport:)'}}
  // expected-note@-4 {{candidate exactly matches}}
}

distributed actor class BadRedeclare11 { // expected-error {{type 'BadRedeclare11' does not conform to protocol 'DistributedActor'}}
  convenience init(transport xxx: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' local-initializer 'init(transport:)' cannot be implemented explicitly}}
  // expected-error@-2 {{invalid redeclaration of synthesized 'init(transport:)'}}
  // expected-error@-3 {{invalid redeclaration of synthesized initializer 'init(transport:)'}}
  // expected-note@-4 {{candidate exactly matches}}
}

distributed actor class BadRedeclare2 { // expected-error {{type 'BadRedeclare2' does not conform to protocol 'DistributedActor'}}
  convenience init(resolve address: ActorAddress, using transport: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' resolve-initializer 'init(resolve:using:)' cannot be implemented explicitly}}
  // expected-note@-2 {{candidate exactly matches}}
  // expected-error@-3 {{invalid redeclaration of synthesized 'init(resolve:using:)'}}
  // expected-error@-4 {{invalid redeclaration of synthesized initializer 'init(resolve:using:)'}}
}

distributed actor class BadRedeclare21 { //expected-error {{type 'BadRedeclare21' does not conform to protocol 'DistributedActor'}}
  convenience init(resolve xxx: ActorAddress, using yyy: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' resolve-initializer 'init(resolve:using:)' cannot be implemented explicitly}}
  // expected-note@-2 {{candidate exactly matches}}
  // expected-error@-3 {{invalid redeclaration of synthesized 'init(resolve:using:)'}}
  // expected-error@-4 {{invalid redeclaration of synthesized initializer 'init(resolve:using:)'}}
}

// TODO: handle subclassing as well
