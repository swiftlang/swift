// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor OK0 { }

@available(SwiftStdlib 5.5, *)
distributed actor OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructors can be synthesized
}

// TODO: test all the FIXITs in this file (!!!)

@available(SwiftStdlib 5.5, *)
distributed actor Bad1 {
  init() {
    // expected-error@-1 {{'distributed actor' initializer 'init()' must be 'convenience' initializer. Distributed actors have an implicitly synthesized designated 'init(transport:)' local-initializer, which other initializers must delegate to}}
    // expected-error@-2 {{'distributed actor' initializer 'init()' must (directly or indirectly) delegate to 'init(transport:)}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad11 {
  convenience init() {
    // expected-error@-1 {{'distributed actor' initializer 'init()' must (directly or indirectly) delegate to 'init(transport:)'}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad12 {
  init(x: String) {
    // expected-error@-1 {{'distributed actor' initializer 'init(x:)' must be 'convenience' initializer. Distributed actors have an implicitly synthesized designated 'init(transport:)' local-initializer, which other initializers must delegate to}}
    // expected-error@-2 {{'distributed actor' initializer 'init(x:)' must (directly or indirectly) delegate to 'init(transport:)}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor OK2 {
  var x: Int

  convenience init(x: Int, transport: ActorTransport) {
    self.init(transport: transport)
    self.x = x
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad2 {
  var x: Int

  convenience init(x: Int, transport: ActorTransport) {
    self.init(transport: transport)
    self.x = x
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad3 {
  var x: Int

  convenience init(y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(y:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
    // forgot to delegate to init(transport:)
    self.x = y
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor OKMulti {
  // @derived init(transport:)

  convenience init(y: Int, transport: ActorTransport) { // ok
    self.init(transport: transport)
  }

  convenience init(x: Int, y: Int, transport: ActorTransport) {
    // ok, since we do delegate to init(transport) *eventually*
    self.init(y: y, transport: transport)
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor BadMulti {
  // @derived init(transport:)

  convenience init(y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(y:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
    // self.init(transport: transport) // forgot to delegate to local init!
  }

  convenience init(x: Int, y: Int, transport: ActorTransport) {
    // expected-error@-1 {{'distributed actor' initializer 'init(x:y:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
    // ok, since we do delegate to init(transport) *eventually*
    self.init(y: y, transport: transport)
  }
}

// It is illegal to manually invoke the resolve initializer,
// because it may result in "not a real instance" i.e. a proxy
// and a proxy does not have any storage, so it would be wrong to allow other
// initializers to keep running while we actually created a proxy with no storage.
@available(SwiftStdlib 5.5, *)
distributed actor BadResolveInitCall {
  convenience init(any: Any, id: AnyActorIdentity, transport: ActorTransport) throws {
    // expected-error@-1 {{'distributed actor' initializer 'init(any:id:transport:)' cannot delegate to resolve-initializer 'init(resolve:using:)', as it may result resolving a storageless proxy instance}}
    // expected-error@-2 {{'distributed actor' initializer 'init(any:id:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
    try self.init(resolve: id, using: transport) // TODO: suggest removing this call, since it is illegal
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor BadRedeclare1 { // expected-error {{type 'BadRedeclare1' does not conform to protocol 'DistributedActor'}}
  convenience init(transport: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' local-initializer 'init(transport:)' cannot be implemented explicitly}}
  // expected-error@-2 {{invalid redeclaration of synthesized 'init(transport:)'}}
  // expected-error@-3 {{invalid redeclaration of synthesized initializer 'init(transport:)'}}
  // expected-note@-4 {{candidate exactly matches}}
}

@available(SwiftStdlib 5.5, *)
distributed actor BadRedeclare11 { // expected-error {{type 'BadRedeclare11' does not conform to protocol 'DistributedActor'}}
  convenience init(transport xxx: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' local-initializer 'init(transport:)' cannot be implemented explicitly}}
  // expected-error@-2 {{invalid redeclaration of synthesized 'init(transport:)'}}
  // expected-error@-3 {{invalid redeclaration of synthesized initializer 'init(transport:)'}}
  // expected-note@-4 {{candidate exactly matches}}
}

@available(SwiftStdlib 5.5, *)
distributed actor BadRedeclare2 { // expected-error {{type 'BadRedeclare2' does not conform to protocol 'DistributedActor'}}
  convenience init(resolve id: AnyActorIdentity, using transport: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' resolve-initializer 'init(resolve:using:)' cannot be implemented explicitly}}
  // expected-note@-2 {{candidate exactly matches}}
  // expected-error@-3 {{invalid redeclaration of synthesized 'init(resolve:using:)'}}
  // expected-error@-4 {{invalid redeclaration of synthesized initializer 'init(resolve:using:)'}}
}

@available(SwiftStdlib 5.5, *)
distributed actor BadRedeclare21 { //expected-error {{type 'BadRedeclare21' does not conform to protocol 'DistributedActor'}}
  convenience init(resolve xxx: AnyActorIdentity, using yyy: ActorTransport) {}
  // expected-error@-1 {{'distributed actor' resolve-initializer 'init(resolve:using:)' cannot be implemented explicitly}}
  // expected-note@-2 {{candidate exactly matches}}
  // expected-error@-3 {{invalid redeclaration of synthesized 'init(resolve:using:)'}}
  // expected-error@-4 {{invalid redeclaration of synthesized initializer 'init(resolve:using:)'}}
}

@available(SwiftStdlib 5.5, *)
distributed actor BadRedeclare22 { //expected-error {{type 'BadRedeclare22' does not conform to protocol 'DistributedActor'}}
  convenience init(resolve: AnyActorIdentity, using yyy: ActorTransport) throws {}
  // expected-error@-1 {{'distributed actor' resolve-initializer 'init(resolve:using:)' cannot be implemented explicitly}}
  // expected-note@-2 {{candidate exactly matches}}
  // expected-error@-3 {{invalid redeclaration of synthesized 'init(resolve:using:)'}}
  // expected-error@-4 {{invalid redeclaration of synthesized initializer 'init(resolve:using:)'}}
}
