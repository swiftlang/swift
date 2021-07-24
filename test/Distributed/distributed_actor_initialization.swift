// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
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

// TODO: test all the FIXITs in this file

@available(SwiftStdlib 5.5, *)
distributed actor Bad1 {
  init() {
    // expected-error@-1 {{'distributed actor' initializer 'init()' must (directly or indirectly) delegate to 'init(transport:)}}
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
    // expected-error@-1 {{'distributed actor' initializer 'init(x:)' must (directly or indirectly) delegate to 'init(transport:)}}
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

  // expected-error@+1 {{'distributed actor' initializer 'init(y:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
  convenience init(y: Int, transport: ActorTransport) {}

  // expected-error@+1 {{'distributed actor' initializer 'init(x:y:transport:)' must (directly or indirectly) delegate to 'init(transport:)'}}
  convenience init(x: Int, y: Int, transport: ActorTransport) {
    self.init(y: y, transport: transport)
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
