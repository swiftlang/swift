// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor OK0 { }

@available(SwiftStdlib 5.5, *)
distributed actor OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructor can be synthesized
}

// TODO(distributed): test all the FIXITs in this file

@available(SwiftStdlib 5.5, *)
distributed actor Bad1 {
  init() {
    // expected-error@-1 {{designated distributed actor initializer 'init()' is missing required ActorTransport parameter}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad12 {
  init(x: String) {
    // expected-error@-1 {{designated distributed actor initializer 'init(x:)' is missing required ActorTransport parameter}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor OK2 {
  var x: Int

  init(x: Int, transport: ActorTransport) { // ok
    self.x = x
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Bad2 {
  var x: Int = 1

  init(transport: ActorTransport, too many: ActorTransport) {
    // expected-error@-1{{designated distributed actor initializer 'init(transport:too:)' must accept exactly one ActorTransport parameter, found 2}}
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor OK3 {
  var x: Int

  init(y: Int, transport: ActorTransport) {
    self.x = y
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor OKMulti {

  convenience init(y: Int, transport: ActorTransport) { // ok
    self.init(transport: transport)
  }

}
