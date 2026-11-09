// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor Worker {
  @_remoteCallIdempotent
  distributed func put(_ value: Int) {}

  @_remoteCallIdempotent
  distributed func get() -> Int { 0 }

  // expected-error@+1:3{{'@_remoteCallIdempotent' can only be applied to a 'distributed' method}}
  @_remoteCallIdempotent
  func notDistributed() {}
}

protocol Store: DistributedActor {
  @_remoteCallIdempotent
  distributed func put(_ value: Int)
}

actor Plain {
  // expected-error@+1:3{{'@_remoteCallIdempotent' can only be applied to a 'distributed' method}}
  @_remoteCallIdempotent
  func put(_ value: Int) {}
}

// expected-error@+1:1{{'@_remoteCallIdempotent' can only be applied to a 'distributed' method}}
@_remoteCallIdempotent
func global() {}

// expected-error@+1:1{{'@_remoteCallIdempotent' may only be used on 'func' declarations}}
@_remoteCallIdempotent
struct NotAFunction {}
