// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -I %t

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// A distributed actor's synthesized members reference 'self', which is
// always-unsafe here. None of that is annotatable, so none of it is diagnosed.
@unsafe(always)
distributed actor AlwaysUnsafeActor {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func value() -> Int { 1 }
}
