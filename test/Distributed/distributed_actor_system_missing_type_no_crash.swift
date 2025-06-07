// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// rdar://90886302 This test would crash if not typealias ActorSystem was declared for an actor
distributed actor Fish {
  // expected-error@-1{{distributed actor 'Fish' does not declare ActorSystem it can be used with}}
  // expected-error@-2{{distributed actor 'Fish' does not declare ActorSystem it can be used with}}
  // expected-error@-3{{type 'Fish' does not conform to protocol 'DistributedActor'}}
  // expected-note@-4{{you can provide a module-wide default actor system by declaring:\ntypealias DefaultDistributedActorSystem = <#ConcreteActorSystem#>}}
  // expected-note@-5 {{add stubs for conformance}}

  distributed func tell(_ text: String, by: Fish) {
    // What would the fish say, if it could talk?
  }
}
