// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// This system does not exist: but we should not crash, but just diagnose about it:
typealias DefaultDistributedActorSystem = ClusterSystem // expected-error{{cannot find type 'ClusterSystem' in scope}}

distributed actor MyActor {
  // expected-error@-1{{distributed actor 'MyActor' does not declare ActorSystem it can be used with}}
  // expected-note@-2{{you can provide a module-wide default actor system by declaring:}}
  // expected-error@-3{{type 'MyActor' does not conform to protocol 'DistributedActor'}}
  // expected-note@-4 {{add stubs for conformance}}

  distributed var foo: String {
    return "xyz"
  }
}

distributed actor BadSystemActor {
  // expected-error@-1{{distributed actor 'BadSystemActor' does not declare ActorSystem it can be used with}}
  // expected-note@-2{{you can provide a module-wide default actor system by declaring:}}
  // expected-error@-3{{type 'BadSystemActor' does not conform to protocol 'DistributedActor'}}
  // expected-note@-4 {{add stubs for conformance}}

  // This system does not exist: but we should not crash, but just diagnose about it:
  typealias ActorSystem = ClusterSystem // expected-error{{cannot find type 'ClusterSystem' in scope}}
}
