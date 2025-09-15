// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor { }

distributed actor MissingImportDistributedActor_0 { }
// expected-error@-1{{distributed actor 'MissingImportDistributedActor_0' declared without importing module 'Distributed'}}{{5:1-1=import Distributed\n}}

let t: DistributedActorSystem // expected-error{{cannot find type 'DistributedActorSystem' in scope}}

