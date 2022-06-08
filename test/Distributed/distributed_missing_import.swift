// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor { }

distributed actor MissingImportDistributedActor_0 { }
// expected-error@-1{{'Distributed' module not imported, required for 'distributed actor'}}

let t: DistributedActorSystem // expected-error{{cannot find type 'DistributedActorSystem' in scope}}
let a: ActorAddress // expected-error{{cannot find type 'ActorAddress' in scope}}

