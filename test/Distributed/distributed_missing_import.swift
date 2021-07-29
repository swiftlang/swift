// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor { }

@available(SwiftStdlib 5.5, *)
distributed actor MissingImportDistributedActor_0 { } // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
// expected-error@-1{{actor 'MissingImportDistributedActor_0' has no initializers}}

let t: ActorTransport // expected-error{{cannot find type 'ActorTransport' in scope}}
let a: ActorAddress // expected-error{{cannot find type 'ActorAddress' in scope}}

