// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor { }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor MissingImportDistributedActor_0 { } // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
// expected-error@-1{{class 'MissingImportDistributedActor_0' has no initializers}}
