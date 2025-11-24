// RUN: %target-typecheck-verify-swift -enable-upcoming-feature GlobalActorIsMainActor -enable-upcoming-feature RegionBasedIsolation
// expected-no-diagnostics

@MainActor
class Thing {}

class SubThing: Thing {}
