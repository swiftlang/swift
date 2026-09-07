// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {
}

distributed actor First {
  distributed func one(second: Second) async throws {
    try await second.two(first: self, second: second)
  }
}

distributed actor Second {
  distributed func two(first: First, second: Second) async {
    try! await first.one(second: self)
  }
}

// In order to avoid weird diagnostics and cycles in lookups, we just outright ban a distributed actor being an actor system.
// This isn't something realistic code would attempt, but we should offer a nice diagnostic rather than crash.
//
// expected-error@+2{{distributed actor cannot conform to the 'DistributedActorSystem' protocol}}
// expected-error@+1{{type 'NotAnActorSystem' does not conform to protocol 'DistributedActorSystem'}}
distributed actor NotAnActorSystem: DistributedActorSystem {
  // expected-note@-1{{add stubs for conformance}}
}

// The same goes for using a 'distributed actor' as some other distributed
// actor's actor system, be it spelled explicitly like here, or picked up
// as the module-wide 'DefaultDistributedActorSystem'
distributed actor UsesActorAsItsSystem {
  // expected-error@-1{{distributed actor 'UsesActorAsItsSystem' does not declare ActorSystem it can be used with}}
  // expected-note@-2{{you can provide a module-wide default actor system by declaring:}}
  // expected-error@-3{{type 'UsesActorAsItsSystem' does not conform to protocol 'DistributedActor'}}
  // expected-error@-4{{type 'UsesActorAsItsSystem' does not conform to protocol 'Identifiable'}}
  // expected-note@-5{{add stubs for conformance}}
  typealias ActorSystem = First
  // expected-note@-1{{possibly intended match 'UsesActorAsItsSystem.ActorSystem' (aka 'First') does not conform to 'DistributedActorSystem'}}
}
