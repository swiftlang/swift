// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

// On purpose missing 'import Distributed'
import FakeDistributedActorSystems

extension FakeRoundtripActorSystemDistributedActor {
  // expected-error@+1{{}}
  distributed func echo(name: String) -> String {
    return "Echo: \(name)"
  }
}
