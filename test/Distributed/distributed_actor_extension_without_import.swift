// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
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
