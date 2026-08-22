// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

// This test intentionally does NOT enable the 'DistributedRemoteBlockingCalls'
// experimental feature, to verify the feature-gate diagnostic on '@remoteCall'.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  // Without the experimental feature enabled, '@remoteCall' is rejected.
  // expected-error@+1{{'remoteCall(blocking)' attribute is only valid when experimental feature DistributedRemoteBlockingCalls is enabled}}
  @remoteCall(blocking)
  distributed func hello() async -> Int { 0 }
}
