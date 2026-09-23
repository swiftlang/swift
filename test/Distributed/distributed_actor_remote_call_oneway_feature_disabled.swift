// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

// This test intentionally does NOT enable the 'OnewayMethods'
// experimental feature, to verify the feature-gate diagnostic on 'oneway'.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  // Without the experimental feature enabled, the 'oneway' modifier is rejected.
  // expected-error@+1{{'oneway' modifier is only valid when experimental feature OnewayMethods is enabled}}
  distributed func thanks() oneway {}
}
