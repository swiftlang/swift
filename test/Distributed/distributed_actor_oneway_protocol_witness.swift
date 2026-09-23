// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayMethods -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayMethods

// 'oneway' participates in the function type, so protocol-requirement witnessing
// follows the same rule as 'async': a 'oneway' requirement must be satisfied by
// a 'oneway' witness (and vice versa). There is no separate matching rule.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol Pinger: DistributedActor {
  distributed func ping() oneway
}

// A 'oneway' requirement is satisfied by a matching 'oneway' witness.
distributed actor GoodPinger: Pinger {
  distributed func ping() oneway {}
}
