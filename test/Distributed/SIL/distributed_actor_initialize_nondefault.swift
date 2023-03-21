// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -emit-sil -verify -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

distributed actor MyDistActor {
    nonisolated var localUnownedExecutor: UnownedSerialExecutor? {
        return MainActor.sharedUnownedExecutor
    }

// // MyDistActor.init(actorSystem:)
// CHECK: sil hidden @$s14default_deinit11MyDistActorC11actorSystemAC015FakeDistributedE7Systems0h9RoundtripeG0C_tcfc : $@convention(method) (@owned FakeRoundtripActorSystem, @owned MyDistActor) -> @owned MyDistActor
// CHECK-NOT: {{%[0-9]+}} = builtin "initializeDefaultActor"(%1 : $MyDistActor) : $()
// CHECK: [[ACTOR_INSTANCE:%[0-9]+]] = builtin "initializeNonDefaultDistributedActor"(%1 : $MyDistActor) : $()
}

