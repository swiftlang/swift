// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -Xllvm -sil-print-types -emit-sil -verify -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_in_compiler

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

distributed actor MyDistActor {

// protocol witness for DistributedActorSystem.resolve<A>(id:as:) in conformance FakeRoundtripActorSystem
// CHECK:  sil hidden @$s14default_deinit11MyDistActorC7resolve2id5usingAC015FakeDistributedE7Systems0E7AddressV_AG0i9RoundtripE6SystemCtKFZ
// CHECK:  bb0([[ACTOR_ID_ARG:%[0-9]+]] : $ActorAddress, [[SYSTEM_ARG:%[0-9]+]] : $FakeRoundtripActorSystem, [[TYPE_ARG:%[0-9]+]] : $@thick MyDistActor.Type):
// CHECK: [[SYS_RESOLVE_RESULT:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0a9RoundtripC6SystemC7resolve2id2asxSgAA0C7AddressV_xmtK0B00bC0RzlF

// CHECK: [[ACTOR_INSTANCE:%[0-9]+]] = builtin "initializeDistributedRemoteActor"(%7 : $@thick MyDistActor.Type) : $MyDistActor
// CHECK: [[ID_PROPERTY:%[0-9]+]] = ref_element_addr [immutable] [[ACTOR_INSTANCE]] : $MyDistActor, #MyDistActor.id
// CHECK: retain_value [[ACTOR_ID_ARG]] : $ActorAddress
// CHECK: store [[ACTOR_ID_ARG]] to [[ID_PROPERTY]] : $*ActorAddress
// CHECK: [[SYSTEM_PROPERTY:%[0-9]+]] = ref_element_addr [immutable] [[ACTOR_INSTANCE]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK: strong_retain [[SYSTEM_ARG]] : $FakeRoundtripActorSystem
// CHECK: store [[SYSTEM_ARG]] to [[SYSTEM_PROPERTY]] : $*FakeRoundtripActorSystem
// CHECK: br bb5([[ACTOR_INSTANCE]] : $MyDistActor)
}

