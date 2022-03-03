// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name remoteCall -primary-file %s -dump-ast -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --color --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol P: DistributedActor where ActorSystem == FakeActorSystem {
}

extension P {
  distributed func testing() {}
}

// CHECK: (func_decl implicit "$dist_testing(param:)" interface type='(P) -> () async throws -> String' access=internal captures=(actorSystem<direct>) nonisolated
