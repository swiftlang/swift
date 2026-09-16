// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name deinit_backdeploy -primary-file %s -emit-sil -target %target-swift-5.7-abi-triple -enable-experimental-feature DistributedActorResignRemoteID -I %t | %FileCheck %s --enable-var-scope

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedActorResignRemoteID
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {
  init(system: FakeActorSystem) {
    self.actorSystem = system
  }
}

// CHECK-LABEL: // DA.__deallocating_deinit
// CHECK: sil hidden{{.*}} @$s17deinit_backdeploy2DACfD

// The remote branch performs an availability check before calling `resignRemoteID`
// CHECK: // remoteActorDeinitBB
// CHECK: [[STDLIB_AVAIL_FN:%[0-9]+]] = function_ref @{{.*}}_stdlib_isOSVersionAtLeast{{.*}}
// CHECK: [[IS_AVAIL:%[0-9]+]] = apply [[STDLIB_AVAIL_FN]]
// CHECK: cond_br [[IS_AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]

// In the "unavailable" branch, we do NOT fall back to any other call.
// CHECK: // resignRemoteIDUnavailableBB
// CHECK: [[UNAVAIL_BB]]:
// CHECK-NEXT: br [[CONT_BB:bb[0-9]+]]

// In the "available" branch, we invoke `resignRemoteID`
// CHECK: // resignRemoteIDAvailableBB
// CHECK: [[AVAIL_BB]]:
// CHECK: witness_method $FakeActorSystem, #DistributedActorSystem.resignRemoteID
// CHECK: br [[CONT_BB]]
