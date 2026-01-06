// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -Xllvm -sil-print-types -emit-sil -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

class SomeClass {}

enum Err : Error {
  case blah
}

distributed actor MyDistActor {
  var localOnlyField: SomeClass

  init?(system_sync_fail: FakeActorSystem, cond: Bool) {
    self.actorSystem = system_sync_fail
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: // MyDistActor.init(system_sync_fail:cond:)
// CHECK: sil hidden @$s14default_deinit11MyDistActorC16system_sync_fail4condACSg015FakeDistributedE7Systems0jE6SystemV_Sbtcfc : $@convention(method) (@owned FakeActorSystem, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
// CHECK: bb0([[SYS_PARAM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)

// CHECK:   [[SYS_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:   store [[SYS_PARAM]] to [[SYS_FIELD]] : $*FakeActorSystem

// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{.*}} to [init] [[ID_FIELD]] : $*ActorAddress

// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

// CHECK: [[SUCCESS_BB]]:
// CHECK: [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF
// CHECK:   = apply [[READY_FN]]
// CHECK:   br [[RET_BB:bb[0-9]+]]

// CHECK: [[FAIL_BB]]:
// CHECK:   [[RESIGN_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8resignIDyyAA0C7AddressVF : $@convention(method) (@guaranteed ActorAddress, @guaranteed FakeActorSystem) -> ()
// CHECK:   = apply [[RESIGN_FN]]
// CHECK:   builtin "destroyDefaultActor"
// CHECK:   br [[RET_BB]]

// CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
// CHECK:   return
// CHECK: } // end sil function '$s14default_deinit11MyDistActorC16system_sync_fail4condACSg015FakeDistributedE7Systems0jE6SystemV_Sbtcfc'


}
