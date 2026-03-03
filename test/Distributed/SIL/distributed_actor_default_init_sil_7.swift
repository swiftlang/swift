// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -Xllvm -sil-print-types -emit-sil -verify -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_in_compiler

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

class SomeClass {}

enum Err : Error {
  case blah
}

func getSomeClass() throws -> SomeClass { throw Err.blah }
func getSystem() throws -> FakeActorSystem { throw Err.blah }

distributed actor MyDistActor {
  var someField: SomeClass

  init() throws {
    do {
      actorSystem = try getSystem()
    } catch {
      actorSystem = FakeActorSystem()
    }
    someField = try getSomeClass()
  }

// CHECK:  sil hidden @$s14default_deinit11MyDistActorCACyKcfc : $@convention(method) (@owned MyDistActor) -> (@owned MyDistActor, @error any Error) {
// CHECK:  bb0([[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:    try_apply {{%[0-9]+}}() : $@convention(thin) () -> (@owned FakeActorSystem, @error any Error), normal [[SYSTEM_SUCCESS_BB:bb[0-9]+]], error [[SYSTEM_ERROR_BB:bb[0-9]+]]

// CHECK:  [[SYSTEM_SUCCESS_BB]]([[SYSTEM_VAL:%[0-9]+]] : $FakeActorSystem):
                // *** save system ***
// CHECK:    [[TP_FIELD1:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    store [[SYSTEM_VAL]] to [[TP_FIELD1]] : $*FakeActorSystem
                // *** obtain an identity ***
// CHECK:    [[TP_FIELD2:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[RELOADED_SYS1:%[0-9]+]] = load [[TP_FIELD2]] : $*FakeActorSystem
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8assignIDyAA0C7AddressVxm0B00bC0RzAF0G0RtzlF
// CHECK:    [[ID:%[0-9]+]] = apply [[ASSIGN_ID_FN]]<MyDistActor>([[SELF_METATYPE]], {{.*}}
                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    copy_addr {{.*}} to [init] [[ID_FIELD]] : $*ActorAddress
// CHECK-NOT: apply
// CHECK:    br [[JOIN_PT:bb[0-9]+]]

// CHECK:  [[JOIN_PT]]:
// CHECK:    try_apply {{.*}}() : $@convention(thin) () -> (@owned SomeClass, @error any Error), normal [[CLASS_SUCCESS_BB:bb[0-9]+]], error [[CLASS_ERROR_BB:bb[0-9]+]]

// CHECK:  [[CLASS_SUCCESS_BB]]{{.*}}:
// CHECK:    store {{.*}} to {{.*}} : $*SomeClass
                // *** invoke actorReady ***
// CHECK:    [[TP_FIELD3:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[RELOADED_SYS2:%[0-9]+]] = load [[TP_FIELD3]] : $*FakeActorSystem
// CHECK:    [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF
// CHECK:    = apply [[READY_FN]]<MyDistActor>([[SELF]], {{.*}}
// CHECK:    return [[SELF]] : $MyDistActor

// CHECK:  [[SYSTEM_ERROR_BB]]{{.*}}:
// CHECK:    function_ref @$s27FakeDistributedActorSystems0aC6SystemVACycfC
// CHECK:    store {{.*}} to {{.*}} : $*FakeActorSystem
// CHECK:    store {{.*}} to {{.*}} : $*ActorAddress
// CHECK:    br [[JOIN_PT]]

// CHECK:  [[CLASS_ERROR_BB]]{{.*}}:
            // ** deinit the id **
// CHECK:    [[REF_ID_D:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    [[ID_D:%[0-9]+]] = begin_access [deinit] [static] [[REF_ID_D]] : $*ActorAddress
// CHECK:    [[REF_SYS_D:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[ID:%[0-9]+]] = load [[ID_D]] : $*ActorAddress
// CHECK:    [[SYS:%[0-9]+]] = load [[REF_SYS_D]] : $*FakeActorSystem
// CHECK:    [[RESIGN_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8resignIDyyAA0C7AddressVF
// CHECK:    = apply [[RESIGN_FN]]([[ID]], [[SYS]]) : $@convention(method) (@guaranteed ActorAddress, @guaranteed FakeActorSystem) -> ()
// CHECK:    destroy_addr [[ID_D]] : $*ActorAddress
// CHECK:    end_access [[ID_D]] : $*ActorAddress
            // ** deinit the system **
// CHECK:    [[REF_SYS_D2:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[SYSTEM_ACC:%[0-9]+]] = begin_access [deinit] [static] [[REF_SYS_D2]] : $*FakeActorSystem
// CHECK:    destroy_addr [[SYSTEM_ACC]] : $*FakeActorSystem
// CHECK:    end_access [[SYSTEM_ACC]] : $*FakeActorSystem
// CHECK:    [[EI:%.*]] = end_init_let_ref [[SELF]]
// CHECK:    builtin "destroyDefaultActor"([[EI]] : $MyDistActor) : $()
// CHECK:    dealloc_partial_ref [[EI]]
// CHECK:    throw


}

