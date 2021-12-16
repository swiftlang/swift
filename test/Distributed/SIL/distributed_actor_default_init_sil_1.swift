// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

class SomeClass {}

enum Err : Error {
  case blah
}

distributed actor MyDistActor {
  var localOnlyField: SomeClass

  init(system_sync: FakeActorSystem) {
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: // MyDistActor.init(system_sync:)
// CHECK:  sil hidden @$s14default_deinit11MyDistActorC11system_syncAC015FakeDistributedE7Systems0hE6SystemV_tcfc : $@convention(method) (@owned FakeActorSystem, @owned MyDistActor) -> @owned MyDistActor {
// CHECK:  bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
                // *** save system ***
// CHECK:    [[TP_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    store [[SYSTEM]] to [[TP_FIELD]] : $*FakeActorSystem
                // *** obtain an identity ***
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8assignIDyAA0C7AddressVxm01_B00bC0RzAF0G0RtzlF : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@thick τ_0_0.Type, @guaranteed FakeActorSystem) -> @owned ActorAddress
// CHECK:    [[ID:%[0-9]+]] = apply [[ASSIGN_ID_FN]]<MyDistActor>([[SELF_METATYPE]], [[SYSTEM]]) : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@thick τ_0_0.Type, @guaranteed FakeActorSystem) -> @owned ActorAddress
                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    store [[ID]] to [[ID_FIELD]] : $*ActorAddress
                // *** save user-defined property ***
// CHECK:    store {{%[0-9]+}} to {{%[0-9]+}} : $*SomeClass
                // *** invoke actorReady ***
// CHECK:    [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx01_B00bC0RzAA0C7AddressV2IDRtzlF : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @guaranteed FakeActorSystem) -> ()
// CHECK:    = apply [[READY_FN]]<MyDistActor>([[SELF]], [[SYSTEM]])
                // *** clean-ups ***
// CHECK:    release_value [[SYSTEM]] : $FakeActorSystem
// CHECK:    return [[SELF]] : $MyDistActor
// CHECK:  } // end sil function '$s14default_deinit11MyDistActorC11system_syncAC015FakeDistributedE7Systems0hE6SystemV_tcfc'

}
