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

  init(system_sync: FakeActorSystem) {
    self.localOnlyField = SomeClass()
    self.actorSystem = system_sync
  }

// CHECK-LABEL: // MyDistActor.init(system_sync:)
// CHECK:  sil hidden @$s14default_deinit11MyDistActorC11system_syncAC015FakeDistributedE7Systems0hE6SystemV_tcfc : $@convention(method) (@owned FakeActorSystem, @owned MyDistActor) -> @owned MyDistActor {
// CHECK:  bb0([[SYS_PARAM:%[0-9]+]] : $FakeActorSystem, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)

                // *** save user-defined property ***
// CHECK:    store {{%[0-9]+}} to {{%[0-9]+}} : $*SomeClass

                // *** save system ***
// CHECK:    [[TP_FIELD1:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    store [[SYS_PARAM]] to [[TP_FIELD1]] : $*FakeActorSystem

                // *** obtain an identity ***
// CHECK:    [[TP_FIELD2:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[RELOADED_SYS1:%[0-9]+]] = load [[TP_FIELD2]] : $*FakeActorSystem
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8assignIDyAA0C7AddressVxm0B00bC0RzAF0G0RtzlF
// CHECK:    [[ID:%[0-9]+]] = apply [[ASSIGN_ID_FN]]<MyDistActor>([[SELF_METATYPE]],

                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    copy_addr {{.*}} to [init] [[ID_FIELD]] : $*ActorAddress

                // *** invoke actorReady ***
// CHECK:    [[TP_FIELD3:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    [[RELOADED_SYS2:%[0-9]+]] = load [[TP_FIELD3]] : $*FakeActorSystem
// CHECK:    [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF
// CHECK:    = apply [[READY_FN]]<MyDistActor>([[SELF]],

// CHECK:  } // end sil function '$s14default_deinit11MyDistActorC11system_syncAC015FakeDistributedE7Systems0hE6SystemV_tcfc'

}
