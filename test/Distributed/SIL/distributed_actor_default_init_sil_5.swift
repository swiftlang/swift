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

  init(system_async: FakeActorSystem, cond: Bool) async {
    self.actorSystem = system_async
    if cond {
      self.localOnlyField = SomeClass()
    }
    self.localOnlyField = SomeClass()
  }

// CHECK: sil hidden @$s14default_deinit11MyDistActorC12system_async4condAC015FakeDistributedE7Systems0iE6SystemV_SbtYacfc : $@convention(method) @async (@owned FakeActorSystem, Bool, @sil_isolated @owned MyDistActor) -> @owned MyDistActor {
// CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[SYS_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:   store [[SYSTEM]] to [[SYS_FIELD]] : $*FakeActorSystem
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{.*}} to [init] [[ID_FIELD]] : $*ActorAddress
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

// CHECK: [[TRUE_BB]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK:   // function_ref FakeActorSystem.actorReady<A>(_:)
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF
// CHECK-NEXT:   [[APPLIED:%[0-9]+]] = apply [[READY_FN]]
// CHECK:        br [[JOIN:bb[0-9]+]]

// CHECK: [[FALSE_BB]]:
// CHECK:   br [[JOIN]]

// CHECK: [[JOIN]]({{.*}} : $Builtin.Int3):
// CHECK:   cond_br {{%[0-9]+}}, [[PARTIAL_DEINIT:bb[0-9]+]], [[NO_DEINIT:bb[0-9]+]]

// CHECK: [[PARTIAL_DEINIT]]:
// CHECK:   destroy_addr {{.*}} : $*SomeClass
// CHECK:   br [[CONTINUE:bb[0-9]+]]

// CHECK: [[NO_DEINIT]]:
// CHECK:   br [[CONTINUE]]

// CHECK: [[CONTINUE]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK:   // function_ref FakeActorSystem.actorReady<A>(_:)
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @guaranteed FakeActorSystem) -> ()
// CHECK-NEXT:   = apply [[READY_FN]]
// CHECK:        return
// CHECK: } // end sil function '$s14default_deinit11MyDistActorC12system_async4condAC015FakeDistributedE7Systems0iE6SystemV_SbtYacfc'



// Acknowledge that the deinit has an actorReady call. We cover deinits more in another test.
// CHECK-LABEL: sil hidden @$s14default_deinit11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject {
// CHECK: // function_ref FakeActorSystem.resignID(_:)
// CHECK-NEXT: = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8resignIDyyAA0C7AddressVF : $@convention(method) (@guaranteed ActorAddress, @guaranteed FakeActorSystem) -> ()
// CHECK: } // end sil function '$s14default_deinit11MyDistActorCfd'

}
