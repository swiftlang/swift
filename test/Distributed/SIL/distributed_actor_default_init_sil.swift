// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --dump-input=always
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
// CHECK:  sil hidden @$s14default_deinit11MyDistActorC11system_syncAC04FakeE7Systems0hE6SystemV_tcfc : $@convention(method) (FakeActorSystem, @owned MyDistActor) -> @owned MyDistActor {
// CHECK:  bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
                // *** save system ***
// CHECK:    [[TP_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:    store [[SYSTEM]] to [[TP_FIELD]] : $*FakeActorSystem
                // *** obtain an identity ***
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = function_ref @$s16FakeDistributedActorSystems0aB6SystemV8assignIDyAA0B7AddressVxm12_Distributed0hB0RzAF0F0RtzlF : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@thick τ_0_0.Type, FakeActorSystem) -> @owned ActorAddress
// CHECK:    [[ID:%[0-9]+]] = apply [[ASSIGN_ID_FN]]<MyDistActor>([[SELF_METATYPE]], [[SYSTEM]]) : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@thick τ_0_0.Type, FakeActorSystem) -> @owned ActorAddress
                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    store [[ID]] to [[ID_FIELD]] : $*ActorAddress
                // *** save user-defined property ***
// CHECK:    store {{%[0-9]+}} to {{%[0-9]+}} : $*SomeClass
                // *** invoke actorReady ***
// CHECK:    [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady : <Self where Self : DistributedActorSystem><Act where Act : DistributedActor> (Self) -> (Act) -> ()
// CHECK:    = apply [[READY_FN]]<FakeActorSystem, MyDistActor>([[SELF]], [[SYSTEM]])
                // *** clean-ups ***
// CHECK:    destroy_addr [[SYSTEM]] : $*FakeActorSystem
// CHECK:    return [[SELF]] : $MyDistActor
// CHECK:  } // end sil function '$s4test11MyDistActorC14system_syncAC12_Distributed03AnyD9TransportV_tcfc'



  init?(system_sync_fail: FakeActorSystem, cond: Bool) {
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: // MyDistActor.init(system_sync_fail:cond:)
// CHECK: sil hidden @$s14default_deinit11MyDistActorC16system_sync_fail4condACSg04FakeE7Systems0jE6SystemV_Sbtcfc : $@convention(method) (FakeActorSystem, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
// CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:   copy_addr [[SYSTEM]] to [initialization] [[TPORT_FIELD]] : $*FakeActorSystem
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*ActorAddress
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

// CHECK: [[SUCCESS_BB]]:
// CHECK:   [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady
// CHECK:   = apply [[READY_FN]]
// CHECK:   br [[RET_BB:bb[0-9]+]]

// CHECK: [[FAIL_BB]]:
// CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.resignID
// CHECK:   = apply [[RESIGN_FN]]
// CHECK:   builtin "destroyDefaultActor"
// CHECK:   br [[RET_BB]]

// CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
// CHECK:   return
// CHECK: } // end sil function '$s4test11MyDistActorC19system_sync_fail4condACSg12_Distributed03AnyD9TransportV_Sbtcfc'



  init?(system_async_fail: FakeActorSystem, cond: Bool) async {
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

  // CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorC20system_async_fail4condACSg12_Distributed03AnyD9TransportV_SbtYacfc : $@convention(method) @async (@in FakeActorSystem, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
  // CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
  // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

  // CHECK: [[SUCCESS_BB]]:
  // CHECK:   hop_to_executor {{%[0-9]+}}
  // CHECK:   [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady
  // CHECK:   = apply [[READY_FN]]
  // CHECK:   br [[RET_BB:bb[0-9]+]]

  // CHECK: [[FAIL_BB]]:
  // CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.resignID
  // CHECK:   = apply [[RESIGN_FN]]
  // CHECK:   builtin "destroyDefaultActor"
  // CHECK:   br [[RET_BB]]

  // CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
  // CHECK:   return
  // CHECK: } // end sil function '$s4test11MyDistActorC20system_async_fail4condACSg12_Distributed03AnyD9TransportV_SbtYacfc'



  init?(system_async_fail_throws: FakeActorSystem, cond: Bool) async throws {
    guard cond else { throw Err.blah }
    self.localOnlyField = SomeClass()
  }

  // CHECK-LABEL: sil hidden @$s4test11MyDistActorC27system_async_fail_throws4condACSg12_Distributed03AnyD9TransportV_SbtYaKcfc : $@convention(method) @async (@in FakeActorSystem, Bool, @owned MyDistActor) -> (@owned Optional<MyDistActor>, @error Error) {
  // CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
  // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

  // CHECK: [[SUCCESS_BB]]:
  // CHECK:   hop_to_executor {{%[0-9]+}}
  // CHECK:   [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady
  // CHECK:   = apply [[READY_FN]]
  // CHECK:   br [[RET_BB:bb[0-9]+]]

  // CHECK: [[FAIL_BB]]:
  // CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.resignID
  // CHECK:   = apply [[RESIGN_FN]]
  // CHECK:   builtin "destroyDefaultActor"
  // CHECK:   throw {{%[0-9]+}} : $Error

  // CHECK: [[RET_BB]]:
  // CHECK:   return
  // CHECK: } // end sil function '$s4test11MyDistActorC27system_async_fail_throws4condACSg12_Distributed03AnyD9TransportV_SbtYaKcfc'



  init(system_async: FakeActorSystem, cond: Bool) async {
    if cond {
      self.localOnlyField = SomeClass()
    }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: sil hidden @$s4test11MyDistActorC15system_async4condAC12_Distributed03AnyD9TransportV_SbtYacfc : $@convention(method) @async (@in FakeActorSystem, Bool, @owned MyDistActor) -> @owned MyDistActor {
// CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorSystem
// CHECK:   copy_addr [[SYSTEM]] to [initialization] [[TPORT_FIELD]] : $*FakeActorSystem
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*ActorAddress
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

// CHECK: [[TRUE_BB]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady
// CHECK-NEXT:   = apply [[READY_FN]]
// CHECK:        br [[JOIN:bb[0-9]+]]

// CHECK: [[FALSE_BB]]:
// CHECK:   br [[JOIN]]

// CHECK: [[JOIN]]:
// CHECK:   cond_br {{%[0-9]+}}, [[PARTIAL_DEINIT:bb[0-9]+]], [[NO_DEINIT:bb[0-9]+]]

// CHECK: [[PARTIAL_DEINIT]]:
// CHECK:   destroy_addr {{.*}} : $*SomeClass
// CHECK:   br [[CONTINUE:bb[0-9]+]]

// CHECK: [[NO_DEINIT]]:
// CHECK:   br [[CONTINUE]]

// CHECK: [[CONTINUE]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $FakeActorSystem, #DistributedActorSystem.actorReady
// CHECK-NEXT:   = apply [[READY_FN]]
// CHECK:        return
// CHECK: } // end sil function '$s4test11MyDistActorC15system_async4condAC12_Distributed03AnyD9TransportV_SbtYacfc'



// Acknowledge that the deinit has an actorReady call. We cover deinits more in another test.
// CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject {
// CHECK:   #DistributedActorSystem.resignID
// CHECK: } // end sil function '$s4test11MyDistActorCfd'


}
