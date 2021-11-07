// RUN: %target-swift-frontend -module-name test -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --enable-var-scope --dump-input=fail --implicit-check-not=actorReady --implicit-check-not=resignIdentity --implicit-check-not=hop_to_executor
// REQUIRES: concurrency
// REQUIRES: distributed

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import _Distributed

/// Use the existential wrapper as the default actor transport.
typealias DefaultActorTransport = AnyActorTransport

class SomeClass {}

enum Err : Error {
  case blah
}

distributed actor MyDistActor {
  var localOnlyField: SomeClass

  init(transport_sync: AnyActorTransport) {
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL:  sil hidden @$s4test11MyDistActorC14transport_syncAC12_Distributed03AnyD9TransportV_tcfc : $@convention(method) (@in AnyActorTransport, @owned MyDistActor) -> @owned MyDistActor {
// CHECK:  bb0([[TPORT:%[0-9]+]] : $*AnyActorTransport, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
                // *** save transport ***
// CHECK:    [[TP_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:    copy_addr [[TPORT]] to [initialization] [[TP_FIELD]] : $*AnyActorTransport // id: %6
                // *** obtain an identity ***
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ID_STACK:%[0-9+]+]] = alloc_stack $AnyActorIdentity
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> Self.Identity
// CHECK:    = apply [[ASSIGN_ID_FN]]<AnyActorTransport, MyDistActor>([[ID_STACK]], [[SELF_METATYPE]], [[TPORT]])
                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    copy_addr [[ID_STACK]] to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
                // *** save user-defined property ***
// CHECK:    store {{%[0-9]+}} to {{%[0-9]+}} : $*SomeClass
                // *** invoke actorReady ***
// CHECK:    [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act) -> ()
// CHECK:    = apply [[READY_FN]]<AnyActorTransport, MyDistActor>([[SELF]], [[TPORT]])
                // *** clean-ups ***
// CHECK:    dealloc_stack [[ID_STACK]] : $*AnyActorIdentity
// CHECK:    destroy_addr [[TPORT]] : $*AnyActorTransport
// CHECK:    return [[SELF]] : $MyDistActor
// CHECK:  } // end sil function '$s4test11MyDistActorC14transport_syncAC12_Distributed03AnyD9TransportV_tcfc'



  init?(transport_sync_fail: AnyActorTransport, cond: Bool) {
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: sil hidden @$s4test11MyDistActorC19transport_sync_fail4condACSg12_Distributed03AnyD9TransportV_Sbtcfc : $@convention(method) (@in AnyActorTransport, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
// CHECK: bb0([[TPORT:%[0-9]+]] : $*AnyActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   copy_addr [[TPORT]] to [initialization] [[TPORT_FIELD]] : $*AnyActorTransport
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

// CHECK: [[SUCCESS_BB]]:
// CHECK:   [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady
// CHECK:   = apply [[READY_FN]]
// CHECK:   br [[RET_BB:bb[0-9]+]]

// CHECK: [[FAIL_BB]]:
// CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.resignIdentity
// CHECK:   = apply [[RESIGN_FN]]
// CHECK:   builtin "destroyDefaultActor"
// CHECK:   br [[RET_BB]]

// CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
// CHECK:   return
// CHECK: } // end sil function '$s4test11MyDistActorC19transport_sync_fail4condACSg12_Distributed03AnyD9TransportV_Sbtcfc'



  init?(transport_async_fail: AnyActorTransport, cond: Bool) async {
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

  // CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorC20transport_async_fail4condACSg12_Distributed03AnyD9TransportV_SbtYacfc : $@convention(method) @async (@in AnyActorTransport, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
  // CHECK: bb0([[TPORT:%[0-9]+]] : $*AnyActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
  // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

  // CHECK: [[SUCCESS_BB]]:
  // CHECK:   hop_to_executor {{%[0-9]+}}
  // CHECK:   [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady
  // CHECK:   = apply [[READY_FN]]
  // CHECK:   br [[RET_BB:bb[0-9]+]]

  // CHECK: [[FAIL_BB]]:
  // CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.resignIdentity
  // CHECK:   = apply [[RESIGN_FN]]
  // CHECK:   builtin "destroyDefaultActor"
  // CHECK:   br [[RET_BB]]

  // CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
  // CHECK:   return
  // CHECK: } // end sil function '$s4test11MyDistActorC20transport_async_fail4condACSg12_Distributed03AnyD9TransportV_SbtYacfc'



  init?(transport_async_fail_throws: AnyActorTransport, cond: Bool) async throws {
    guard cond else { throw Err.blah }
    self.localOnlyField = SomeClass()
  }

  // CHECK-LABEL: sil hidden @$s4test11MyDistActorC27transport_async_fail_throws4condACSg12_Distributed03AnyD9TransportV_SbtYaKcfc : $@convention(method) @async (@in AnyActorTransport, Bool, @owned MyDistActor) -> (@owned Optional<MyDistActor>, @error Error) {
  // CHECK: bb0([[TPORT:%[0-9]+]] : $*AnyActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
  // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

  // CHECK: [[SUCCESS_BB]]:
  // CHECK:   hop_to_executor {{%[0-9]+}}
  // CHECK:   [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady
  // CHECK:   = apply [[READY_FN]]
  // CHECK:   br [[RET_BB:bb[0-9]+]]

  // CHECK: [[FAIL_BB]]:
  // CHECK:   [[RESIGN_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.resignIdentity
  // CHECK:   = apply [[RESIGN_FN]]
  // CHECK:   builtin "destroyDefaultActor"
  // CHECK:   throw {{%[0-9]+}} : $Error

  // CHECK: [[RET_BB]]:
  // CHECK:   return
  // CHECK: } // end sil function '$s4test11MyDistActorC27transport_async_fail_throws4condACSg12_Distributed03AnyD9TransportV_SbtYaKcfc'



  init(transport_async: AnyActorTransport, cond: Bool) async {
    if cond {
      self.localOnlyField = SomeClass()
    }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: sil hidden @$s4test11MyDistActorC15transport_async4condAC12_Distributed03AnyD9TransportV_SbtYacfc : $@convention(method) @async (@in AnyActorTransport, Bool, @owned MyDistActor) -> @owned MyDistActor {
// CHECK: bb0([[TPORT:%[0-9]+]] : $*AnyActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   copy_addr [[TPORT]] to [initialization] [[TPORT_FIELD]] : $*AnyActorTransport
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

// CHECK: [[TRUE_BB]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady
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
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $AnyActorTransport, #ActorTransport.actorReady
// CHECK-NEXT:   = apply [[READY_FN]]
// CHECK:        return
// CHECK: } // end sil function '$s4test11MyDistActorC15transport_async4condAC12_Distributed03AnyD9TransportV_SbtYacfc'



// Acknowledge that the deinit has an actorReady call. We cover deinits more in another test.
// CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject {
// CHECK:   #ActorTransport.resignIdentity
// CHECK: } // end sil function '$s4test11MyDistActorCfd'


}
