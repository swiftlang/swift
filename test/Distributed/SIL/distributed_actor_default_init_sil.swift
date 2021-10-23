// RUN: %target-swift-frontend -module-name test -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --enable-var-scope --dump-input=fail --implicit-check-not=actorReady --implicit-check-not=resignIdentity --implicit-check-not=hop_to_executor
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: OS=macosx

import _Distributed

class SomeClass {}

@available(macOS 12, *)
distributed actor MyDistActor {
  var localOnlyField: SomeClass

  init(transport_sync: ActorTransport) {
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL:  sil hidden{{.*}} @$s4test11MyDistActorC14transport_syncAC12_Distributed0D9Transport_p_tcfc : $@convention(method) (@in ActorTransport, @owned MyDistActor) -> @owned MyDistActor {
// CHECK:  bb0([[TPORT:%[0-9]+]] : $*ActorTransport, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:    builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
                // *** save transport ***
// CHECK:    [[TP_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:    copy_addr [[TPORT]] to [initialization] [[TP_FIELD]] : $*ActorTransport // id: %6
                // *** obtain an identity ***
// CHECK:    [[TPORT_OPEN:%[0-9]+]] = open_existential_addr immutable_access [[TPORT]]
// CHECK:    [[SELF_METATYPE:%[0-9]+]] = metatype $@thick MyDistActor.Type
// CHECK:    [[ASSIGN_ID_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, [[TPORT_OPEN]]
// CHECK:    [[ID_STACK:%[0-9+]+]] = alloc_stack $AnyActorIdentity
// CHECK:    = apply [[ASSIGN_ID_FN]]<@opened({{.*}}) ActorTransport, MyDistActor>([[ID_STACK]], [[SELF_METATYPE]], [[TPORT_OPEN]])
                // *** save identity ***
// CHECK:    [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:    copy_addr [[ID_STACK]] to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
                // *** save user-defined property ***
// CHECK:    store {{%[0-9]+}} to {{%[0-9]+}} : $*SomeClass
                // *** invoke actorReady ***
// CHECK:    [[TPORT_OPEN:%[0-9]+]] = open_existential_addr immutable_access [[TPORT]]
// CHECK:    [[READY_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.actorReady : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act) -> (), [[TPORT_OPEN]]
// CHECK:    = apply [[READY_FN]]<@opened({{.*}}) ActorTransport, MyDistActor>([[SELF]], [[TPORT_OPEN]])
                // *** clean-ups ***
// CHECK:    dealloc_stack [[ID_STACK]] : $*AnyActorIdentity
// CHECK:    destroy_addr [[TPORT]] : $*ActorTransport
// CHECK:    return [[SELF]] : $MyDistActor
// CHECK:  } // end sil function '$s4test11MyDistActorC14transport_syncAC12_Distributed0D9Transport_p_tcfc'



  init?(transport_sync_fail: ActorTransport, cond: Bool) {
    guard cond else { return nil }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorC19transport_sync_fail4condACSg12_Distributed0D9Transport_p_Sbtcfc : $@convention(method) (@in ActorTransport, Bool, @owned MyDistActor) -> @owned Optional<MyDistActor> {
// CHECK: bb0([[TPORT:%[0-9]+]] : $*ActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   copy_addr [[TPORT]] to [initialization] [[TPORT_FIELD]] : $*ActorTransport
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

// CHECK: [[SUCCESS_BB]]:
// CHECK:   [[READY_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.actorReady
// CHECK:   = apply [[READY_FN]]
// CHECK:   br [[RET_BB:bb[0-9]+]]

// CHECK: [[FAIL_BB]]:
// CHECK:   builtin "destroyDefaultActor"
// CHECK:   br [[RET_BB]]

// CHECK: [[RET_BB]]({{%[0-9]+}} : $Optional<MyDistActor>):
// CHECK:   return
// CHECK: } // end sil function '$s4test11MyDistActorC19transport_sync_fail4condACSg12_Distributed0D9Transport_p_Sbtcfc'



  init(transport_async: ActorTransport, cond: Bool) async {
    if cond {
      self.localOnlyField = SomeClass()
    }
    self.localOnlyField = SomeClass()
  }

// CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorC15transport_async4condAC12_Distributed0D9Transport_p_SbtYacfc : $@convention(method) @async (@in ActorTransport, Bool, @owned MyDistActor) -> @owned MyDistActor {
// CHECK: bb0([[TPORT:%[0-9]+]] : $*ActorTransport, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   builtin "initializeDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[TPORT_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   copy_addr [[TPORT]] to [initialization] [[TPORT_FIELD]] : $*ActorTransport
// CHECK:   [[ID_FIELD:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   copy_addr {{%[0-9]+}} to [initialization] [[ID_FIELD]] : $*AnyActorIdentity
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[COND]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

// CHECK: [[TRUE_BB]]:
// CHECK:        hop_to_executor [[SELF]] : $MyDistActor
// CHECK-NEXT:   open_existential_addr immutable_access {{%[0-9]+}} : $*ActorTransport
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.actorReady
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
// CHECK-NEXT:   open_existential_addr immutable_access {{%[0-9]+}} : $*ActorTransport
// CHECK-NEXT:   [[READY_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.actorReady
// CHECK-NEXT:   = apply [[READY_FN]]
// CHECK:        return
// CHECK: } // end sil function '$s4test11MyDistActorC15transport_async4condAC12_Distributed0D9Transport_p_SbtYacfc'



// Acknowledge that the deinit has an actorReady call. We cover deinits more in another test.
// CHECK-LABEL: sil hidden{{.*}} @$s4test11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject {
// CHECK:   #ActorTransport.resignIdentity
// CHECK: } // end sil function '$s4test11MyDistActorCfd'


}


