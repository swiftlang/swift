// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

class SomeClass {}

@available(macOS 12, *)
distributed actor MyDistActor {
  let localOnlyField: SomeClass

  init(transport: ActorTransport) {
    self.localOnlyField = SomeClass()
  }
}

// MARK: distributed actor check

// This test checks that we resign the identity for local deallocations,
// destroy only the correct stored properties whether remote or local, and also
// destroy the executor.

// CHECK-LABEL: sil hidden{{.*}} @$s14default_deinit11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject {
// CHECK: bb0([[SELF:%[0-9]+]] : $MyDistActor):
// CHECK:   [[EXI_SELF:%[0-9]+]] = init_existential_ref [[SELF]] : $MyDistActor
// CHECK:   [[IS_REMOTE_FN:%[0-9]+]] = function_ref @swift_distributed_actor_is_remote
// CHECK:   [[IS_REMOTE:%[0-9]+]] = apply [[IS_REMOTE_FN]]([[EXI_SELF]])
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[IS_REMOTE]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[REMOTE_BB:bb[0-9]+]], [[LOCAL_BB:bb[0-9]+]]

// *** If local... invoke transport.resignIdentity()
// CHECK: [[LOCAL_BB]]:
// CHECK:   [[ID_REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   [[TPORT_REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   [[TPORT_OPENED:%[0-9]+]] = open_existential_addr immutable_access [[TPORT_REF]]
// CHECK:   [[RESIGN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.resignIdentity : <Self where Self : ActorTransport> (Self) -> (AnyActorIdentity) -> (), [[TPORT_OPENED]]
// CHECK:   apply [[RESIGN]]<@opened({{.*}}) ActorTransport>([[ID_REF]], [[TPORT_OPENED]])
// CHECK:   br [[CONTINUE:bb[0-9]+]]

// *** If remote...
// CHECK: [[REMOTE_BB]]:
// CHECK:  br [[CONTINUE]]

// Now we deallocate stored properties, and how we do that depends again on
// being remote or local. Default code emission does another is_remote test,
// so we check for that here and leave tail-merging to the optimizer, for now.
// CHECK: [[CONTINUE]]:
            // *** this is entirely copy-pasted from the first check in bb0 ***
// CHECK:   [[EXI_SELF:%[0-9]+]] = init_existential_ref [[SELF]] : $MyDistActor
// CHECK:   [[IS_REMOTE_FN:%[0-9]+]] = function_ref @swift_distributed_actor_is_remote
// CHECK:   [[IS_REMOTE:%[0-9]+]] = apply [[IS_REMOTE_FN]]([[EXI_SELF]])
// CHECK:   [[RAW_BOOL:%[0-9]+]] = struct_extract [[IS_REMOTE]] : $Bool, #Bool._value
// CHECK:   cond_br [[RAW_BOOL]], [[REMOTE_BB_DEALLOC:bb[0-9]+]], [[LOCAL_BB_DEALLOC:bb[0-9]+]]

// *** only destroy the id and transport if remote ***
// CHECK: [[REMOTE_BB_DEALLOC]]:
            // *** destroy transport ***
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*ActorTransport
// CHECK:   end_access [[ACCESS]]
            // *** destroy identity ***
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*AnyActorIdentity
// CHECK:   end_access [[ACCESS]]
// CHECK:   br [[AFTER_DEALLOC:bb[0-9]+]]

// *** destroy everything if local ***
// CHECK: [[LOCAL_BB_DEALLOC]]:
            // *** destroy the user-defined field ***
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.localOnlyField
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*SomeClass
// CHECK:   end_access [[ACCESS]]
            // *** the rest of this part is identical to the remote case ***
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.actorTransport
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*ActorTransport
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $MyDistActor, #MyDistActor.id
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*AnyActorIdentity
// CHECK:   end_access [[ACCESS]]
// CHECK:   br [[AFTER_DEALLOC]]

// CHECK: [[AFTER_DEALLOC]]:
// CHECK:   builtin "destroyDefaultActor"([[SELF]] : $MyDistActor)
// CHECK:   [[CAST:%[0-9]+]] = unchecked_ref_cast [[SELF]]
// CHECK:   return [[CAST]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s14default_deinit11MyDistActorCfd'


// MARK: local actor check

@available(macOS 12, *)
actor SimpleActor {
  let someField: SomeClass
  init() {
    self.someField = SomeClass()
  }
}

// additionally, we add basic coverage for a non-distributed actor's deinit


// CHECK-LABEL: sil hidden{{.*}} @$s14default_deinit11SimpleActorCfd : $@convention(method) (@guaranteed SimpleActor) -> @owned Builtin.NativeObject {
// CHECK: bb0([[SELF:%[0-9]+]] : $SimpleActor):
// CHECK:   [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleActor, #SimpleActor.someField
// CHECK:   [[ACCESS:%[0-9]+]] = begin_access [deinit] [static] [[REF]]
// CHECK:   destroy_addr [[ACCESS]] : $*SomeClass
// CHECK:   end_access [[ACCESS]]
// CHECK:   builtin "destroyDefaultActor"([[SELF]] : $SimpleActor)
// CHECK:   [[CAST:%[0-9]+]] = unchecked_ref_cast [[SELF]]
// CHECK:   return [[CAST]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s14default_deinit11SimpleActorCfd'


