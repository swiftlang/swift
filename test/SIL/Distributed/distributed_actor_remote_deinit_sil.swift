// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --dump-input=fail

import _Distributed

class SomeClass {}

@available(SwiftStdlib 5.5, *)
actor SimpleActor {
  let someFieldInLocalActor: SomeClass
  init(field: SomeClass) {
    self.someFieldInLocalActor = field
  }
}


@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {
  let localOnlyField: SomeClass

  init(field: SomeClass, transport: ActorTransport) {
    self.localOnlyField = field
  }
}

// ==== ------------------------------------------------------------------------
// ==== Check that a normal local only actor is left unchanged

// CHECK: // SimpleActor.deinit
// CHECK: sil hidden{{.*}} @$s35distributed_actor_remote_deinit_sil11SimpleActorCfd : $@convention(method) (@guaranteed SimpleActor) -> @owned Builtin.NativeObject {
// CHECK: // %0 "self" // users: %6, %5, %2, %1
// CHECK: bb0(%0 : $SimpleActor):
// CHECK:  debug_value %0 : $SimpleActor, let, name "self", argno 1, implicit // id: %1
// CHECK:  %2 = ref_element_addr %0 : $SimpleActor, #SimpleActor.someFieldInLocalActor // user: %3
// CHECK:  %3 = load %2 : $*SomeClass // user: %4
// CHECK:  strong_release %3 : $SomeClass // id: %4
// CHECK:  %5 = builtin "destroyDefaultActor"(%0 : $SimpleActor) : $()
// CHECK:  %6 = unchecked_ref_cast %0 : $SimpleActor to $Builtin.NativeObject // user: %7
// CHECK:  return %6 : $Builtin.NativeObject // id: %7
// CHECK: } // end sil function '$s35distributed_actor_remote_deinit_sil11SimpleActorCfd'




// ==== deinit must have the extra "if remote..." path emitted for the
// distributed actor only. That path will not attempt deallocating the
// `localOnly...` fields, since they were never initialized and have no storage.

// CHECK: //  SimpleEmptyDistributedActor.deinit
// sil hidden [available 12.0] @$s35distributed_actor_remote_deinit_sil27SimpleEmptyDistributedActorCfd : $@convention(method) (@guaranteed SimpleEmptyDistributedActor) -> @owned Builtin.NativeObject {
// CHECK: //  [[SELF:%[0-9]+]] "self"
// CHECK: bb0(%0 : $SimpleEmptyDistributedActor):
// CHECK:  debug_value [[SELF]] : $SimpleEmptyDistributedActor, let, name "self", argno 1, implicit
// CHECK:  [[IDENTITY_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.id // users: %13, %24
// CHECK:  [[TRANSPORT_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport
// CHECK:  [[SELF_1:%[0-9]+]] = init_existential_ref %0 : $SimpleEmptyDistributedActor : $SimpleEmptyDistributedActor, $AnyObject
// CHECK:  //  function_ref swift_distributed_actor_is_remote
// CHECK:  [[IS_REMOTE_FN_1:%[0-9]+]] = function_ref @swift_distributed_actor_is_remote : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  [[IS_REMOTE_FN_RES_1:%[0-9]+]] = apply [[IS_REMOTE_FN_1]]([[SELF_1]]) : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  [[IS_REMOTE_BOOL_1:%[0-9]+]] = struct_extract [[IS_REMOTE_FN_RES_1]] : $Bool, #Bool._value
// CHECK:  cond_br [[IS_REMOTE_BOOL_1]], [[BB_CONT_1:bb[0-9]+]], [[BB_RESIGN_DIST_IDENTITY:bb[0-9]+]]

// CHECK: [[BB_CONT_1]]:
// CHECK:  br [[BB_CHECK_REMOTE_OR_LOCAL_MEMBER_DEINIT_TYPE:bb[0-9]+]]

// It was a local actor, so we resign the address:
// CHECK: [[BB_RESIGN_DIST_IDENTITY]]:
//         %11 = open_existential_addr immutable_access %4 : $*ActorTransport to $*@opened({{.*}}) ActorTransport // users: %13, %13, %12
// CHECK:  [[RESIGN_FN:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.resignIdentity
// CHECK:  [[RESIGNED:%[0-9]+]] = apply [[RESIGN_FN]]<@opened({{.*}}) ActorTransport>(%3, %11) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport> (@in_guaranteed AnyActorIdentity, @in_guaranteed τ_0_0) -> ()
// CHECK:  br [[BB_CHECK_REMOTE_OR_LOCAL_MEMBER_DEINIT_TYPE]]

// Check if we must skip destroying local storage because actor was remote
// CHECK: [[BB_CHECK_REMOTE_OR_LOCAL_MEMBER_DEINIT_TYPE]]:
// CHECK:  %15 = init_existential_ref %0 : $SimpleEmptyDistributedActor : $SimpleEmptyDistributedActor, $AnyObject
// CHECK:  %16 = apply %6(%15) : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  %17 = struct_extract %16 : $Bool, #Bool._value // user: %18
// CHECK:  cond_br %17, [[BB_CONT_REMOTE_DONT_DESTROY_LOCAL_MEMBERS:bb[0-9]+]], [[BB_CONT_DESTROY_LOCAL_THEN_INDEPENDENT_MEMBERS:bb[0-9]+]]

// CHECK: [[BB_CONT_REMOTE_DONT_DESTROY_LOCAL_MEMBERS]]:
// CHECK:  br [[BB_DESTROY_INDEPENDENT_MEMBERS:bb[0-9]+]]

// We were a local instance after all, and thus must destroy local properties
// CHECK: [[BB_CONT_DESTROY_LOCAL_THEN_INDEPENDENT_MEMBERS]]:
// CHECK:  [[FIELD_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.localOnlyField
// CHECK:  [[LOAD_FIELD_ADDR:%[0-9]+]] = load [[FIELD_ADDR]] : $*SomeClass
// CHECK:  strong_release [[LOAD_FIELD_ADDR]] : $SomeClass
// CHECK:  br [[BB_DESTROY_INDEPENDENT_MEMBERS]]

// Destroy "distributed nonisolated" fields and the actor itself
// CHECK: [[BB_DESTROY_INDEPENDENT_MEMBERS]]:
// CHECK:  destroy_addr [[IDENTITY_ADDR]] : $*AnyActorIdentity
// CHECK:  destroy_addr [[TRANSPORT_ADDR]] : $*ActorTransport
// CHECK:  {{.*}} = builtin "destroyDefaultActor"(%0 : $SimpleEmptyDistributedActor) : $()
// CHECK:  dealloc_ref [[SELF]] : $SimpleEmptyDistributedActor
// CHECK:  [[EMPTY:%[0-9]+]] = tuple ()
// CHECK:  return [[EMPTY]] : $()
// } // end sil function '$s35distributed_actor_remote_deinit_sil27SimpleEmptyDistributedActorCfD'