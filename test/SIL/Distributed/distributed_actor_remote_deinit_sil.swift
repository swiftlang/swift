// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

class SomeClass {}

@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {
  let localOnlyField: SomeClass

  init(field: SomeClass, transport: ActorTransport) {
    self.localOnlyField = field
  }
}

// ==== ------------------------------------------------------------------------
// ==== Check that a normal local only actor is left unchanged

// ==== deinit must have the extra "if remote..." path emitted for the
// distributed actor only. That path will not attempt deallocating the
// `localOnly...` fields, since they were never initialized and have no storage.

// CHECK: //  SimpleEmptyDistributedActor.deinit
// CHECK: sil hidden{{.*}} @$s35distributed_actor_remote_deinit_sil27SimpleEmptyDistributedActorCfd : $@convention(method) (@guaranteed SimpleEmptyDistributedActor) -> @owned Builtin.NativeObject {
// CHECK: //  [[SELF:%[0-9]+]] "self"
// CHECK: bb0(%0 : $SimpleEmptyDistributedActor):
// CHECK-NEXT:  debug_value [[SELF]] : $SimpleEmptyDistributedActor, let, name "self", argno 1, implicit
// CHECK-NEXT:  [[IDENTITY_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.id
// CHECK-NEXT:  [[TRANSPORT_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport
// CHECK:  [[SELF_1:%[0-9]+]] = init_existential_ref %0 : $SimpleEmptyDistributedActor : $SimpleEmptyDistributedActor, $AnyObject
// CHECK:  //  function_ref swift_distributed_actor_is_remote
// CHECK:  [[IS_REMOTE_FN_1:%[0-9]+]] = function_ref @swift_distributed_actor_is_remote : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  [[IS_REMOTE_FN_RES_1:%[0-9]+]] = apply [[IS_REMOTE_FN_1]]([[SELF_1]]) : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  [[IS_REMOTE_BOOL_1:%[0-9]+]] = struct_extract [[IS_REMOTE_FN_RES_1]] : $Bool, #Bool._value
// CHECK:  cond_br [[IS_REMOTE_BOOL_1]], bb2, bb1

// CHECK: bb1: // Preds: bb0
// CHECK:  %9 = open_existential_addr immutable_access %3 : $*ActorTransport to $*@opened("{{.*}}") ActorTransport // users: %11, %11, %10
// CHECK:  %10 = witness_method $@opened("{{.*}}") ActorTransport, #ActorTransport.resignIdentity : <Self where Self : ActorTransport> (Self) -> (AnyActorIdentity) -> (), %9 : $*@opened("{{.*}}") ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport> (@in_guaranteed AnyActorIdentity, @in_guaranteed τ_0_0) -> () // type-defs: %9; user: %11
// CHECK:  %11 = apply %10<@opened("{{.*}}") ActorTransport>(%2, %9) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport> (@in_guaranteed AnyActorIdentity, @in_guaranteed τ_0_0) -> () // type-defs: %9
// CHECK:  br bb3 // id: %12

// CHECK: bb2: // Preds: bb0
// CHECK:  br bb3 // id: %13

// CHECK: bb3: // Preds: bb1 bb2
// CHECK:  %14 = init_existential_ref %0 : $SimpleEmptyDistributedActor : $SimpleEmptyDistributedActor, $AnyObject // user: %15
// CHECK:  %15 = apply %5(%14) : $@convention(thin) (@guaranteed AnyObject) -> Bool // user: %16
// CHECK:  %16 = struct_extract %15 : $Bool, #Bool._value // user: %17
// CHECK:  cond_br %16, bb4, bb5 // id: %17

// CHECK: bb4: // Preds: bb3
// CHECK:  br bb6 // id: %18

// CHECK: bb5: // Preds: bb3
// CHECK:  %19 = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.localOnlyField // user: %20
// CHECK:  %20 = load %19 : $*SomeClass // user: %21
// CHECK:  strong_release %20 : $SomeClass // id: %21
// CHECK:  br bb6 // id: %22

// CHECK: bb6: // Preds: bb5 bb4
// CHECK:  destroy_addr %3 : $*ActorTransport // id: %23
// CHECK:  destroy_addr %2 : $*AnyActorIdentity // id: %24
// CHECK:  %25 = builtin "destroyDefaultActor"(%0 : $SimpleEmptyDistributedActor) : $()
// CHECK:  %26 = unchecked_ref_cast %0 : $SimpleEmptyDistributedActor to $Builtin.NativeObject // user: %27
// CHECK:  return %26 : $Builtin.NativeObject // id: %27
// CHECK: } // end sil function '$s35distributed_actor_remote_deinit_sil27SimpleEmptyDistributedActorCfd'
