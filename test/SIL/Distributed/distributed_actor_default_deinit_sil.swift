// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {
}

// ==== ------------------------------------------------------------------------
// ==== deinit must invoke transport.resignIdentity() when it is local

// CHECK: // SimpleEmptyDistributedActor.deinit
// sil hidden [available 12.0] @$s36distributed_actor_default_deinit_sil27SimpleEmptyDistributedActorCfd : $@convention(method) (@guaranteed SimpleEmptyDistributedActor) -> @owned Builtin.NativeObject {
// CHECK: [[SELF:%[0-9]+]] "self"
// CHECK: bb0(%0 : $SimpleEmptyDistributedActor):
// CHECK:  debug_value %0 : $SimpleEmptyDistributedActor, let, name "self", argno 1 
// CHECK:  [[ID_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.id
// CHECK:  [[TRANSPORT_ADDR:%[0-9]+]] = ref_element_addr %0 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport
// CHECK:  %4 = init_existential_ref %0 : $SimpleEmptyDistributedActor : $SimpleEmptyDistributedActor, $AnyObject
// CHECK:  // function_ref swift_distributed_actor_is_remote
// CHECK:  %5 = function_ref @swift_distributed_actor_is_remote : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  %6 = apply %5(%4) : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:  %7 = struct_extract %6 : $Bool, #Bool._value
// CHECK:  cond_br %7, [[REMOTE_BB:bb[0-9]+]], [[LOCAL_BB:bb[0-9]+]]

// If local...
// CHECK: [[LOCAL_BB]]:
// CHECK:  %9 = open_existential_addr immutable_access %3 : $*ActorTransport to $*@opened({{.*}}) ActorTransport
// CHECK:  %10 = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.resignIdentity : <Self where Self : ActorTransport> (Self) -> (AnyActorIdentity) -> (), %9 : $*@opened({{.*}}) ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport> (@in_guaranteed AnyActorIdentity, @in_guaranteed τ_0_0) -> ()
// CHECK:  %11 = apply %10<@opened({{.*}}) ActorTransport>(%2, %9) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport> (@in_guaranteed AnyActorIdentity, @in_guaranteed τ_0_0) -> () 
// CHECK:  br bb3

// If remote...
// CHECK: [[REMOTE_BB]]:
// CHECK:  br bb3

// Finish up the destroying...
// CHECK: bb3:
// CHECK:  destroy_addr [[TRANSPORT_ADDR]] : $*ActorTransport
// CHECK:  destroy_addr [[ID_ADDR]] : $*AnyActorIdentity
// CHECK:  [[_:%[0-9]+]] = builtin "destroyDefaultActor"(%0 : $SimpleEmptyDistributedActor) : $()
// CHECK:  [[SELF:%[0-9]+]] = unchecked_ref_cast %0 : $SimpleEmptyDistributedActor to $Builtin.NativeObject
// CHECK:  return [[SELF]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s36distributed_actor_default_deinit_sil27SimpleEmptyDistributedActorCfd'
