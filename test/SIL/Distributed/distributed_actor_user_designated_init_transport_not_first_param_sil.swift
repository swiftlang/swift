// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleDistributedActor {
  let storedFirst: Int
  init(other: Int, notFirstParam transport: ActorTransport) {
    self.storedFirst = other
  }
}

// Check the that the user-defined constructor gained the lifecycle hooks:
// CHECK: // SimpleDistributedActor.init(first:other:notFirstParam)
// sil hidden [available 12.0] @$s34distributed_actor_default_init_sil27SimpleDistributedActorC9transportAC01_H00I9Transport_p_tcfc : $@convention(method) (@in ActorTransport, @owned SimpleDistributedActor) -> @owned SimpleDistributedActor {
// CHECK: [[FIRST:%[0-9]+]] "first"
// CHECK: [[OTHER:%[0-9]+]] "other"
// CHECK: [[TRANSPORT:%[0-9]+]] "notFirstParam"
// CHECK: [[SELF:%[0-9]+]] "self"
// CHECK: bb0([[TRANSPORT]] : $*ActorTransport, [[OTHER]] : $Int, [[SELF]] : $SimpleDistributedActor):
// CHECK: [[DEFAULT_ACTOR_INIT:%[0-9]+]] = builtin "initializeDefaultActor"([[SELF]] : $SimpleDistributedActor) : $()
// CHECK: [[OPEN_TRANSPORT_EXISTENTIAL:%[0-9]+]] = open_existential_addr immutable_access [[TRANSPORT]] : $*ActorTransport to $*@opened({{.*}}) ActorTransport
// CHECK: [[SELF_METATYPE:%[0-9]+]] = metatype $@thick SimpleDistributedActor.Type
// CHECK: [[ASSIGN_WITNESS_METHOD:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, [[OPEN_TRANSPORT_EXISTENTIAL]] : $*@opened({{.*}}) ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity
// CHECK: [[IDENTITY_STACK_TMP:%[0-9]+]] = alloc_stack $AnyActorIdentity
// CHECK: [[IDENTITY_RETURNED:%[0-9]+]] = apply [[ASSIGN_WITNESS_METHOD]]<@opened({{.*}}) ActorTransport, SimpleDistributedActor>([[IDENTITY_STACK_TMP]], [[SELF_METATYPE]], [[OPEN_TRANSPORT_EXISTENTIAL]]) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity
// CHECK: [[IDENTITY_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleDistributedActor, #SimpleDistributedActor.id
// Store the identity
// CHECK: copy_addr [take] [[IDENTITY_STACK_TMP]] to [initialization] [[IDENTITY_PROPERTY]] : $*AnyActorIdentity
// CHECK: [[TRANSPORT_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleDistributedActor, #SimpleDistributedActor.actorTransport
// Store the transport
// CHECK: copy_addr [[TRANSPORT]] to [initialization] [[TRANSPORT_PROPERTY]] : $*ActorTransport
// CHECK: dealloc_stack [[IDENTITY_STACK_TMP]] : $*AnyActorIdentity
// CHECK: destroy_addr [[TRANSPORT]] : $*ActorTransport
// // While in AST the return was "return null" after SILGen we properly return the self
// CHECK: return [[SELF]] : $SimpleDistributedActor
// } // end sil function '$s34distributed_actor_default_init_sil27SimpleDistributedActorC9transportAC01_H00I9Transport_p_tcfc'
