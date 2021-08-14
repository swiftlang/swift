// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleUserDefinedInitDistributedActor {
  init(kappa transport: ActorTransport, other: Int) {}
  init(other: Int, theTransport: ActorTransport) {}
}

// CHECK: // SimpleUserDefinedInitDistributedActor.init(kappa:other:)
// CHECK: [[TRANSPORT:%[0-9]+]] "transport"
// CHECK: [[OTHER:%[0-9]+]] "other"
// CHECK: [[SELF:%[0-9]+]] "self"
// CHECK: bb0([[TRANSPORT]] : $*ActorTransport, [[OTHER]] : $Int, [[SELF]] : $SimpleUserDefinedInitDistributedActor):
// CHECK: [[DEFAULT_ACTOR_INIT:%[0-9]+]] = builtin "initializeDefaultActor"([[SELF]] : $SimpleUserDefinedInitDistributedActor) : $()
// CHECK: [[OPEN_TRANSPORT_EXISTENTIAL:%[0-9]+]] = open_existential_addr immutable_access [[TRANSPORT]] : $*ActorTransport to $*@opened({{.*}}) ActorTransport
// CHECK: [[SELF_METATYPE:%[0-9]+]] = metatype $@thick SimpleUserDefinedInitDistributedActor.Type
// CHECK: [[ASSIGN_WITNESS_METHOD:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.assignIdentity
// CHECK: [[IDENTITY_STACK_TMP:%[0-9]+]] = alloc_stack $AnyActorIdentity
// CHECK: [[IDENTITY_RETURNED:%[0-9]+]] = apply [[ASSIGN_WITNESS_METHOD]]<@opened({{.*}}) ActorTransport, SimpleUserDefinedInitDistributedActor>([[IDENTITY_STACK_TMP]], [[SELF_METATYPE]], [[OPEN_TRANSPORT_EXISTENTIAL]]) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity
// CHECK: [[IDENTITY_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.id
// Store the identity
// CHECK: copy_addr [take] [[IDENTITY_STACK_TMP]] to [initialization] [[IDENTITY_PROPERTY]] : $*AnyActorIdentity
// CHECK: [[TRANSPORT_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.actorTransport
// Store the transport
// CHECK: copy_addr [[TRANSPORT]] to [initialization] [[TRANSPORT_PROPERTY]] : $*ActorTransport
// CHECK: dealloc_stack [[IDENTITY_STACK_TMP]] : $*AnyActorIdentity
// CHECK: destroy_addr [[TRANSPORT]] : $*ActorTransport
// // While in AST the return was "return null" after SILGen we properly return the self
// CHECK: return [[SELF]] : $SimpleUserDefinedInitDistributedActor
// CHECK: }

// Even if the transport is in another position, we still locate it by the type
// CHECK: // SimpleUserDefinedInitDistributedActor.init(other:theTransport:)
// CHECK: [[OTHER:%[0-9]+]] "other"
// CHECK: [[TRANSPORT:%[0-9]+]] "theTransport"
// CHECK: [[SELF:%[0-9]+]] "self"
// CHECK: bb0([[OTHER]] : $Int, [[TRANSPORT]] : $*ActorTransport, [[SELF]] : $SimpleUserDefinedInitDistributedActor):
// CHECK: [[DEFAULT_ACTOR_INIT:%[0-9]+]] = builtin "initializeDefaultActor"([[SELF]] : $SimpleUserDefinedInitDistributedActor) : $()
// CHECK: [[OPEN_TRANSPORT_EXISTENTIAL:%[0-9]+]] = open_existential_addr immutable_access [[TRANSPORT]] : $*ActorTransport to $*@opened({{.*}}) ActorTransport
// CHECK: [[SELF_METATYPE:%[0-9]+]] = metatype $@thick SimpleUserDefinedInitDistributedActor.Type
// CHECK: [[ASSIGN_WITNESS_METHOD:%[0-9]+]] = witness_method $@opened({{.*}}) ActorTransport, #ActorTransport.assignIdentity
// CHECK: [[IDENTITY_STACK_TMP:%[0-9]+]] = alloc_stack $AnyActorIdentity
// CHECK: [[IDENTITY_RETURNED:%[0-9]+]] = apply [[ASSIGN_WITNESS_METHOD]]<@opened({{.*}}) ActorTransport, SimpleUserDefinedInitDistributedActor>([[IDENTITY_STACK_TMP]], [[SELF_METATYPE]], [[OPEN_TRANSPORT_EXISTENTIAL]]) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity
// CHECK: [[IDENTITY_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.id
// Store the identity
// CHECK: copy_addr [take] [[IDENTITY_STACK_TMP]] to [initialization] [[IDENTITY_PROPERTY]] : $*AnyActorIdentity
// CHECK: [[TRANSPORT_PROPERTY:%[0-9]+]] = ref_element_addr [[SELF]] : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.actorTransport
// Store the transport
// CHECK: copy_addr [[TRANSPORT]] to [initialization] [[TRANSPORT_PROPERTY]] : $*ActorTransport
// CHECK: dealloc_stack [[IDENTITY_STACK_TMP]] : $*AnyActorIdentity
// CHECK: destroy_addr [[TRANSPORT]] : $*ActorTransport
// // While in AST the return was "return null" after SILGen we properly return the self
// CHECK: return [[SELF]] : $SimpleUserDefinedInitDistributedActor
// CHECK: }

