// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --dump-input=fail
// REQUIRES: concurrency

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleUserDefinedInitDistributedActor {
  init(kappa transport: ActorTransport, other: Int) {}
  init(other: Int, theTransport: ActorTransport) {}
}

// CHECK: // SimpleUserDefinedInitDistributedActor.init(kappa:other:)
// CHECK: sil hidden{{.*}} @$s41distributed_actor_user_transport_init_sil37SimpleUserDefinedInitDistributedActorC5kappa5otherAC01_K00L9Transport_p_Sitcfc : $@convention(method) (@in ActorTransport, Int, @owned SimpleUserDefinedInitDistributedActor) -> @owned SimpleUserDefinedInitDistributedActor {
// CHECK: // %0 "transport" // users: %17, %9, %8, %3
// CHECK: // %1 "other" // user: %4
// CHECK: // %2 "self" // users: %7, %14, %6, %18, %5
// CHECK: bb0(%0 : $*ActorTransport, %1 : $Int, %2 : $SimpleUserDefinedInitDistributedActor):
// CHECK:  debug_value_addr %0 : $*ActorTransport, let, name "transport", argno 1 // id: %3
// CHECK:  debug_value %1 : $Int, let, name "other", argno 2 // id: %4
// CHECK:  debug_value %2 : $SimpleUserDefinedInitDistributedActor, let, name "self", argno 3, implicit // id: %5
// CHECK:  %6 = builtin "initializeDefaultActor"(%2 : $SimpleUserDefinedInitDistributedActor) : $()

// Store the transport
// CHECK:  %7 = ref_element_addr %2 : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.actorTransport // user: %8
// CHECK:  copy_addr %0 to [initialization] %7 : $*ActorTransport // id: %8

// Assign the identity
// CHECK:  %9 = open_existential_addr immutable_access %0 : $*ActorTransport to $*@opened("{{.*}}") ActorTransport // users: %13, %13, %11
// CHECK:  %10 = metatype $@thick SimpleUserDefinedInitDistributedActor.Type // user: %13
// CHECK:  %11 = witness_method $@opened("{{.*}}") ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, %9 : $*@opened("{{.*}}") ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %9; user: %13
// CHECK:  %12 = alloc_stack $AnyActorIdentity // users: %16, %15, %13
// CHECK:  %13 = apply %11<@opened("{{.*}}") ActorTransport, SimpleUserDefinedInitDistributedActor>(%12, %10, %9) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %9

// Store the identity
// CHECK:  %14 = ref_element_addr %2 : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.id // user: %15
// CHECK:  copy_addr [take] %12 to [initialization] %14 : $*AnyActorIdentity // id: %15
// CHECK:  dealloc_stack %12 : $*AnyActorIdentity // id: %16
// CHECK:  destroy_addr %0 : $*ActorTransport // id: %17
// CHECK:  return %2 : $SimpleUserDefinedInitDistributedActor // id: %18
// CHECK: } // end sil function '$s41distributed_actor_user_transport_init_sil37SimpleUserDefinedInitDistributedActorC5kappa5otherAC01_K00L9Transport_p_Sitcfc'


// Even if the transport is in another position, we still locate it by the type
// CHECK: // SimpleUserDefinedInitDistributedActor.init(other:theTransport:)
// CHECK: sil hidden{{.*}} @$s41distributed_actor_user_transport_init_sil37SimpleUserDefinedInitDistributedActorC5other12theTransportACSi_01_K00lO0_ptcfc : $@convention(method) (Int, @in ActorTransport, @owned SimpleUserDefinedInitDistributedActor) -> @owned SimpleUserDefinedInitDistributedActor {
// CHECK: // %0 "other" // user: %3
// CHECK: // %1 "theTransport" // users: %17, %9, %8, %4
// CHECK: // %2 "self" // users: %7, %14, %6, %18, %5
// CHECK: bb0(%0 : $Int, %1 : $*ActorTransport, %2 : $SimpleUserDefinedInitDistributedActor):
// CHECK:  debug_value %0 : $Int, let, name "other", argno 1 // id: %3
// CHECK:  debug_value_addr %1 : $*ActorTransport, let, name "theTransport", argno 2 // id: %4
// CHECK:  debug_value %2 : $SimpleUserDefinedInitDistributedActor, let, name "self", argno 3, implicit // id: %5
// CHECK:  %6 = builtin "initializeDefaultActor"(%2 : $SimpleUserDefinedInitDistributedActor) : $()

// Store the transport
// CHECK:  %7 = ref_element_addr %2 : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.actorTransport // user: %8
// CHECK:  copy_addr %1 to [initialization] %7 : $*ActorTransport // id: %8

// Assign an identity
// CHECK:  %9 = open_existential_addr immutable_access %1 : $*ActorTransport to $*@opened("{{.*}}") ActorTransport // users: %13, %13, %11
// CHECK:  %10 = metatype $@thick SimpleUserDefinedInitDistributedActor.Type // user: %13
// CHECK:  %11 = witness_method $@opened("{{.*}}") ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, %9 : $*@opened("{{.*}}") ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %9; user: %13
// CHECK:  %12 = alloc_stack $AnyActorIdentity // users: %16, %15, %13
// CHECK:  %13 = apply %11<@opened("{{.*}}") ActorTransport, SimpleUserDefinedInitDistributedActor>(%12, %10, %9) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %9

// Store the identity
// CHECK:  %14 = ref_element_addr %2 : $SimpleUserDefinedInitDistributedActor, #SimpleUserDefinedInitDistributedActor.id // user: %15
// CHECK:  copy_addr [take] %12 to [initialization] %14 : $*AnyActorIdentity // id: %15
// CHECK:  dealloc_stack %12 : $*AnyActorIdentity // id: %16
// CHECK:  destroy_addr %1 : $*ActorTransport // id: %17

// While in AST the return was "return null" after SILGen we properly return the self
// CHECK:  return %2 : $SimpleUserDefinedInitDistributedActor // id: %18
// CHECK: } // end sil function '$s41distributed_actor_user_transport_init_sil37SimpleUserDefinedInitDistributedActorC5other12theTransportACSi_01_K00lO0_ptcfc'
