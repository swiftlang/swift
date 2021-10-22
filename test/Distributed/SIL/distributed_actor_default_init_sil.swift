// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {
}

// CHECK: // SimpleEmptyDistributedActor.init(transport:)
// CHECK: sil hidden{{.*}} @$s34distributed_actor_default_init_sil27SimpleEmptyDistributedActorC9transportAC01_H00I9Transport_p_tcfc : $@convention(method) (@in ActorTransport, @owned SimpleEmptyDistributedActor) -> @owned SimpleEmptyDistributedActor {
// CHECK: // %0 "transport" // users: %15, %7, %6, %2
// CHECK: // %1 "self" // users: %5, %12, %4, %16, %3
// CHECK: bb0(%0 : $*ActorTransport, %1 : $SimpleEmptyDistributedActor):
// CHECK:  debug_value %0 : $*ActorTransport, let, name "transport", argno 1, implicit, expr op_deref // id: %2
// CHECK:  debug_value %1 : $SimpleEmptyDistributedActor, let, name "self", argno 2, implicit // id: %3
// CHECK:  %4 = builtin "initializeDefaultActor"(%1 : $SimpleEmptyDistributedActor) : $()

// Store the transport
// CHECK:  %5 = ref_element_addr %1 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport // user: %6
// CHECK:  copy_addr %0 to [initialization] %5 : $*ActorTransport // id: %6

// Assign an identity
// CHECK:  %7 = open_existential_addr immutable_access %0 : $*ActorTransport to $*@opened("{{.*}}") ActorTransport // users: %11, %11, %9
// CHECK:  %8 = metatype $@thick SimpleEmptyDistributedActor.Type // user: %11
// CHECK:  %9 = witness_method $@opened("{{.*}}") ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, %7 : $*@opened("{{.*}}") ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %7; user: %11
// CHECK:  %10 = alloc_stack $AnyActorIdentity // users: %14, %13, %11
// CHECK:  %11 = apply %9<@opened("{{.*}}") ActorTransport, SimpleEmptyDistributedActor>(%10, %8, %7) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> @out AnyActorIdentity // type-defs: %7

// Store the identity
// CHECK:  %12 = ref_element_addr %1 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.id // user: %13
// CHECK:  copy_addr [take] %10 to [initialization] %12 : $*AnyActorIdentity // id: %13

// CHECK:  dealloc_stack %10 : $*AnyActorIdentity // id: %14
// CHECK:  destroy_addr %0 : $*ActorTransport // id: %15
// CHECK:  return %1 : $SimpleEmptyDistributedActor // id: %16
// CHECK: } // end sil function '$s34distributed_actor_default_init_sil27SimpleEmptyDistributedActorC9transportAC01_H00I9Transport_p_tcfc'
