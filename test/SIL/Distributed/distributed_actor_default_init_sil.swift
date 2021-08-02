// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s -dump-input=always

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {}

//9: @available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
//10: distributed actor SimpleEmptyDistributedActor {
//11:  typealias ID = AnyActorIdentity
//12:  @_distributedActorIndependent @_hasStorage final let actorTransport: ActorTransport { get }
//13:  @objc deinit
//14:  nonisolated var hashValue: Int { get }
//15:  @_distributedActorIndependent @_hasStorage final let id: AnyActorIdentity { get }
//16:  nonisolated init(transport: ActorTransport)
//17: }

// Check the default constructor:
// CHECK: // SimpleEmptyDistributedActor.init(transport:)
// CHECK: [[TRANSPORT:%[0-9]+]] "transport"
// CHECK: [[SELF:%[0-9]+]] "self"

// Store the transport
//  %5 = begin_borrow %4 : $SimpleEmptyDistributedActor        // users: %11, %8
//  %6 = alloc_stack $ActorTransport                // users: %10, %9, %7
//  copy_addr [[TRANSPORT]] to [initialization] %6 : $*ActorTransport // id: %7
//  %8 = ref_element_addr %5 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport // user: %9
//  copy_addr [take] %6 to %8 : $*ActorTransport    // id: %9

// Assign the identity
// CHECK: [[OPEN_TRANSPORT:%[0-9]+]] = open_existential_addr immutable_access [[TRANSPORT]] : $*ActorTransport to $*@opened([[OPENED_TRANSPORT_ID:".*"]]) ActorTransport
// CHECK: [[ASSIGN_WITNESS_M:%[0-9]+]] = witness_method $@opened([[OPENED_TRANSPORT_ID]]) ActorTransport, #ActorTransport.assignIdentity : 
// CHECK: [[ASSIGNED_ID:%[0-9]+]] = apply [[ASSIGN_WITNESS_M]]<@opened([[OPENED_TRANSPORT_ID]]) ActorTransport, @dynamic_self SimpleEmptyDistributedActor>(%15, %14) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> AnyActorIdentity
// Store the identity

// CHECK: XXXXXXXXXX



//// SimpleEmptyDistributedActor.init(transport:)
//sil hidden [ossa] @$s3lol16SimpleEmptyDistributedActorC9transportAcA14ActorTransport_p_tcfc : $@convention(method) (@in ActorTransport, @owned SimpleEmptyDistributedActor) -> @owned SimpleEmptyDistributedActor {
//// [[TRANSPORT]] "transport"                                 // users: %26, %14, %7, %2
//// [[SELF]] "self"                                      // users: %17, %15, %4, %3
//  bb0([[TRANSPORT]] : $*ActorTransport, [[SELF]] : @owned $SimpleEmptyDistributedActor):
//  debug_value_addr [[TRANSPORT]] : $*ActorTransport, let, name "transport", argno 1 // id: %2
//  debug_value [[SELF]] : $SimpleEmptyDistributedActor, let, name "self", argno 2 // id: %3
//  %4 = mark_uninitialized [rootself] [[SELF]] : $SimpleEmptyDistributedActor // users: %25, %24, %12, %5
//  %5 = begin_borrow %4 : $SimpleEmptyDistributedActor        // users: %11, %8
//  %6 = alloc_stack $ActorTransport                // users: %10, %9, %7
//  copy_addr [[TRANSPORT]] to [initialization] %6 : $*ActorTransport // id: %7
//  %8 = ref_element_addr %5 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.actorTransport // user: %9
//  copy_addr [take] %6 to %8 : $*ActorTransport    // id: %9
//  dealloc_stack %6 : $*ActorTransport             // id: %10
//  end_borrow %5 : $SimpleEmptyDistributedActor               // id: %11
//  %12 = begin_borrow %4 : $SimpleEmptyDistributedActor       // users: %23, %20
//  %13 = alloc_stack $ActorIdentity                // users: %22, %21, %18
//  %14 = open_existential_addr immutable_access [[TRANSPORT]] : $*ActorTransport to $*@opened([[OPENED_TRANSPORT_ID]]) ActorTransport // users: %17, %17, [[ASSIGN_WITNESS_M]]
//  %15 = metatype $@thick @dynamic_self SimpleEmptyDistributedActor.Type // type-defs: [[SELF]]; user: %17
//  [[ASSIGN_WITNESS_M]] = witness_method $@opened([[OPENED_TRANSPORT_ID]]) ActorTransport, #ActorTransport.assignIdentity : <Self where Self : ActorTransport><Act where Act : DistributedActor> (Self) -> (Act.Type) -> AnyActorIdentity, %14 : $*@opened([[OPENED_TRANSPORT_ID]]) ActorTransport : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> AnyActorIdentity // type-defs: %14; user: %17
//  %17 = apply [[ASSIGN_WITNESS_M]]<@opened([[OPENED_TRANSPORT_ID]]) ActorTransport, @dynamic_self SimpleEmptyDistributedActor>(%15, %14) : $@convention(witness_method: ActorTransport) <τ_0_0 where τ_0_0 : ActorTransport><τ_1_0 where τ_1_0 : DistributedActor> (@thick τ_1_0.Type, @in_guaranteed τ_0_0) -> AnyActorIdentity // type-defs: %14, [[SELF]]; user: %19
//  %18 = init_existential_addr %13 : $*ActorIdentity, $AnyActorIdentity // user: %19
//  store %17 to [trivial] %18 : $*AnyActorIdentity // id: %19
//  %20 = ref_element_addr %12 : $SimpleEmptyDistributedActor, #SimpleEmptyDistributedActor.id // user: %21
//  copy_addr [take] %13 to %20 : $*ActorIdentity   // id: %21
//  dealloc_stack %13 : $*ActorIdentity             // id: %22
//  end_borrow %12 : $SimpleEmptyDistributedActor              // id: %23
//  %24 = copy_value %4 : $SimpleEmptyDistributedActor         // user: %27
//  destroy_value %4 : $SimpleEmptyDistributedActor            // id: %25
//  destroy_addr [[TRANSPORT]] : $*ActorTransport              // id: %26
//  return %24 : $SimpleEmptyDistributedActor                  // id: %27
//} // end sil function '$s3lol16SimpleEmptyDistributedActorC9transportAcA14ActorTransport_p_tcfc'