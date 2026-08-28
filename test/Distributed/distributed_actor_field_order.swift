// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -disable-availability-checking -module-name field_order | %FileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// Verifies the stored-property layout of a distributed actor.
//
// The runtime relies on this exact field order: 'swift_distributedActor_remote_initialize'
// trims the allocation of a 'remote' proxy at the offset of the first user-defined
// stored property.
//
// Field order MUST be: $defaultActor, id, actorSystem, <user properties...>

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

public distributed actor WithUserFields {
  var first: Int = 1
  var second: String = ""
}

// The synthesized fields come first, in this order, before any user property.
// CHECK-LABEL: @"$s11field_order14WithUserFieldsC2id11Distributed19LocalTestingActorIDVvpWvd" =
// CHECK: @"$s11field_order14WithUserFieldsC11actorSystem{{.*}}vpWvd" =
// CHECK: @"$s11field_order14WithUserFieldsC5firstSivpWvd" =
// CHECK: @"$s11field_order14WithUserFieldsC6secondSSvpWvd" =

public distributed actor WithoutUserFields {
}

// Even with no user properties the synthesized fields are present.
// CHECK-LABEL: @"$s11field_order17WithoutUserFieldsC2id11Distributed19LocalTestingActorIDVvpWvd" =
// CHECK: @"$s11field_order17WithoutUserFieldsC11actorSystem{{.*}}vpWvd" =

public distributed actor CustomExecutorActor {
  var first: Int = 1

  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }
}

// A distributed actor with a custom executor is not a default actor, so it has no
// '$defaultActor' storage. 'id' and 'actorSystem' still come first, and
// 'unownedExecutor' is computed, so it contributes no field of its own.
// CHECK-LABEL: @"$s11field_order19CustomExecutorActorC2id11Distributed012LocalTestingE2IDVvpWvd" =
// CHECK: @"$s11field_order19CustomExecutorActorC11actorSystem{{.*}}vpWvd" =
// CHECK: @"$s11field_order19CustomExecutorActorC5firstSivpWvd" =
