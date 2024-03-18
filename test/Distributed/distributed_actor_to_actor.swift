// RUN: %target-swift-frontend  -disable-availability-checking %s -parse-as-library -parse-stdlib -emit-sil -o - | %FileCheck %s -check-prefix=CHECK-SIL

// RUN: %target-swift-frontend  -disable-availability-checking %s -parse-as-library -parse-stdlib -emit-ir -o - | %FileCheck %s -check-prefix=CHECK-IR

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://123970272
// UNSUPPORTED: CPU=arm64e

import _Concurrency
import Distributed

// Protocol conformance descriptor:
//   - extension of DistributedActor
//   - flags, which includes a specific bit + conditional conformance
//   - witness for unownedExecutor
// CHECK-IR: @"$sxScA11DistributedMc" = linkonce_odr hidden constant
// CHECK-IR-SAME: $s11Distributed0A5ActorPAAEMXE
// CHECK-IR-SAME: i32 459080
// CHECK-IR-SAME: $sxScA11DistributedScA15unownedExecutorScevgTW

// Make sure there is no runtime record of the protocol descriptor.
// CHECK-IR-NOT: $sxScA11DistributedHc

// CHECK-SIL-LABEL: sil hidden @$s021distributed_actor_to_B011getAnyActor0aF0ScA_pxYi_t11Distributed0gF0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : DistributedActor> (@sil_isolated @guaranteed τ_0_0) -> @owned any Actor
func getAnyActor(distributedActor: isolated some DistributedActor) -> any Actor {
  // CHECK-SIL: [[EXISTENTIAL:%.*]] = init_existential_ref %0 : $τ_0_0 : $τ_0_0, $any Actor
  // CHECK-SIL-NEXT: strong_retain [[EXISTENTIAL]] : $any Actor
  // CHECK-SIL-NEXT: return [[EXISTENTIAL]] : $any Actor
  return Builtin.distributedActorAsAnyActor(distributedActor)
}

// CHECK-IR-LABEL: define {{.*}} @"$s021distributed_actor_to_B011getAnyActor0aF0ScA_pxYi_t11Distributed0gF0RzlF
// CHECK-IR: %conditional.requirement.buffer = alloca [1 x ptr]
// CHECK-IR: [[CONDITIONAL_REQ_GEP:%[0-9]+]] = getelementptr inbounds [1 x ptr], ptr %conditional.requirement.buffer, i32 0, i32 0
// CHECK-IR-NEXT: [[SELF_DA_REQ:%.*]] = getelementptr inbounds ptr, ptr [[CONDITIONAL_REQ_GEP]], i32 0
// CHECK-IR-NEXT: store ptr %"some DistributedActor.DistributedActor", ptr [[SELF_DA_REQ]]
// CHECK-IR-NEXT: call ptr @swift_getWitnessTable(ptr @"$sxScA11DistributedMc", ptr %"some DistributedActor", ptr [[CONDITIONAL_REQ_GEP]])

// CHECK-SIL-LABEL: sil_witness_table shared <Self where Self : DistributedActor> T: Actor module Distributed {
// CHECK-SIL-NEXT: method #Actor.unownedExecutor!getter: <Self where Self : Actor> (Self) -> () -> UnownedSerialExecutor : @$sxScA11DistributedScA15unownedExecutorScevgTW
// CHECK-SIL-NEXT: conditional_conformance (Self: DistributedActor): dependent
// CHECK-SIL-NEXT: }
