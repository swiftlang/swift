// RUN:  %target-swift-emit-silgen %s -enable-experimental-distributed -disable-availability-checking | %FileCheck %s 
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

distributed actor DA {
  typealias ActorSystem = LocalTestingDistributedActorSystem
}

extension DA {
  // CHECK-LABEL: sil hidden [thunk] [distributed] [ref_adhoc_requirement_witness "$s11Distributed29LocalTestingInvocationDecoderC18decodeNextArgumentxyKSeRzSERzlF"] [ossa] @$s17distributed_thunk2DAC1fyyYaKFTE : $@convention(method) @async (@guaranteed DA) -> @error Error {
  // CHECK: function_ref @swift_distributed_actor_is_remote

  // Call the actor function
  // CHECK: function_ref @$s17distributed_thunk2DAC1fyyF : $@convention(method) (@guaranteed DA) -> ()

  distributed func f() { }
}
