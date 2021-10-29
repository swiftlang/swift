// RUN:  %target-swift-emit-silgen %s -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

distributed actor DA { }

extension DA {
  // CHECK-LABEL: sil hidden [thunk] [ossa] @$s17distributed_thunk2DAC1fyyFTE : $@convention(method) @async (@guaranteed DA) -> @error Error
  // CHECK: function_ref @swift_distributed_actor_is_remote

  // Call the actor function
  // CHECK: function_ref @$s17distributed_thunk2DAC1fyyF : $@convention(method) (@guaranteed DA) -> ()

  // ... or the remote thunk
  // CHECK: dynamic_function_ref @$s17distributed_thunk2DAC9_remote_fyyYaKF : $@convention(method) @async (@guaranteed DA) -> @error Error
  distributed func f() { }
}
