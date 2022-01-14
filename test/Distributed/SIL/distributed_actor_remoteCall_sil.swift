// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --color --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

distributed actor MyDistActor {
  distributed func test() {}

  nonisolated func TESTTESTTESTTEST(i: Int, s: String) async throws {
    // bb0:
    let remote = __isRemoteActor(self)

    if remote {
      // bb1:
      var invocation = self.actorSystem.makeInvocationEncoder()
//      try invocation.recordArgument/*<Int>*/(i)
//      try invocation.recordArgument/*<String>*/(s)
//      try invocation.recordErrorType/*<Error>*/(Error.self)
//      try invocation.recordReturnType/*<String>*/(String.self)
      try invocation.doneRecording()

      let target = RemoteCallTarget(_mangledName: "MANGLED_NAME")

      try await self.actorSystem.remoteCall(
        on: self,
        target: target,
        invocationDecoder: &invocation,
        throwing: Never.self,
        returning: String.self
      )

    } else {
      // bb2:
      try await self.test()
    }
  }
}

// CHECK-LABEL: sil hidden [thunk] [distributed] [ossa] @$s14default_deinit11MyDistActorC4testyyFTE : $@convention(method) @async (@guaranteed MyDistActor) -> @error Error {
// CHECK: // [[SELF:%[0-9]+]] "self"
//
// CHECK: bb0(%0 : @guaranteed $MyDistActor):
// CHECK:   %1 = init_existential_ref %0 : $MyDistActor : $MyDistActor, $AnyObject
// CHECK:   // function_ref swift_distributed_actor_is_remote
// CHECK:   %2 = function_ref @swift_distributed_actor_is_remote : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:   %3 = apply %2(%1) : $@convention(thin) (@guaranteed AnyObject) -> Bool
// CHECK:   %4 = struct_extract %3 : $Bool, #Bool._value
// CHECK:   cond_br %4, [[REMOTE_CALL_BB_1:%[0-9]+]], [[LOCAL_CALL_BB:%[0-9]+]]

// === Make the InvocationDecoder:
// CHECK: [[REMOTE_CALL_BB_1]]
// CHECK:   [[SYSTEM:%[0-9]+]] = ref_element_addr %0 : $MyDistActor, #MyDistActor.actorSystem // user: %8
// CHECK:   [[REMOTE_CALL_REF:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV21makeInvocationEncoderAA0aG0VyF : $@convention(method) (@guaranteed FakeActorSystem) -> FakeInvocation
// CHECK:   [[INVOCATION_DECODER:%[0-9]+]] = apply [[REMOTE_CALL_REF]](%6) : $@convention(method) (@guaranteed FakeActorSystem) -> FakeInvocation

// === Done recording:
// CHECK: [[INVOCATION_ACCESS:%[0-9]+]] = begin_access [modify] [static] %15 : $*FakeInvocation
// CHECK: // function_ref FakeInvocation.doneRecording()
// CHECK: [[DONE_RECORDING_REF:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0A10InvocationV13doneRecordingyyKF : $@convention(method) (@inout FakeInvocation) -> @error Error
// CHECK: try_apply [[DONE_RECORDING_REF]]([[INVOCATION_ACCESS]]) : $@convention(method) (@inout FakeInvocation) -> @error Error, normal [[DONE_NORMAL_BB:%[0-9]+]], error [[DONE_ERROR_BB:%[0-9]+]]
