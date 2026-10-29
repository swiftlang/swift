// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls %s -module-name main -o %t/send_any_call_silgen.sil
// RUN: %FileCheck %s < %t/send_any_call_silgen.sil

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// 'send' of a call of a function which is not '@oneway' runs the call in a
// discarding task, like for a '@oneway' function. The task starts on the
// callee's actor

actor Worker {
  func work() {}
  func workAsync() async {}
}

@MainActor func mainActorFunction() {}
func isolatedParameter(_ w: isolated Worker) {}

// A synchronous method which is isolated to the actor is enqueued on the
// actor, in a task whose operation is synchronous
// CHECK-LABEL: sil hidden [ossa] @$s4main8sendWorkyyAA6WorkerCF :
// CHECK-NOT: builtin "createAsyncTask"
// CHECK: function_ref @$ss14_enqueueOneway2on_yx_yxYictScARzlF
// CHECK-NOT: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main8sendWorkyyAA6WorkerCF'
func sendWork(_ w: Worker) {
  send w.work()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main13sendWorkAsyncyyAA6WorkerCF :
// CHECK: [[EXEC:%.*]] = builtin "buildDefaultActorExecutorRef"<Worker>
// CHECK: [[SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
// CHECK: builtin "createAsyncTask"({{.*}} : $Int, [[SOME_EXEC]] : $Optional<Builtin.Executor>,
// CHECK: } // end sil function '$s4main13sendWorkAsyncyyAA6WorkerCF'
func sendWorkAsync(_ w: Worker) {
  send w.workAsync()
}

// A synchronous global actor isolated function is enqueued on its global
// actor
// CHECK-LABEL: sil hidden [ossa] @$s4main13sendMainActoryyF :
// CHECK: function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF
// CHECK-NOT: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main13sendMainActoryyF'
func sendMainActor() {
  send mainActorFunction()
}

// A function isolated to an actor through an 'isolated' parameter runs in a
// task
// CHECK-LABEL: sil hidden [ossa] @$s4main21sendIsolatedParameteryyAA6WorkerCF :
// CHECK: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main21sendIsolatedParameteryyAA6WorkerCF'
func sendIsolatedParameter(_ w: Worker) {
  send isolatedParameter(w)
}

