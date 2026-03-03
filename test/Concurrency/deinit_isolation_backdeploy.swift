// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -emit-silgen -DSILGEN %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: OS=macosx


@MainActor
class C {
  // CHECK-LABEL: sil hidden [ossa] @$s27deinit_isolation_backdeploy1CCfD : $@convention(method) (@owned C) -> ()
  // CHECK: function_ref @swift_task_deinitOnExecutorMainActorBackDeploy
  isolated deinit { }
}

// Make sure this function is available
// CHECK: sil hidden_external [serialized] [available 12.0.0] @swift_task_deinitOnExecutorMainActorBackDeploy : $@convention(thin) (@owned AnyObject, @convention(thin) (@owned AnyObject) -> (), Builtin.Executor, Builtin.Word) -> ()
