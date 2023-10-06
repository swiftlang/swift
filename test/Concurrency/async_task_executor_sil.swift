// RUN: %target-swift-frontend  -disable-availability-checking %s -parse-as-library -parse-stdlib -emit-sil -o - | %FileCheck %s

// REQUIRES: concurrency

import Swift
import _Concurrency

// CHECK-LABEL: sil{{.*}} @$s14builtin_silgen30testNonisolatedTaskExecutorGetyyYaF : {{.*}} {
// CHECK: bb0:
// CHECK: [[PREFERRED_EXECUTOR_FN:%[0-9]+]] = function_ref @swift_task_getPreferredTaskExecutor
// CHECK: [[PREFERRED_EXECUTOR:%[0-9]+]] = apply [[PREFERRED_EXECUTOR_FN]]()
// CHECK: [[PREFERRED_EXECUTOR_OPT:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[PREFERRED_EXECUTOR]] : $Builtin.Executor
// CHECK: hop_to_executor [[PREFERRED_EXECUTOR_OPT]]
@available(SwiftStdlib 9999, *)
func testNonisolatedTaskExecutorGet() async {
  Swift.print("test") // don't leave the method empty, or the hop might be optimized away
}
