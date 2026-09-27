// RUN: %target-swift-frontend -emit-ir -O -enable-experimental-feature Embedded -parse-as-library -wmo -module-name main %s | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

// A nil '(any TaskExecutor)?' task executor preference must not add an
// 'InitialTaskExecutorOwned' task option record. The runtime rejects that
// record in Embedded Swift, so e.g. 'Task.immediate' trapped in
// 'swift_task_create'. IRGen used to test the optional against the nil of a
// doubly wrapped optional (an extra inhabitant of the pointer, 1) instead of
// against null

import _Concurrency

@inline(never)
public func run(_ executor: consuming (any TaskExecutor)?) {
  Task.immediate(executorPreference: executor) {}
}

// CHECK-LABEL: define {{.*}}@"$e4main3run{{[^"]*}}"(
// CHECK-SAME: ptr [[EXECUTOR:%[0-9]+]], ptr {{%[0-9]+}})
// CHECK: [[IS_NIL:%.*]] = icmp eq ptr [[EXECUTOR]], null
// CHECK-NEXT: br i1 [[IS_NIL]], label %[[CONT:[^,]+]], label %{{[^ ]+}}
// CHECK: [[CONT]]:
// CHECK: call swiftcc %swift.async_task_and_context @swift_task_create(
