// RUN: %target-swift-emit-silgen %s -enable-experimental-feature ExecutionAttribute | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -enable-experimental-feature ExecutionAttribute -enable-experimental-feature AsyncCallerExecution | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_ExecutionAttribute
// REQUIRES: swift_feature_AsyncCallerExecution

// Validate that both with and without the experimental flag we properly codegen
// execution(caller) and execution(concurrent).

// CHECK-LABEL: // executionCaller()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s14execution_attr0A6CalleryyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
@execution(caller)
func executionCaller() async {}

// CHECK-LABEL: // executionConcurrent()
// CHECK: // Isolation: nonisolated
// CHECK: sil hidden [ossa] @$s14execution_attr0A10ConcurrentyyYaF : $@convention(thin) @async () -> () {
@execution(concurrent)
func executionConcurrent() async {}
