// RUN: %target-swift-emit-silgen -enable-experimental-feature AsyncCallerExecution %s | %FileCheck %s

// REQUIRES: swift_feature_AsyncCallerExecution


// CHECK-LABEL: // concurrentTest()
// CHECK: // Isolation: nonisolated
// CHECK: sil hidden [ossa] @$s14attr_execution14concurrentTestyyYaF : $@convention(thin) @async () -> () {
@concurrent
func concurrentTest() async {}

// CHECK-LABEL: // callerTest()
// CHECK: // Isolation: caller_isolation_inheriting
// CHECK: sil hidden [ossa] @$s14attr_execution10callerTestyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
nonisolated(nonsending)
func callerTest() async {}
