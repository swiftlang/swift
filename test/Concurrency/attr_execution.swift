// RUN: %target-swift-emit-silgen -enable-experimental-feature NonIsolatedAsyncInheritsIsolationFromContext %s | %FileCheck %s

// REQUIRES: asserts
// REQUIRES: swift_feature_NonIsolatedAsyncInheritsIsolationFromContext


// CHECK-LABEL: // concurrentTest()
// CHECK: // Isolation: concurrent
// CHECK: sil hidden [ossa] @$s14attr_execution14concurrentTestyyYaF : $@convention(thin) @async () -> () {
@execution(concurrent)
func concurrentTest() async {}

// CHECK-LABEL: // callerTest()
// CHECK: // Isolation: nonisolated
// CHECK: sil hidden [ossa] @$s14attr_execution10callerTestyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
@execution(caller)
func callerTest() async {}
