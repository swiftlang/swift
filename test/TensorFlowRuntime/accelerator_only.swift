// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-swift-frontend -emit-sil -O %s -verify | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

@TensorFlowGraph
func add_constant(_ a: Tensor<Float>) -> Tensor<Float> {
  return a + Tensor<Float>([[5]])
}

// CHECK-LABEL: @{{.*}}accelerator_only14matmul_and_add{{.*}}
// CHECK: graph_op "s16accelerator_only14matmul_and_addy10TensorFlow0F0VySfGAF_AFtF.tf_only{{.*}}
// CHECK: end sil function {{.*}}accelerator_only14matmul_and_add{{.*}}
@TensorFlowGraph
@inline(never)
func matmul_and_add(_ a : Tensor<Float>, _ b : Tensor<Float>) -> Tensor<Float> {
  return add_constant(a • b)
}

var acceleratorOnly = TestSuite("AcceleratorOnlyFunctions")

acceleratorOnly.testAllBackends("BasicAcceleratorOnly") {
  let result = matmul_and_add(Tensor<Float>(shape: [1, 1], scalars: [2.0]),
                              Tensor(shape: [1, 1], scalars: [3.0]));
  expectEqual(result.array, ShapedArray(shape: [1, 1], scalars: [11.0]))
}

runAllTests()
