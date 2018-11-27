// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -Xllvm -tf-dynamic-compilation -O -emit-sil %s >/dev/null
//
// TPU device placement tests -- compiler-only tests for OSS.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var DevicePlacementTPUTests = TestSuite("DevicePlacementTPU")

@inline(never)
public func explicitDevicePlacement() {
  let x_tpu : TensorHandle<Float> = #tfop("Identity", Tensor(Float(1.0)), T$dtype: Float.tensorFlowDataType, __device: "TPU_SYSTEM")
  let y_cpu : TensorHandle<Float> = #tfop("Identity", Tensor(Float(2.0)), T$dtype: Float.tensorFlowDataType, __shapes: [TensorShape()], __device: "/job:localhost/replica:0/task:0/device:CPU:0")
  // y_cpu is sent from CPU to TPU. 
  let z_tpu : TensorHandle<Float> = #tfop("Add", x_tpu, y_cpu, __device: "TPU_SYSTEM")
  expectNearlyEqualWithScalarTensor(3, Tensor<Float>(handle: z_tpu))
}
DevicePlacementTPUTests.testAllBackends("explicitDevicePlacement",
                                        explicitDevicePlacement)

runAllTests()
