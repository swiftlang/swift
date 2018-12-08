// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Retain/release tests.

import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var RetainReleaseTests = TestSuite("RetainRelease")

RetainReleaseTests.testAllBackends("Basic") {
  let t1 = Tensor<Float>(1.2)
  let _ = t1 + t1
  let _ = t1.array
}

// SR8862: The randomUniform initializer of Tensor uses calls toAccelerator() on
// some tensor handle TH, but the resulting @__tf_to_accel() call got hoisted by
// the optimizer _below_ the last strong_release of TH, causing the call of
// @__tf_to_accel() (or subsequent code) to crash.
// The fix was to remove the incorrect @_effects(readnone) label from the
// @__tf_to_accel() function definition.
struct Layer {
  public let w: Tensor<Float>

  init() {
    w = Tensor(randomUniform: [1, 1])
  }
}
RetainReleaseTests.testAllBackends("SR8862") {
  var layers: [Layer] = []
  layers.append(Layer())
  _hostOp(layers)
}

runAllTests()
