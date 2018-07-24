// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow_swift_bindings
//
// TensorFlow aggregate parameter update tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var ParameterUpdateTests = TestSuite("ParameterUpdate")

RawOpTests.test("UpdateParameters") {
  struct Foo : Parameterized {
    @TFParameter var w = Tensor<Float>(1)
    mutating func foo() {
      updateParameters(withGradients: Parameters(w: Tensor(1))) {
        $0 += $1
      }
    }
  }
  var f = Foo()
  f.foo()
  expectEqual(Tensor<Float>(2), f.w)
}

runAllTests()
