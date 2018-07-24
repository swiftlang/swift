// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// TensorFlow aggregate parameter update tests.

import TensorFlow
import StdlibUnittest

var ParameterUpdateTests = TestSuite("ParameterUpdate")

// TODO: When SR-8360 is fixed, move this to the body of the 'UpdateParameters'
// test.
struct Foo : Parameterized {
  @TFParameter var w = Tensor<Float>(1)
  mutating func foo() {
    updateParameters(withGradients: Parameters(w: Tensor(1))) {
      $0 += $1
    }
  }
}

ParameterUpdateTests.test("UpdateParameters") {
  var f = Foo()
  f.foo()
  expectEqual(Tensor<Float>(2), f.w)
}

runAllTests()
