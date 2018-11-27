// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// FIXME: It's looking for a _modify coroutine on `allParameters`.
//
// TensorFlow aggregate parameter update tests.

import TensorFlow
import StdlibUnittest

var ParameterUpdateTests = TestSuite("ParameterUpdate")

ParameterUpdateTests.test("UpdateParameters") {
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
  expectEqual(2, f.w.scalar!)
}

ParameterUpdateTests.test("UpdateParameterArrays") {
  struct Foo : Parameterized {
    @TFParameter var w = [Tensor<Float>(1), Tensor<Float>(2)]
    mutating func foo() {
      updateParameters(withGradients: Parameters(w: [Tensor(1), Tensor(2)])) {
        $0 += $1
      }
    }
  }
  var f = Foo()
  f.foo()
  expectEqual(2, f.w[0].scalar!)
  expectEqual(4, f.w[1].scalar!)
}

runAllTests()
