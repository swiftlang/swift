// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: OS=macosx

// Control flow related tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest
import Foundation

var ControlFlowTests = TestSuite("ControlFlow")

class X {
  @objc func f() {}
}
class Y {}
@inline(never)
public func testDynamicMethodBranch(_ obj: AnyObject,
                                    _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if let foo = obj.f {
    foo()
    b += 1.0
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testDynamicMethodBranch") {
  testDynamicMethodBranch(X(), 2.0)
  testDynamicMethodBranch(Y(), 1.0)
}

runAllTests()
