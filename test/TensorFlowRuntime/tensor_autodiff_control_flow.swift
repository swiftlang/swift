// RUN: %target-run-simple-swift-control-flow-differentiation
// REQUIRES: executable_test
//
// FIXME(TF-326): Re-enable `-O` after deserialization failure fix.
// UNSUPPORTED: swift_test_mode_optimize
//
// Tensor control flow AD runtime tests.
// TODO: Move TensorFlow-specific AD tests into test/AutoDiff.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorControlFlowAD")

TensorADTests.testAllBackends("NestedConditionals") {
  // Test tensor-tensor ops.
  func cond_nested1(_ x: Tensor<Float>, _ y: Tensor<Float>) -> Tensor<Float> {
    if x > 0 {
      if y > 10 {
        let z = x * y
        if z > 100 {
          return x + z
        } else if y == Tensor(20) {
          return z + z
        }
      } else {
        return x + y
      }
    }
    return -y
  }

  expectEqual((Tensor(40), Tensor(8)),
              gradient(at: Tensor(4), Tensor(20), in: cond_nested1))
  expectEqual((Tensor(0), Tensor(-1)),
              gradient(at: Tensor(4), Tensor(21), in: cond_nested1))
  expectEqual((Tensor(1), Tensor(1)),
              gradient(at: Tensor(4), Tensor(5), in: cond_nested1))
  expectEqual((Tensor(0), Tensor(-1)),
              gradient(at: Tensor(-3), Tensor(-2), in: cond_nested1))

  // Test tensor-scalar ops.
  func cond_nested2(_ x: Tensor<Float>, _ y: Float) -> Tensor<Float> {
    if x > 0 {
      if y > 10 {
        let z = x * y
        if z > 100 {
          return x + z
        } else if y == 20 {
          return z + z
        }
      } else {
        return x + y
      }
    }
    return Tensor(-y)
  }

  expectEqual((Tensor(40), 8), gradient(at: Tensor(4), 20, in: cond_nested2))
  expectEqual((Tensor(0), -1), gradient(at: Tensor(4), 21, in: cond_nested2))
  expectEqual((Tensor(1), 1), gradient(at: Tensor(4), 5, in: cond_nested2))
  expectEqual((Tensor(0), -1), gradient(at: Tensor(-3), -2, in: cond_nested2))
}

TensorADTests.testAllBackends("Recursion") {
  func factorial(_ x: Tensor<Float>) -> Tensor<Float> {
    if x == Tensor(1) {
      return Tensor(1)
    }
    return x * factorial(x - 1)
  }
  expectEqual(Tensor(0), gradient(at: Tensor(1), in: factorial))
  expectEqual(Tensor(1), gradient(at: Tensor(2), in: factorial))
  expectEqual(Tensor(5), gradient(at: Tensor(3), in: factorial))
  expectEqual(Tensor(26), gradient(at: Tensor(4), in: factorial))
  expectEqual(Tensor(154), gradient(at: Tensor(5), in: factorial))

  func product(_ x: Tensor<Float>, count: Int) -> Tensor<Float> {
    precondition(count > 0)
    if count == 1 {
      return x
    }
    return x * product(x, count: count - 1)
  }
  expectEqual(Tensor(300),
              gradient(at: Tensor(10), in: { x in product(x, count: 3) }))
  expectEqual(Tensor(-20),
              gradient(at: Tensor(-10), in: { x in product(x, count: 2) }))
  expectEqual(Tensor(1),
              gradient(at: Tensor(100), in: { x in product(x, count: 1) }))
}

runAllTests()
