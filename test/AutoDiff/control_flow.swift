// RUN: %target-run-simple-swift-control-flow-differentiation
// REQUIRES: executable_test

import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

ControlFlowTests.test("Conditional") {
  func cond1(_ x: Float) -> Float {
    if x > 0 {
      return x * x
    }
    return x + x
  }
  expectEqual(8, gradient(at: 4, in: cond1))
  expectEqual(2, gradient(at: -10, in: cond1))

  func cond2(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      return x * y
    }
    return y - x
  }
  expectEqual((5, 4), gradient(at: 4, 5, in: cond2))
  expectEqual((-1, 1), gradient(at: -3, -2, in: cond2))

  func cond_generic<T : Differentiable & FloatingPoint>(
    _ x: T, _ y: T
  ) -> T {
    if x > 0 {
      return x
    }
    return y
  }
  // FIXME: Fix "instruction isn't dominated by its operand" crash in AdjointEmitter.
  expectEqual((1, 0), gradient(at: 4, 5, in: { x, y in cond_generic(x, y) }))
  expectEqual((0, 1), gradient(at: -4, 5, in: { x, y in cond_generic(x, y) }))
}

ControlFlowTests.test("NestedConditionals") {
  func nested1(_ x: Float) -> Float {
    if x > 0 {
      if x > 10 {
        return x * x + x
      } else {
        return x * x
      }
    }
    return x * -x
  }
  expectEqual(23, gradient(at: 11, in: nested1))
  expectEqual(8, gradient(at: 4, in: nested1))
  expectEqual(20, gradient(at: -10, in: nested1))

  func nested2(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      if y > 10 {
        return x * y
      } else {
        return x + y
      }
    }
    return -y
  }
  // FIXME: Debug incorrect "0" gradient values wrt x.
  // expectEqual((20, 4), gradient(at: 4, 20, in: nested2))
  expectEqual((0, 4), gradient(at: 4, 20, in: nested2))
  // expectEqual((1, 1), gradient(at: 4, 5, in: nested2))
  expectEqual((0, 1), gradient(at: 4, 5, in: nested2))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested2))
}

runAllTests()
