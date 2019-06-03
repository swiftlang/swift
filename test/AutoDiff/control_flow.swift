// RUN: %target-run-simple-swift-control-flow-differentiation
// REQUIRES: executable_test

import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

ControlFlowTests.test("Conditionals") {
  func cond1(_ x: Float) -> Float {
    if x > 0 {
      return x * x
    }
    return x + x
  }
  expectEqual(8, gradient(at: 4, in: cond1))
  expectEqual(2, gradient(at: -10, in: cond1))

  func cond2(_ x: Float) -> Float {
    let y: Float
    if x > 0 {
      y = x * x
    } else if x == -1337 {
      y = 0
    } else {
      y = x + x
    }
    return y
  }
  expectEqual(8, gradient(at: 4, in: cond2))
  expectEqual(2, gradient(at: -10, in: cond2))
  expectEqual(0, gradient(at: -1337, in: cond2))

  func cond3(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      return x * y
    }
    return y - x
  }
  expectEqual((5, 4), gradient(at: 4, 5, in: cond3))
  expectEqual((-1, 1), gradient(at: -3, -2, in: cond3))

  func cond_empty(_ x: Float) -> Float {
    if x > 0 {
      // Create empty trampoline blocks.
    }
    return x * x
  }
  expectEqual(4, gradient(at: 2, in: cond_empty))
  expectEqual(-6, gradient(at: -3, in: cond_empty))

  func cond_generic<T : Differentiable & FloatingPoint>(
    _ x: T, _ y: T
  ) -> T {
    if x > 0 {
      return x
    }
    return y
  }
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
  expectEqual((20, 4), gradient(at: 4, 20, in: nested2))
  expectEqual((1, 1), gradient(at: 4, 5, in: nested2))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested2))

  func nested3(_ x: Float, _ y: Float) -> Float {
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
    return -y
  }
  expectEqual((40, 8), gradient(at: 4, 20, in: nested3))
  expectEqual((0, -1), gradient(at: 4, 21, in: nested3))
  expectEqual((1, 1), gradient(at: 4, 5, in: nested3))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested3))
}

ControlFlowTests.test("Recursion") {
  func factorial(_ x: Float) -> Float {
    if x == 1 {
      return 1
    }
    return x * factorial(x - 1)
  }
  expectEqual(0, gradient(at: 1, in: factorial))
  expectEqual(1, gradient(at: 2, in: factorial))
  expectEqual(5, gradient(at: 3, in: factorial))
  expectEqual(26, gradient(at: 4, in: factorial))
  expectEqual(154, gradient(at: 5, in: factorial))

  func product(_ x: Float, count: Int) -> Float {
    precondition(count > 0)
    if count == 1 {
      return x
    }
    return x * product(x, count: count - 1)
  }
  expectEqual(300, gradient(at: 10, in: { x in product(x, count: 3) }))
  expectEqual(-20, gradient(at: -10, in: { x in product(x, count: 2) }))
  expectEqual(1, gradient(at: 100, in: { x in product(x, count: 1) }))
}

runAllTests()
