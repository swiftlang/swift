// RUN: %target-run-simple-swift
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

  func cond2_var(_ x: Float) -> Float {
    var y: Float = x
    if x > 0 {
      y = y * x
    } else if x == -1337 {
      y = x // Dummy assignment; shouldn't affect computation.
      y = x // Dummy assignment; shouldn't affect computation.
      y = 0
    } else {
      y = x + y
    }
    return y
  }
  expectEqual(8, gradient(at: 4, in: cond2_var))
  expectEqual(2, gradient(at: -10, in: cond2_var))
  expectEqual(0, gradient(at: -1337, in: cond2_var))

  func cond3(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      return x * y
    }
    return y - x
  }
  expectEqual((5, 4), gradient(at: 4, 5, in: cond3))
  expectEqual((-1, 1), gradient(at: -3, -2, in: cond3))

  func cond_tuple(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y: (Float, Float) = (x, x)
    if x > 0 {
      return y.0 + y.1
    }
    return y.0 + y.0 - y.1 + y.0
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple))

  func cond_tuple2(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y: (Float, Float) = (x, x)
    let y0 = y.0
    if x > 0 {
      let y1 = y.1
      return y0 + y1
    }
    let y0_double = y0 + y.0
    let y1 = y.1
    return y0_double - y1 + y.0
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple2))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple2))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple2))

  func cond_tuple_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y: (Float, Float) = (x, x)
    var z: (Float, Float) = (x + x, x - x)
    if x > 0 {
      var w = (x, x)
      y.0 = w.1
      y.1 = w.0
      z.0 = z.0 - y.0
      z.1 = z.1 + y.0
    } else {
      z = (x, x)
    }
    return y.0 + y.1 - z.0 + z.1
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple_var))

  func cond_nestedtuple_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y: (Float, Float) = (x + x, x - x)
    var z: ((Float, Float), Float) = (y, x)
    if x > 0 {
      var w = (x, x)
      y.0 = w.1
      y.1 = w.0
      z.0.0 = z.0.0 - y.0
      z.0.1 = z.0.1 + y.0
    } else {
      z = ((y.0 - x, y.1 + x), x)
    }
    return y.0 + y.1 - z.0.0 + z.0.1
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_nestedtuple_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_nestedtuple_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_nestedtuple_var))

  struct FloatPair : Differentiable {
    var first, second: Float
    init(_ first: Float, _ second: Float) {
      self.first = first
      self.second = second
    }
  }

  struct Pair<T : Differentiable, U : Differentiable> : Differentiable {
    var first: T
    var second: U
    init(_ first: T, _ second: U) {
      self.first = first
      self.second = second
    }
  }

  func cond_struct(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y = FloatPair(x, x)
    if x > 0 {
      return y.first + y.second
    }
    return y.first + y.first - y.second + y.first
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct))

  func cond_struct2(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y = FloatPair(x, x)
    let y0 = y.first
    if x > 0 {
      let y1 = y.second
      return y0 + y1
    }
    let y0_double = y0 + y.first
    let y1 = y.second
    return y0_double - y1 + y.first
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct2))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct2))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct2))

  func cond_struct_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y = FloatPair(x, x)
    var z = FloatPair(x + x, x - x)
    if x > 0 {
      var w = y
      y.first = w.second
      y.second = w.first
      z.first = z.first - y.first
      z.second = z.second + y.first
    } else {
      z = FloatPair(x, x)
    }
    return y.first + y.second - z.first + z.second
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct_var))

  func cond_nestedstruct_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y = FloatPair(x + x, x - x)
    var z = Pair(y, x)
    if x > 0 {
      var w = FloatPair(x, x)
      y.first = w.second
      y.second = w.first
      z.first.first = z.first.first - y.first
      z.first.second = z.first.second + y.first
    } else {
      z = Pair(FloatPair(y.first - x, y.second + x), x)
    }
    return y.first + y.second - z.first.first + z.first.second
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_nestedstruct_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_nestedstruct_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_nestedstruct_var))

  func guard1(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      return x * x
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard1))
  expectEqual((-6, 0), gradient(at: -3, -2, in: guard1))

  func guard2(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      if y > 0 {
        return x * y
      } else if x == -1337 {
        return x * x
      }
      return 0
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard2))
  expectEqual((5, -1337), gradient(at: -1337, 5, in: guard2))
  expectEqual((-2674, 0), gradient(at: -1337, -5, in: guard2))
  expectEqual((2, -3), gradient(at: -3, 2, in: guard2))

  func guard2_var(_ x: Float, _ y: Float) -> Float {
    var z = y
    guard x > 0 else {
      if y > 0 {
        z = z * x
      } else if x == -1337 {
        z = x
        z = z * z
      } else {
        z = 0
      }
      return z
    }
    return z * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard2_var))
  expectEqual((5, -1337), gradient(at: -1337, 5, in: guard2_var))
  expectEqual((-2674, 0), gradient(at: -1337, -5, in: guard2_var))
  expectEqual((2, -3), gradient(at: -3, 2, in: guard2_var))

  func guard3(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      fatalError()
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard3))
  expectCrash {
    gradient(at: -3, -2, in: guard3)
  }

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

  func nested3_var(_ x: Float, _ y: Float) -> Float {
    var w = y
    if x > 0 {
      if y > 10 {
        var z = x * w
        if z > 100 {
          z = x + z
          return z
        } else if y == 20 {
          z = z + z
          return z
        }
      } else {
        w = x + w
        return w
      }
    }
    w = -w
    return w
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

  func factorial_var1(_ x: Float) -> Float {
    var y: Float = x
    if x == 1 {
      y = 1
    } else {
      y = x
      y = y * factorial_var1(y - 1)
    }
    return y
  }
  expectEqual(0, gradient(at: 1, in: factorial_var1))
  expectEqual(1, gradient(at: 2, in: factorial_var1))
  expectEqual(5, gradient(at: 3, in: factorial_var1))
  expectEqual(26, gradient(at: 4, in: factorial_var1))
  expectEqual(154, gradient(at: 5, in: factorial_var1))

  func factorial_var2(_ x: Float) -> Float {
    // Next line is the only difference with `factorial_var1`.
    var y: Float = 1
    if x == 1 {
      y = 1
    } else {
      y = x
      y = y * factorial_var2(y - 1)
    }
    return y
  }
  // FIXME: Fix zero gradients (related to activity analysis).
  // See `factorial_var1` for the working version.
  /*
  expectEqual(0, gradient(at: 1, in: factorial_var2))
  expectEqual(1, gradient(at: 2, in: factorial_var2))
  expectEqual(5, gradient(at: 3, in: factorial_var2))
  expectEqual(26, gradient(at: 4, in: factorial_var2))
  expectEqual(154, gradient(at: 5, in: factorial_var2))
  */
  expectEqual(0, gradient(at: 1, in: factorial_var2))
  expectEqual(0, gradient(at: 2, in: factorial_var2))
  expectEqual(0, gradient(at: 3, in: factorial_var2))
  expectEqual(0, gradient(at: 4, in: factorial_var2))
  expectEqual(0, gradient(at: 5, in: factorial_var2))

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
