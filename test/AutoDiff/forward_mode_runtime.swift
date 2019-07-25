// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardMode")

ForwardModeTests.test("Unary") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: func_to_diff)
  expectEqual(16, y)
  expectEqual(8, differential(1))
}

ForwardModeTests.test("Binary") {
  func func_to_diff(x: Float, y: Float) -> Float {
    return x * y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: func_to_diff)
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("BinaryWithLets") {
  func func_to_diff(x: Float, y: Float) -> Float {
    let a = x + y
    let b = a
    return b * -y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: func_to_diff)
  expectEqual(-45, y)
  expectEqual(-19, differential(1, 1))
}

ForwardModeTests.test("TrackedIdentity") {
  func identity(x: Tracked<Float>) -> Tracked<Float> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: identity)
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

ForwardModeTests.test("TrackedAddition") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x + y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(9, y)
  expectEqual(2, differential(1, 1))
}

ForwardModeTests.test("TrackedDivision") {
  func divide(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x / y
  }
  let (y, differential) = valueWithDifferential(at: 10, 5, in: divide)
  expectEqual(2, y)
  expectEqual(-0.2, differential(1, 1))
}

ForwardModeTests.test("TrackedMultipleMultiplication") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x * y * x
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(80, y)
  // 2yx+xx
  expectEqual(56, differential(1, 1))
}

ForwardModeTests.test("TrackedWithLets") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    let a = x + y
    let b = a * a // (x+y)^2
    let c = b / x + y // (x+y)^2/x+y
    return c
  }
  // (3x^2+2xy-y^2)/x^2+1
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(25.25, y)
  expectEqual(4.9375, differential(1, 1))
}

runAllTests()
