// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest

var ForwardModeTests = TestSuite("ForwardMode")

// Basic Float constant functions.

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

// Functions with variables.

ForwardModeTests.test("UnaryWithVars") {
  func unary(x: Float) -> Float {
    var a = x
    a = x
    var b = a + 2
    b = b - 1
    let c: Float = 3
    var d = a + b + c - 1
    d = d + d
    return d
  }

  let (y, differential) = valueWithDifferential(at: 4, in: unary)
  expectEqual(22, y)
  expectEqual(4, differential(1))
}

// Functions with basic struct

struct A: Differentiable & AdditiveArithmetic {
    var x: Float
  }

ForwardModeTests.test("StructInit") {
  func structInit(x: Float) -> A {
    return A(x: 2 * x)
  }

  let (y, differential) = valueWithDifferential(at: 4, in: structInit)
  expectEqual(A(x: 8), y)
  expectEqual(A(x: 2), differential(1))
}

ForwardModeTests.test("StructExtract") {
  func structExtract(x: A) -> Float {
    return 2 * x.x
  }

  let (y, differential) = valueWithDifferential(
    at: A(x: 4), 
    in: structExtract) 
  expectEqual(8, y)
  expectEqual(2, differential(A(x: 1)))
}

ForwardModeTests.test("LocalStructVariable") {
  func structExtract(x: A) -> A {
    let a = A(x: 2 * x.x) // 2x
    var b = A(x: a.x + 2) // 2x + 2
    b = A(x: b.x + a.x) // 2x + 2 + 2x = 4x + 2
    return b
  }

  let (y, differential) = valueWithDifferential(
    at: A(x: 4), 
    in: structExtract) 
  expectEqual(A(x: 18), y)
  expectEqual(A(x: 4), differential(A(x: 1)))
}

// Functions with methods.

extension A {
  func noParamMethodA() -> A {
    return A(x: 2 * x)
  }

  func noParamMethodx() -> Float {
    return 2 * x
  }

  static func *(lhs: A, rhs: A) -> A {
    return A(x: lhs.x * rhs.x)
  }

  func complexBinaryMethod(u: A, v: Float) -> A {
    var b: A = u * A(x: 2)  // A(x: u * 2)
    b.x = b.x * v        // A(x: u * 2 * v)
    let c = b.x + 1      // u * 2 * v + 1

    // A(x: u * 2 * v + 1 + u * 2 * v) = A(x: x * (4uv + 1))
    return A(x: x * (c + b.x))
  }
}

ForwardModeTests.test("noParamMethodA") {
  let (y, differential) = valueWithDifferential(at: A(x: 4)) { x in
    x.noParamMethodA()
  }
  expectEqual(A(x: 8), y)
  expectEqual(A(x: 2), differential(A(x: 1)))
}

ForwardModeTests.test("noParamMethodx") {
  let (y, differential) = valueWithDifferential(at: A(x: 4)) { x in
    x.noParamMethodx()
  }
  expectEqual(8, y)
  expectEqual(2, differential(A(x: 1)))
}

ForwardModeTests.test("complexBinaryMethod") {
  let (y, differential) = valueWithDifferential(at: A(x: 4), A(x: 5), 3) { 
    (x, y, z) in
    // derivative = A(x: 4uv + 4xv + 4ux + 1) = 4*5*3 + 4*4*3 + 4*5*4 + 1 = 189
    x.complexBinaryMethod(u: y, v: z)
  }
  expectEqual(A(x: 244), y)
  expectEqual(A(x: 189), differential(A(x: 1), A(x: 1), 1))
}

runAllTests()
