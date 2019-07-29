// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardMode")

ForwardModeTests.test("Identity") {
  func func_to_diff(x: Float) -> Float {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: func_to_diff)
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

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

// Generics.

struct Tensor<Scalar : FloatingPoint & Differentiable> : VectorProtocol, Differentiable {
  // NOTE: `value` must have type with known size (e.g. `Float`, not `Scalar`)
  // until differentiation has indirect passing support.
  var value: Float
  init(_ value: Float) { self.value = value }
}

ForwardModeTests.test("GenericIdentity") {
  func identity<T : Differentiable>(_ x: T) -> T {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    identity(x) 
  }
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

ForwardModeTests.test("GenericTensorIdentity") {
  func identity<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Tensor<T> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    identity(Tensor<Float>(x)) 
  }
  expectEqual(Tensor<Float>(4), y)
  expectEqual(Tensor<Float>(1), differential(1))
}

ForwardModeTests.test("GenericTensorPlus") {
  func plus<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Float {
    return x.value + x.value
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    plus(Tensor<Float>(x)) 
  }
  expectEqual(8, y)
  expectEqual(2, differential(1))
}

ForwardModeTests.test("GenericTensorBinaryInput") {
  func binary<T : FloatingPoint & Differentiable>(_ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    return x.value * y.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("GenericTensorWithLets") {
  func binary<T : FloatingPoint & Differentiable>(_ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    let a = Tensor<T>(x.value)
    let b = Tensor<T>(y.value)
    return a.value * b.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("GenericTensorWithVars") {
  func binary<T : FloatingPoint & Differentiable>(_ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    var a = Tensor<T>(x.value)
    var b = Tensor<T>(y.value)
    b = a
    a = Tensor<T>(y.value)
    return a.value * b.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("GenericTrackedIdentity") {
  func identity<T : Differentiable>(_ x: Tracked<T>) -> Tracked<T> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in
    identity(Tracked(x))
  }
  expectEqual(4, y)
  expectEqual(1, differential(1))
}


runAllTests()
