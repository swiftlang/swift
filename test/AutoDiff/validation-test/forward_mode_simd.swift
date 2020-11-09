// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-forward-mode-differentiation)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardModeDifferentiation")

//===----------------------------------------------------------------------===//
// SIMD methods from SIMDDifferentiation.swift.gyb
// Tests replicate reverse mode tests from test/AutoDiff/stdlib/simd.swift
//===----------------------------------------------------------------------===//

ForwardModeTests.test("init(repeating:)") {
  func foo1(x: Float) -> SIMD4<Float> {
    return SIMD4<Float>(repeating: 2 * x)
  }
  let (val1, df1) = valueWithDifferential(at: 5, in: foo1)
  expectEqual(SIMD4<Float>(10, 10, 10, 10), val1)
  expectEqual(SIMD4<Float>(6, 6, 6, 6), df1(3))
}

ForwardModeTests.test("Identity") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  func foo1(x: SIMD4<Float>) -> SIMD4<Float> {
    return x
  }
  let (val1, df1) = valueWithDifferential(at: a, in: foo1)
  expectEqual(a, val1)
  expectEqual(g, df1(.init(g)))
}

ForwardModeTests.test("Negate") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  func foo1(x: SIMD4<Float>) -> SIMD4<Float> {
    return -x
  }
  let (val1, df1) = valueWithDifferential(at: a, in: foo1)
  expectEqual(-a, val1)
  expectEqual(-g, df1(.init(g)))
}

ForwardModeTests.test("subscript") {
  let a = SIMD4<Float>(1, 2, 3, 4)

  func foo1(x: SIMD4<Float>) -> Float {
    return x[3]
  }

  let (val1, df1) = valueWithDifferential(at: a, in: foo1)
  expectEqual(4, val1)
  expectEqual(4, df1(a))
}

ForwardModeTests.test("Addition") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  // SIMD + SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x + y
  }
  let (val1, df1) = valueWithDifferential(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(2, 4, 6, 8), val1)
  expectEqual(a + g, df1(a, g))

  // SIMD + Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x + y
  }
  let (val2, df2) = valueWithDifferential(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(6, 7, 8, 9), val2)
  expectEqual(g + 1, df2(g, 1))

  // Scalar + SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y + x
  }
  let (val3, df3) = valueWithDifferential(at: a, 5, in: foo3)
  expectEqual(SIMD4<Float>(6, 7, 8, 9), val3)
  expectEqual(2 + g, df3(g, 2))
}

ForwardModeTests.test("Subtraction") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  // SIMD - SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x - y
  }
  let (val1, df1) = valueWithDifferential(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(0, 0, 0, 0), val1)
  expectEqual(g - a, df1(g, a))

  // SIMD - Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x - y
  }
  let (val2, df2) = valueWithDifferential(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(-4, -3, -2, -1), val2)
  expectEqual(g - 1, df2(g, 1))

  // Scalar - SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y - x
  }
  let (val3, df3) = valueWithDifferential(at: a, 5, in: foo3)
  expectEqual(SIMD4<Float>(4, 3, 2, 1), val3)
  expectEqual(2 - g, df3(g, 2))
}

ForwardModeTests.test("Multiplication") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let a2 = SIMD4<Float>(4, 3, 2, 1)
  let g = SIMD4<Float>(1, 1, 1, 1)
  let g2 = SIMD4<Float>(0, 2, 1, 3)

  // SIMD * SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x * y
  }
  let (val1, df1) = valueWithDifferential(at: a, a2, in: foo1)
  expectEqual(a * a2, val1)
  expectEqual(a * g2 + g * a2, df1(g, g2))

  // SIMD * Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x * y
  }
  let (val2, df2) = valueWithDifferential(at: a, 5, in: foo2)
  expectEqual(a * 5, val2)
  expectEqual(a * 2 + g * 5, df2(g, 2))

  // Scalar * SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y * x
  }
  let (val3, df3) = valueWithDifferential(at: a, 5, in: foo3)
  expectEqual(a * 5, val3)
  expectEqual(a * 3 + g * 5, df3(g, 3))
}

ForwardModeTests.test("Division") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  // SIMD / SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x / y
  }
  let (val1, df1) = valueWithDifferential(at: a, a, in: foo1)
  expectEqual(a / a, val1)
  expectEqual((g * a - a * g) / (a * a)/* == 0 */, df1(g, g))

  // SIMD / Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x / y
  }
  let (val2, df2) = valueWithDifferential(at: a, 5, in: foo2)
  expectEqual(a / 5, val2)
  expectEqual((g * 5 - a * 2) / (5 * 5), df2(g, 2))

  // Scalar / SIMD
  func foo3(x: Float, y: SIMD4<Float>) -> SIMD4<Float> {
    return x / y
  }
  let (val3, df3) = valueWithDifferential(at: 5, a, in: foo3)
  expectEqual(5 / a, val3)
  expectEqual((3 * a - 5 * g) / (a * a), df3(3, g))
}

ForwardModeTests.test("Generics") {
  let a = SIMD3<Double>(1, 2, 3)
  let g = SIMD3<Double>(1, 1, 1)

  // FIXME(SR-13210): Fix forward-mode SIL verification error.
  /*
  func testInit<Scalar, SIMDType: SIMD>(x: Scalar) -> SIMDType
    where SIMDType.Scalar == Scalar,
          SIMDType : Differentiable,
          Scalar : BinaryFloatingPoint & Differentiable,
          SIMDType.TangentVector == SIMDType,
          Scalar.TangentVector == Scalar {
    return SIMDType.init(repeating: x)
  }
  func simd3Init(x: Double) -> SIMD3<Double> { testInit(x: x) }
  let (val1, df1) = valueWithDifferential(at: 10, in: simd3Init)
  expectEqual(SIMD3<Double>(10, 10, 10), val1)
  expectEqual(SIMD3<Double>(5, 5, 5), df1(5))
  */

  // SIMDType + SIMDType
  func testAddition<Scalar, SIMDType: SIMD>(lhs: SIMDType, rhs: SIMDType)
    -> SIMDType
    where SIMDType.Scalar == Scalar,
          SIMDType : Differentiable,
          SIMDType.TangentVector : SIMD,
          Scalar : BinaryFloatingPoint,
          SIMDType.TangentVector.Scalar : BinaryFloatingPoint {
    return lhs + rhs
  }
  func simd3Add(lhs: SIMD3<Double>, rhs: SIMD3<Double>) -> SIMD3<Double> {
    return testAddition(lhs: lhs, rhs: rhs)
  }
  let (val2, df2) = valueWithDifferential(at: a, a, in: simd3Add)
  expectEqual(SIMD3<Double>(2, 4, 6), val2)
  expectEqual(g + a, df2(g, a))

  // Scalar - SIMDType
  func testSubtraction<Scalar, SIMDType: SIMD>(lhs: Scalar, rhs: SIMDType)
    -> SIMDType
    where SIMDType.Scalar == Scalar,
          SIMDType : Differentiable,
          Scalar : BinaryFloatingPoint & Differentiable,
          SIMDType.TangentVector == SIMDType,
          Scalar.TangentVector == Scalar {
    return lhs - rhs
  }
  func simd3Subtract(lhs: Double, rhs: SIMD3<Double>) -> SIMD3<Double> {
    return testSubtraction(lhs: lhs, rhs: rhs)
  }
  let (val3, df3) = valueWithDifferential(at: 5, a, in: simd3Subtract)
  expectEqual(SIMD3<Double>(4, 3, 2), val3)
  expectEqual(2 - g, df3(2, g))

  // SIMDType * Scalar
  func testMultipication<Scalar, SIMDType: SIMD>(lhs: SIMDType, rhs: Scalar)
    -> SIMDType
    where SIMDType.Scalar == Scalar,
      SIMDType : Differentiable,
      Scalar : BinaryFloatingPoint & Differentiable,
      SIMDType.TangentVector == SIMDType,
      Scalar.TangentVector == Scalar {
    return lhs * rhs
  }
  func simd3Multiply(lhs: SIMD3<Double>, rhs: Double) -> SIMD3<Double> {
    return testMultipication(lhs: lhs, rhs: rhs)
  }
  let (val4, df4) = valueWithDifferential(at: a, 5, in: simd3Multiply)
  expectEqual(SIMD3<Double>(5, 10, 15), val4)
  expectEqual(a * 3 + g * 5 , df4(g, 3))
}

runAllTests()
