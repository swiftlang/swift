// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SIMDTests = TestSuite("SIMD")

SIMDTests.test("init(repeating:)") {
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  func foo1(x: Float) -> SIMD4<Float> {
    return SIMD4<Float>(repeating: 2 * x)
  }
  let (val1, bp1) = valueWithPullback(at: 5, in: foo1)
  expectEqual(SIMD4<Float>(10, 10, 10, 10), val1)
  expectEqual(8, bp1(g))
}

SIMDTests.test("Sum") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  
  func foo1(x: SIMD4<Float>) -> Float {
    return x.sum()
  }
  let (val1, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(10, val1)
  expectEqual(SIMD4<Float>(3, 3, 3, 3), bp1(3))
}

SIMDTests.test("Identity") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  func foo1(x: SIMD4<Float>) -> SIMD4<Float> {
    return x
  }
  let (val1, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(a, val1)
  expectEqual(g, bp1(g))
}

SIMDTests.test("Negate") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  func foo1(x: SIMD4<Float>) -> SIMD4<Float> {
    return -x
  }
  let (val1, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(-a, val1)
  expectEqual(-g, bp1(g))
}

SIMDTests.test("subscript") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  
  func foo1(x: SIMD4<Float>) -> Float {
    return x[3]
  }
  
  let (val1, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(4, val1)
  expectEqual(SIMD4<Float>(0, 0, 0, 7), bp1(7))
}

SIMDTests.test("Addition") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD + SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x + y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(2, 4, 6, 8), val1)
  expectEqual((g, g), bp1(g))
  
  // SIMD + Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x + y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(6, 7, 8, 9), val2)
  expectEqual((g, 4), bp2(g))
  
  // Scalar + SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y + x
  }
  let (val3, bp3) = valueWithPullback(at: a, 5, in: foo3)
  expectEqual(SIMD4<Float>(6, 7, 8, 9), val3)
  expectEqual((g, 4), bp3(g))
}

SIMDTests.test("Subtraction") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD - SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x - y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(0, 0, 0, 0), val1)
  expectEqual((g, -g), bp1(g))
  
  // SIMD - Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x - y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(-4, -3, -2, -1), val2)
  expectEqual((g, -4), bp2(g))
  
  // Scalar - SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y - x
  }
  let (val3, bp3) = valueWithPullback(at: a, 5, in: foo3)
  expectEqual(SIMD4<Float>(4, 3, 2, 1), val3)
  expectEqual((-g, 4), bp3(g))
}

SIMDTests.test("Multiplication") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  // SIMD * SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x * y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(a * a, val1)
  expectEqual((a, a), bp1(g))

  // SIMD * Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x * y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(a * 5, val2)
  expectEqual((SIMD4<Float>(5, 5, 5, 5), 10), bp2(g))

  // Scalar * SIMD
  func foo3(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return y * x
  }
  let (val3, bp3) = valueWithPullback(at: a, 5, in: foo3)
  expectEqual(a * 5, val3)
  expectEqual((SIMD4<Float>(5, 5, 5, 5), 10), bp3(g))
}

SIMDTests.test("Division") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD / SIMD
  func foo1(x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> {
    return x / y
  }
  let dlhs1 = g / a
  let drhs1 = -1 / a
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(a / a, val1)
  expectEqual((dlhs1, drhs1), bp1(g))
  
  // SIMD / Scalar
  func foo2(x: SIMD4<Float>, y: Float) -> SIMD4<Float> {
    return x / y
  }
  let dlhs2 = g / 5
  let drhs2 = (-a / 25 * g).sum()
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(a / 5, val2)
  expectEqual((dlhs2, drhs2), bp2(g))
  
  // Scalar / SIMD
  func foo3(x: Float, y: SIMD4<Float>) -> SIMD4<Float> {
    return x / y
  }
  let dlhs3 = (g / a).sum()
  let drhs3 = -5 / (a*a) * g
  let (val3, bp3) = valueWithPullback(at: 5, a, in: foo3)
  expectEqual(5 / a, val3)
  expectEqual((dlhs3, drhs3), bp3(g))
}

SIMDTests.test("Generics") {
  let a = SIMD3<Double>(1, 2, 3)
  let g = SIMD3<Double>(1, 1, 1)
  
  func testInit<Scalar, SIMDType: SIMD>(x: Scalar) -> SIMDType
    where SIMDType.Scalar == Scalar,
          SIMDType : Differentiable,
          Scalar : BinaryFloatingPoint & Differentiable,
          SIMDType.TangentVector == SIMDType,
          Scalar.TangentVector == Scalar {
    return SIMDType.init(repeating: x)
  }
  func simd3Init(x: Double) -> SIMD3<Double> { testInit(x: x) }
  let (val1, bp1) = valueWithPullback(at: 10, in: simd3Init)
  expectEqual(SIMD3<Double>(10, 10, 10), val1)
  expectEqual(3, bp1(g))
  
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
  let (val2, bp2) = valueWithPullback(at: a, a, in: simd3Add)
  expectEqual(SIMD3<Double>(2, 4, 6), val2)
  expectEqual((g, g), bp2(g))
  
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
  let (val3, bp3) = valueWithPullback(at: 5, a, in: simd3Subtract)
  expectEqual(SIMD3<Double>(4, 3, 2), val3)
  expectEqual((3, SIMD3<Double>(-1, -1, -1)), bp3(g))
  
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
  let (val4, bp4) = valueWithPullback(at: a, 5, in: simd3Multiply)
  expectEqual(SIMD3<Double>(5, 10, 15), val4)
  expectEqual((SIMD3<Double>(5, 5, 5), 6), bp4(g))
  
  func testSum<Scalar, SIMDType: SIMD>(x: SIMDType) -> Scalar
    where SIMDType.Scalar == Scalar,
          SIMDType : Differentiable,
          Scalar : BinaryFloatingPoint & Differentiable,
          Scalar.TangentVector : BinaryFloatingPoint,
          SIMDType.TangentVector == SIMDType {
    return x.sum()
  }
  func simd3Sum(x: SIMD3<Double>) -> Double { testSum(x: x) }
  let (val5, bp5) = valueWithPullback(at: a, in: simd3Sum)
  expectEqual(6, val5)
  expectEqual(SIMD3<Double>(7, 7, 7), bp5(7))
}

runAllTests()
