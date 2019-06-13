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
  
  let foo1 = { (x: Float) -> SIMD4<Float> in
    return SIMD4<Float>(repeating: 2 * x)
  }
  let (val, bp1) = valueWithPullback(at: 5, in: foo1)
  expectEqual(SIMD4<Float>(10, 10, 10, 10), val)
  expectEqual(8, bp1(g))
}

SIMDTests.test("Sum") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  
  let foo1 = { (x: SIMD4<Float>) -> Float in
    return x.sum()
  }
  let (val, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(10, val)
  expectEqual(SIMD4<Float>(3, 3, 3, 3), bp1(3))
}

SIMDTests.test("Identity") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  let foo1 = { (x: SIMD4<Float>) -> SIMD4<Float> in
    return x
  }
  let (val, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(a, val)
  expectEqual(g, bp1(g))
}

SIMDTests.test("Negate") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  let foo1 = { (x: SIMD4<Float>) -> SIMD4<Float> in
    return -x
  }
  let (val, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(-a, val)
  expectEqual(-g, bp1(g))
}

SIMDTests.test("subscript") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  let foo1 = { (x: SIMD4<Float>) -> Float in
    return x[3]
  }
  
  let (val, bp1) = valueWithPullback(at: a, in: foo1)
  expectEqual(4, val)
  expectEqual(SIMD4<Float>(0, 0, 0, 7), bp1(7))
}

SIMDTests.test("Addition") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD + SIMD
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x + y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(2, 4, 6, 8), val1)
  expectEqual((g, g), bp1(g))
  
  // SIMD + Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x + y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(6, 7, 8, 9), val2)
  expectEqual((g, 4), bp2(g))
  
  // Scalar + SIMD
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
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
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x - y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(SIMD4<Float>(0, 0, 0, 0), val1)
  expectEqual((g, -g), bp1(g))
  
  // SIMD - Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x - y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(SIMD4<Float>(-4, -3, -2, -1), val2)
  expectEqual((g, -4), bp2(g))
  
  // Scalar - SIMD
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
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
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x * y
  }
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(a * a, val1)
  expectEqual((a, a), bp1(g))

  // SIMD * Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x * y
  }
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(a * 5, val2)
  expectEqual((SIMD4<Float>(5, 5, 5, 5), 10), bp2(g))

  // Scalar * SIMD
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
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
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x / y
  }
  let dlhs1 = g / a
  let drhs1 = -1 / a
  let (val1, bp1) = valueWithPullback(at: a, a, in: foo1)
  expectEqual(a / a, val1)
  expectEqual((dlhs1, drhs1), bp1(g))
  
  // SIMD / Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x / y
  }
  let dlhs2 = g / 5
  let drhs2 = (-a / 25 * g).sum()
  let (val2, bp2) = valueWithPullback(at: a, 5, in: foo2)
  expectEqual(a / 5, val2)
  expectEqual((dlhs2, drhs2), bp2(g))
  
  // Scalar / SIMD
  let foo3 = { (x: Float, y: SIMD4<Float>) -> SIMD4<Float> in
    return x / y
  }
  let dlhs3 = (g / a).sum()
  let drhs3 = -5 / (a*a) * g
  let (val3, bp3) = valueWithPullback(at: 5, a, in: foo3)
  expectEqual(5 / a, val3)
  expectEqual((dlhs3, drhs3), bp3(g))
}

runAllTests()
