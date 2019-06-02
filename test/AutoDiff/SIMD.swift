// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SIMDTests = TestSuite("SIMD")

SIMDTests.test("Identity") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  
  let foo1 = { (x: SIMD4<Float>) -> SIMD4<Float> in
    return x
  }
  let bp1 = pullback(at: a, in: foo1)
  expectEqual(a, bp1(a))
}

SIMDTests.test("Negate") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  
  let foo1 = { (x: SIMD4<Float>) -> SIMD4<Float> in
    return -x
  }
  let bp1 = pullback(at: a, in: foo1)
  expectEqual(-a, bp1(a))
}

//SIMDTests.test("Sum") {
//  let a = SIMD4<Float>(1, 2, 3, 4)
//
//  let foo1 = { (x: SIMD4<Float>) -> Float in
//    return x.sum()
//  }
//  let bp1 = pullback(at: a, in: foo1)
//  expectEqual(SIMD4<Float>(3, 3, 3, 3), bp1(3))
//}

SIMDTests.test("Addition") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD + SIMD
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x + y
  }
  let bp1 = pullback(at: a, a, in: foo1)
  expectEqual((g, g), bp1(g))
  
  // SIMD + Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x + y
  }
  let bp2 = pullback(at: a, 5, in: foo2)
  expectEqual((g, 4), bp2(g))
  
  // Scalar + SIMD
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return y + x
  }
  let bp3 = pullback(at: a, 5, in: foo3)
  expectEqual((g, 4), bp3(g))
}

SIMDTests.test("Multiplication") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)

  // SIMD * SIMD
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x * y
  }
  let bp1 = pullback(at: a, a, in: foo1)
  expectEqual((a, a), bp1(g))

  // SIMD * Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x * y
  }
  let bp2 = pullback(at: a, 5, in: foo2)
  expectEqual((SIMD4<Float>(5, 5, 5, 5), 10), bp2(g))

  // Scalar * SIMD
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return y * x
  }
  let bp3 = pullback(at: a, 5, in: foo3)
  expectEqual((SIMD4<Float>(5, 5, 5, 5), 10), bp3(g))
}

SIMDTests.test("Division") {
  let a = SIMD4<Float>(1, 2, 3, 4)
  let g = SIMD4<Float>(1, 1, 1, 1)
  
  // SIMD / SIMD
  let foo1 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x / y
  }
  let bp1 = pullback(at: a, a, in: foo1)
  let dlhs1 = g / a
  let drhs1 = -1 / a
  expectEqual((dlhs1, drhs1), bp1(g))
  
  // SIMD / Scalar
  let foo2 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return x / y
  }
  let bp2 = pullback(at: a, 5, in: foo2)
  let dlhs2 = g / 5
  let drhs2 = (-a / 25 * g).sum()
  expectEqual((dlhs2, drhs2), bp2(g))
  
  // Scalar / SIMD
  let foo3 = { (x: Float, y: SIMD4<Float>) -> SIMD4<Float> in
    return x / y
  }
  let dlhs3 = (g / a).sum()
  let drhs3 = -5 / (a*a) * g
  let bp3 = pullback(at: 5, a, in: foo3)
  expectEqual((dlhs3, drhs3), bp3(g))
}

runAllTests()
