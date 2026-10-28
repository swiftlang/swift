// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest

let SIMDGenericFPTests = TestSuite("SIMD Generic FP")

func getNegativeZero<T>(of: T.Type) -> T
where T: SIMD, T.Scalar: BinaryFloatingPoint {
  -T()
}

SIMDGenericFPTests.test("negation") {
  let negZero = getNegativeZero(of: SIMD4<Float>.self)
  expectEqual(negZero[0].sign, .minus)
  expectEqual(negZero[1].sign, .minus)
  expectEqual(negZero[2].sign, .minus)
  expectEqual(negZero[3].sign, .minus)
  expectEqual(negZero[0], 0)
  expectEqual(negZero[1], 0)
  expectEqual(negZero[2], 0)
  expectEqual(negZero[3], 0)
}

runAllTests()
