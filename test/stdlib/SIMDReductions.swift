// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let suite = TestSuite("SIMDReductions")

suite.test("integer min/max") {
  let v16: SIMD16<Int8> = [3, -5, 7, 1, 0, 9, -2, 4, 8, 6, -1, 2, 5, 3, 0, 7]
  expectEqual(-5, v16.min())
  expectEqual(9, v16.max())

  let u8: SIMD8<UInt8> = [3, 250, 7, 1, 0, 9, 200, 4]
  expectEqual(0, u8.min())
  expectEqual(250, u8.max())

  let v3: SIMD3<Int32> = [10, -4, 7]
  expectEqual(-4, v3.min())
  expectEqual(10, v3.max())
  let u3: SIMD3<UInt32> = [10, 4, 7]
  expectEqual(4, u3.min())
  expectEqual(10, u3.max())

  let pmin = unsafeBitCast(SIMD4<Int32>(10, -4, 7, .min), to: SIMD3<Int32>.self)
  expectEqual(-4, pmin.min())
  expectEqual(10, pmin.max())
  let pmax = unsafeBitCast(SIMD4<Int32>(10, -4, 7, .max), to: SIMD3<Int32>.self)
  expectEqual(-4, pmax.min())
  expectEqual(10, pmax.max())
}

suite.test("any/all") {
  let zeros = SIMD16<Int32>(repeating: 0)
  let ones = SIMD16<Int32>(repeating: 1)
  expectFalse(any(zeros .== ones))
  expectTrue(all(zeros .== zeros))

  var mixed = zeros
  mixed[5] = 1
  expectTrue(any(mixed .== ones))
  expectFalse(all(mixed .== ones))
}

runAllTests()
