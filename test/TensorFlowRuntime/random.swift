// RUN: %target-run-simple-swift
// RUN: %target-run-disable-deabstraction-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// TensorFlow Random library tests

import StdlibUnittest
import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var RandomTests = TestSuite("Random")

RandomTests.test("ARC4UInt8ArrayInitializers") {
  let _ = ARC4RandomNumberGenerator(seed: [0])
  let _ = ARC4RandomNumberGenerator(seed: [1, 2, 3, 4, 5, 6, 7])
  let _ = ARC4RandomNumberGenerator(seed: Array(repeating: 255, count: 256))
}

RandomTests.test("ARC4BinaryIntegerInitializers") {
  let _ = ARC4RandomNumberGenerator(seed: UInt32(0))
  let _ = ARC4RandomNumberGenerator(seed: UInt64(1))
  let _ = ARC4RandomNumberGenerator(seed: Int16(-42))
}

/// All test vectors used in this suite can be found in RFC 6229.
RandomTests.test("ARC4TestVector1") {
  var rng = ARC4RandomNumberGenerator(seed: [1, 2, 3, 4, 5, 6, 7, 8])
  expectEqual(rng.next(), 0x97ab8a1bf0afb961)
}

RandomTests.test("ARC4TestVector2") {
  var rng = ARC4RandomNumberGenerator(
    seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
           0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
           0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
           0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  for _ in 0 ..< 512 {
    _ = rng.next()
  }
  expectEqual(rng.next(), 0x370b1c1fe655916d)
}

RandomTests.test("ARC4CopyDoesntBreakOriginal") {
  var rng1 = ARC4RandomNumberGenerator(
    seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
           0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
           0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
           0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  for _ in 0 ..< 256 {
    _ = rng1.next()
  }
  var rng2 = rng1
  _ = rng2.next()
  for _ in 0 ..< 256 {
    _ = rng1.next()
  }
  expectEqual(rng1.next(), 0x370b1c1fe655916d)
}

RandomTests.test("ARC4CopyIsDistinctFromOriginal") {
  var rng1 = ARC4RandomNumberGenerator(
    seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
           0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
           0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
           0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  for _ in 0 ..< 256 {
    _ = rng1.next()
  }
  var rng2 = rng1
  for _ in 0 ..< 1000 {
    _ = rng1.next()
  }
  for _ in 0 ..< 256 {
    _ = rng2.next()
  }
  expectEqual(rng2.next(), 0x370b1c1fe655916d)
}

RandomTests.test("UniformDistributionIsInRange") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(lowerBound: 10,
                                                      upperBound: 42)
  for _ in 0 ..< 1000 {
    let r = dist.next(using: &rng)
    expectGE(r, 10)
    expectLT(r, 42)
  }
}

RandomTests.test("UniformDistributionMean") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(lowerBound: 10,
                                                      upperBound: 50)
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += dist.next(using: &rng)
  }
  mean /= Double(count)
  expectNearlyEqual(mean, 30, byError: 0.25)
}

RandomTests.test("UniformDistributionStdDev") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(lowerBound: 10,
                                                      upperBound: 50)
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.next(using: &rng)
    mean += r
    meanSquare += r * r
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, (50 - 10) / 12.squareRoot(), byError: 0.25)
}

RandomTests.test("NormalDistributionMean") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = NormalDistribution<Double>(mean: 10, standardDeviation: 50)
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += dist.next(using: &rng)
  }
  mean /= Double(count)
  expectNearlyEqual(mean, 10, byError: 0.25)
}

RandomTests.test("NormalDistributionStdDev") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = NormalDistribution<Double>(mean: 10, standardDeviation: 50)
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.next(using: &rng)
    mean += r
    meanSquare += r * r
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, 50, byError: 0.25)
}

RandomTests.test("UniformIntDistributionMean") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = UniformIntegerDistribution<UInt16>()
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += Double(dist.next(using: &rng))
  }
  mean /= Double(count)
  expectNearlyEqual(mean, pow(2.0, 15.0), byError: 1000)
}

RandomTests.test("UniformIntDistributionStdDev") {
  var rng = ARC4RandomNumberGenerator(seed: UInt64(42))
  let dist = UniformIntegerDistribution<UInt16>()
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.next(using: &rng)
    mean += Double(r)
    meanSquare += Double(r) * Double(r)
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, pow(2.0, 16.0) / 12.squareRoot(), byError: 1000)
}

runAllTests()
