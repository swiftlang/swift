// RUN: %target-run-simple-swift
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

RandomTests.test("UInt8ArrayInitializers") {
  let _ = ARC4RandomEngine(seed: [0])
  let _ = ARC4RandomEngine(seed: [1, 2, 3, 4, 5, 6, 7])
  let _ = ARC4RandomEngine(seed: Array(repeating: UInt8.max, count: 256))
}

RandomTests.test("BinaryIntegerInitializers") {
  let _ = ARC4RandomEngine(seed: UInt32(0))
  let _ = ARC4RandomEngine(seed: UInt64(1))
  let _ = ARC4RandomEngine(seed: Int16(-42))
}

RandomTests.test("TestVector1") {
  let engine = ARC4RandomEngine(seed: [1, 2, 3, 4, 5, 6, 7, 8])
  let expected: [UInt8] = [0x97, 0xab, 0x8a, 0x1b, 0xf0, 0xaf, 0xb9, 0x61,
                           0x32, 0xf2, 0xf6, 0x72, 0x58, 0xda, 0x15, 0xa8,
                           0x82, 0x63, 0xef, 0xdb, 0x45, 0xc4, 0xa1, 0x86,
                           0x84, 0xef, 0x87, 0xe6, 0xb1, 0x9e, 0x5b, 0x09]
  let actual: [UInt8] = engine.generate(count: 32)
  expectEqual(actual, expected)
}

RandomTests.test("TestVector2") {
  let engine = ARC4RandomEngine(seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
                                       0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
                                       0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
                                       0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  let expected: [UInt8] = [0x37, 0x0b, 0x1c, 0x1f, 0xe6, 0x55, 0x91, 0x6d,
                           0x97, 0xfd, 0x0d, 0x47, 0xca, 0x1d, 0x72, 0xb8]
  let _ = engine.generate(count: 0x1000)
  let actual: [UInt8] = engine.generate(count: 16)
  expectEqual(actual, expected)
}

RandomTests.test("CopyDoesntBreakOriginal") {
  let engine1 = ARC4RandomEngine(seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
                                       0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
                                       0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
                                       0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  let expected: [UInt8] = [0x37, 0x0b, 0x1c, 0x1f, 0xe6, 0x55, 0x91, 0x6d,
                           0x97, 0xfd, 0x0d, 0x47, 0xca, 0x1d, 0x72, 0xb8]

  let _ = engine1.generate(count: 0x800)
  let engine2 = ARC4RandomEngine(engine1)
  let _ = engine2.generate(count: 1)
  let _ = engine1.generate(count: 0x800)
  let actual: [UInt8] = engine1.generate(count: 16)
  expectEqual(actual, expected)

}

RandomTests.test("CopyIsDistinctFromOriginal") {
  let engine1 = ARC4RandomEngine(seed: [0x1a, 0xda, 0x31, 0xd5, 0xcf, 0x68, 0x82, 0x21,
                                       0xc1, 0x09, 0x16, 0x39, 0x08, 0xeb, 0xe5, 0x1d,
                                       0xeb, 0xb4, 0x62, 0x27, 0xc6, 0xcc, 0x8b, 0x37,
                                       0x64, 0x19, 0x10, 0x83, 0x32, 0x22, 0x77, 0x2a])
  let expected: [UInt8] = [0x37, 0x0b, 0x1c, 0x1f, 0xe6, 0x55, 0x91, 0x6d,
                           0x97, 0xfd, 0x0d, 0x47, 0xca, 0x1d, 0x72, 0xb8]

  let _ = engine1.generate(count: 0x800)
  let engine2 = ARC4RandomEngine(engine1)
  let _ = engine1.generate(count: 0x10000)
  let _ = engine2.generate(count: 0x800)
  let actual2: [UInt8] = engine2.generate(count: 16)
  expectEqual(actual2, expected)
}

RandomTests.test("UniformDistributionIsInRange") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(a: 10, b: 42)
  for _ in 0 ..< 1000 {
    let r = dist.generate(using: engine)
    expectGE(r, 10)
    expectLT(r, 42)
  }
}

RandomTests.test("UniformDistributionMean") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(a: 10, b: 50)
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += dist.generate(using: engine)
  }
  mean /= Double(count)
  expectNearlyEqual(mean, 30, byError: 0.25)
}

RandomTests.test("UniformDistributionStdDev") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = UniformFloatingPointDistribution<Double>(a: 10, b: 50)
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.generate(using: engine)
    mean += r
    meanSquare += r * r
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, (50 - 10) / 12.squareRoot(), byError: 0.25)
}

RandomTests.test("NormalDistributionMean") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = NormalFloatingPointDistribution<Double>(mean: 10, standardDeviation: 50)
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += dist.generate(using: engine)
  }
  mean /= Double(count)
  expectNearlyEqual(mean, 10, byError: 0.25)
}

RandomTests.test("NormalDistributionStdDev") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = NormalFloatingPointDistribution<Double>(mean: 10, standardDeviation: 50)
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.generate(using: engine)
    mean += r
    meanSquare += r * r
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, 50, byError: 0.25)
}

RandomTests.test("UniformIntDistributionMean") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = UniformIntegerDistribution<UInt16>()
  let count = 100000
  var mean: Double = 0
  for _ in 0 ..< count {
    mean += Double(dist.generate(using: engine))
  }
  mean /= Double(count)
  expectNearlyEqual(mean, pow(2.0, 15.0), byError: 1000)
}

RandomTests.test("UniformIntDistributionStdDev") {
  let engine = ARC4RandomEngine(seed: UInt64(42))
  let dist = UniformIntegerDistribution<UInt16>()
  let count = 100000
  var mean: Double = 0
  var meanSquare: Double = 0
  for _ in 0 ..< count {
    let r = dist.generate(using: engine)
    mean += Double(r)
    meanSquare += Double(r) * Double(r)
  }
  mean /= Double(count)
  meanSquare /= Double(count)
  let stdDev = (meanSquare - mean * mean).squareRoot()
  expectNearlyEqual(stdDev, pow(2.0, 16.0) / 12.squareRoot(), byError: 1000)
}

runAllTests()
