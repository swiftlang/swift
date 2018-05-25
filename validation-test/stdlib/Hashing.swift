// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import Swift
import SwiftPrivate
import StdlibUnittest


var HashingTestSuite = TestSuite("Hashing")

func checkHash(
  for value: UInt64,
  withSeed seed: (UInt64, UInt64),
  expected: UInt64,
  file: String = #file, line: UInt = #line
) {
  var hasher = Hasher(_seed: seed)
  hasher._combine(value)
  let hash = hasher.finalize()
  expectEqual(
    hash, Int(truncatingIfNeeded: expected),
    file: file, line: line)
}

HashingTestSuite.test("Hasher/CustomKeys") {
  // This assumes Hasher implements SipHash-1-3.
  checkHash(for: 0, withSeed: (0, 0), expected: 0xbd60acb658c79e45)
  checkHash(for: 0, withSeed: (0, 1), expected: 0x1ce32b0b44e61175)
  checkHash(for: 0, withSeed: (1, 0), expected: 0x9c44b7c8df2ca74b)
  checkHash(for: 0, withSeed: (1, 1), expected: 0x9653ca0a3b455506)
  checkHash(for: 0, withSeed: (.max, .max), expected: 0x3ab336a4895e4d36)

  checkHash(for: 1, withSeed: (0, 0), expected: 0x1e9f734161d62dd9)
  checkHash(for: 1, withSeed: (0, 1), expected: 0xb6fcf32d09f76cba)
  checkHash(for: 1, withSeed: (1, 0), expected: 0xacb556b13007504a)
  checkHash(for: 1, withSeed: (1, 1), expected: 0x7defec680db51d24)
  checkHash(for: 1, withSeed: (.max, .max), expected: 0x212798441870ef6b)

  checkHash(for: .max, withSeed: (0, 0), expected: 0x2f205be2fec8e38d)
  checkHash(for: .max, withSeed: (0, 1), expected: 0x3ff7fa33381ecf7b)
  checkHash(for: .max, withSeed: (1, 0), expected: 0x404afd8eb2c4b22a)
  checkHash(for: .max, withSeed: (1, 1), expected: 0x855642d657c1bd46)
  checkHash(for: .max, withSeed: (.max, .max), expected: 0x5b16b7a8181980c2)
}

HashingTestSuite.test("Hasher/DefaultKey") {
  let value: UInt64 = 0x0102030405060708

  let defaultHash = _hashValue(for: value)

  let rawHash = value._rawHashValue(seed: Hasher._seed)
  expectEqual(rawHash, defaultHash)

  let oneShotHash = Hasher._hash(seed: Hasher._seed, value)
  expectEqual(oneShotHash, defaultHash)

  var defaultHasher = Hasher()
  defaultHasher._combine(value)
  expectEqual(defaultHasher.finalize(), defaultHash)

  var customHasher = Hasher(_seed: Hasher._seed)
  customHasher._combine(value)
  expectEqual(customHasher.finalize(), defaultHash)
}

HashingTestSuite.test("Hashing/TopLevelHashing/UInt64") {
  func checkTopLevelHash(
    for value: UInt64,
    seed: (UInt64, UInt64),
    file: String = #file,
    line: UInt = #line) {
    var hasher = Hasher(_seed: seed)
    hasher._combine(value)
    let expected = hasher.finalize()
    let actual = Hasher._hash(seed: seed, value)
    expectEqual(actual, expected, file: file, line: line)
  }
  checkTopLevelHash(for: 0, seed: (0, 0))
  checkTopLevelHash(for: 1, seed: (0, 0))
  checkTopLevelHash(for: 1, seed: (1, 0))
  checkTopLevelHash(for: 1, seed: (1, 1))
  checkTopLevelHash(for: 0x0102030405060708, seed: (1, 1))
  checkTopLevelHash(
    for: 0x0102030405060708,
    seed: (0x0807060504030201, 0x090a0b0c0d0e0f))
  checkTopLevelHash(for: UInt64.max, seed: (1, 1))
  checkTopLevelHash(for: UInt64.max, seed: (UInt64.max, UInt64.max))
}

HashingTestSuite.test("Hashing/TopLevelHashing/UInt") {
  func checkTopLevelHash(
    for value: UInt,
    seed: (UInt64, UInt64),
    file: String = #file,
    line: UInt = #line) {
    var hasher = Hasher(_seed: seed)
    hasher._combine(value)
    let expected = hasher.finalize()
    let actual = Hasher._hash(seed: seed, value)
    expectEqual(actual, expected, file: file, line: line)
  }
  checkTopLevelHash(for: 0, seed: (0, 0))
  checkTopLevelHash(for: 1, seed: (0, 0))
  checkTopLevelHash(for: 1, seed: (1, 0))
  checkTopLevelHash(for: 1, seed: (1, 1))
  checkTopLevelHash(
    for: UInt(truncatingIfNeeded: 0x0102030405060708 as UInt64),
    seed: (1, 1))
  checkTopLevelHash(
    for: UInt(truncatingIfNeeded: 0x0102030405060708 as UInt64),
    seed: (0x8877665544332211, 0x1122334455667788))
  checkTopLevelHash(for: UInt.max, seed: (1, 1))
  checkTopLevelHash(for: UInt.max, seed: (UInt64.max, UInt64.max))
}

HashingTestSuite.test("Hashing/TopLevelHashing/PartialUInt64") {
  func checkTopLevelHash(
    for value: UInt64,
    count: Int,
    seed: (UInt64, UInt64),
    file: String = #file,
    line: UInt = #line) {
    var hasher = Hasher(_seed: seed)
    hasher._combine(bytes: value, count: count)
    let expected = hasher.finalize()
    let actual = Hasher._hash(seed: seed, bytes: value, count: count)
    expectEqual(
      actual,
      expected,
      "seed: \(seed), value: \(value), count: \(count)",
      file: file,
      line: line)
  }
  for seed: (UInt64, UInt64) in [
    (0, 0),
    (1, 0),
    (1, 1),
    (0x1827364554637281, 0xf9e8d7c6b5a49382)
  ] {
    for count in 1 ..< 8 {
      checkTopLevelHash(for: 0, count: count, seed: seed)
    }
    checkTopLevelHash(for: 0x01, count: 1, seed: seed)
    checkTopLevelHash(for: 0x0102, count: 2, seed: seed)
    checkTopLevelHash(for: 0x010203, count: 3, seed: seed)
    checkTopLevelHash(for: 0x01020304, count: 4, seed: seed)
    checkTopLevelHash(for: 0x0102030405, count: 5, seed: seed)
    checkTopLevelHash(for: 0x010203040506, count: 6, seed: seed)
    checkTopLevelHash(for: 0x01020304050607, count: 7, seed: seed)
  }
}

HashingTestSuite.test("Hashing/TopLevelHashing/UnsafeRawBufferPointer") {
  func checkTopLevelHash(
    for buffer: [UInt8],
    seed: (UInt64, UInt64),
    file: String = #file,
    line: UInt = #line) {
    var hasher = Hasher(_seed: seed)
    buffer.withUnsafeBytes { buffer in
      hasher.combine(bytes: buffer)
    }
    let expected = hasher.finalize()
    let actual = buffer.withUnsafeBytes { buffer in
      Hasher._hash(seed: seed, bytes: buffer)
    }
    expectEqual(
      actual,
      expected,
      "seed: \(seed), buffer: \(buffer)",
      file: file,
      line: line)
  }
  for seed: (UInt64, UInt64) in [
    (0, 0),
    (1, 0),
    (1, 1),
    (0x1827364554637281, 0xf9e8d7c6b5a49382)
  ] {
    var zeros: [UInt8] = []
    var integers: [UInt8] = []
    for i: UInt8 in 0 ..< 20 {
      zeros.append(0)
      checkTopLevelHash(for: zeros, seed: seed)
      integers.append(i)
      checkTopLevelHash(for: integers, seed: seed)
    }
  }
}

runAllTests()
