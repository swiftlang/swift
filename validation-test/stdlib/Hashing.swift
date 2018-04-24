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

  var defaultHasher = Hasher()
  defaultHasher._combine(value)
  expectEqual(defaultHasher.finalize(), defaultHash)

  var customHasher = Hasher(_seed: Hasher._seed)
  customHasher._combine(value)
  expectEqual(customHasher.finalize(), defaultHash)
}

runAllTests()
