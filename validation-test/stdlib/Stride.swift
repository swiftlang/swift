// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var StrideTestSuite = TestSuite("Stride")

StrideTestSuite.test("to") {
  checkSequence(Array(0...4), stride(from: 0, to: 5, by: 1))
  checkSequence(Array(1...5).reversed(), stride(from: 5, to: 0, by: -1))
  if #available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *) {
    // This used to crash before https://github.com/apple/swift/pull/34860
    checkSequence(stride(from: 0, to: 127, by: 3).map { Int8($0) },
      stride(from: 0, to: 127 as Int8, by: 3))
  }
}

StrideTestSuite.test("through") {
  checkSequence(Array(0...5), stride(from: 0, through: 5, by: 1))
  checkSequence(Array(0...5).reversed(), stride(from: 5, through: 0, by: -1))
  if #available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *) {
    // This used to crash before https://github.com/apple/swift/pull/34860
    checkSequence(stride(from: 0, through: 127, by: 3).map { Int8($0) },
                  stride(from: 0, through: 127 as Int8, by: 3))
  }
}

runAllTests()
