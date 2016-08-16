// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var StrideTestSuite = TestSuite("Stride")

StrideTestSuite.test("to") {
  checkSequence(Array(0...4), stride(from: 0, to: 5, by: 1))
  checkSequence(Array(1...5).reversed(), stride(from: 5, to: 0, by: -1))
}

StrideTestSuite.test("through") {
  checkSequence(Array(0...5), stride(from: 0, through: 5, by: 1))
  checkSequence(Array(0...5).reversed(), stride(from: 5, through: 0, by: -1))
}

runAllTests()
