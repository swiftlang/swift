// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var StrideTestSuite = TestSuite("Stride")

StrideTestSuite.test("to") {
  checkSequence(Array(0...4), stride(from: 0, to: 5, by: 1))
  checkSequence(Array(1...5).reversed(), stride(from: 5, to: 0, by: -1))
  if #available(SwiftStdlib 5.5, *) {
    // This used to crash before https://github.com/apple/swift/pull/34860
    checkSequence(stride(from: 0, to: 127, by: 3).map { Int8($0) },
      stride(from: 0, to: 127 as Int8, by: 3))
  }
}

StrideTestSuite.test("through") {
  checkSequence(Array(0...5), stride(from: 0, through: 5, by: 1))
  checkSequence(Array(0...5).reversed(), stride(from: 5, through: 0, by: -1))
  if #available(SwiftStdlib 5.5, *) {
    // This used to crash before https://github.com/apple/swift/pull/34860
    checkSequence(stride(from: 0, through: 127, by: 3).map { Int8($0) },
                  stride(from: 0, through: 127 as Int8, by: 3))
  }
}

if #available(SwiftStdlib 6.3, *) {
  StrideTestSuite.test("Unicode.Scalar") {
    func checkValues(
      actual: some RandomAccessCollection<Unicode.Scalar>,
      expected: Range<UInt32>...
    ) {
      let actualValues = actual.lazy.map { $0.value }
      let expectedValues = expected.joined()
      expectTrue(expectedValues.elementsEqual(actualValues))
    }
    let lowerRange: Range<UInt32> = 0 ..< 0xD800
    let upperRange: Range<UInt32> = 0xE000 ..< 0x110000
    do {
      let a: Range<Unicode.Scalar> = "\0" ..< "\u{10FFFF}"
      let b = stride(from: a.lowerBound, to: a.upperBound, by: +1)
      let c = stride(from: a.upperBound, to: a.lowerBound, by: -1)
      checkValues(actual: a, expected: lowerRange, upperRange.dropLast())
      expectEqual(a.count, lowerRange.count + upperRange.count - 1)
      expectTrue(a.elementsEqual(b))
      expectTrue(a.dropFirst().reversed().elementsEqual(c.dropFirst()))
    }
    do {
      let a: ClosedRange<Unicode.Scalar> = "\0" ... "\u{10FFFF}"
      let b = stride(from: a.lowerBound, through: a.upperBound, by: +1)
      let c = stride(from: a.upperBound, through: a.lowerBound, by: -1)
      checkValues(actual: a, expected: lowerRange, upperRange)
      expectEqual(a.count, lowerRange.count + upperRange.count)
      expectTrue(a.elementsEqual(b))
      expectTrue(a.reversed().elementsEqual(c))
    }
  }
}

runAllTests()
