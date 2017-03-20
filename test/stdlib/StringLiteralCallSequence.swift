// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for Swift 4's ExpressibleByStringInterpolation redesign.
//

import StdlibUnittest

var StringLiteralCallSequenceTests = TestSuite("StringLiteralCallSequence")

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/simple literal") {
  expectEqual(
    "Hello, world!", 
    TracingString(stringLiteral: "Hello, world!")
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/empty literal") {
  expectEqual(
    "", 
    TracingString(stringLiteral: "")
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/simple interpolation") {
  expectEqual(
    "Hello, \(1)!", 
    TracingString(stringInterpolation: 
      TracingString(stringLiteral: "Hello, "), 
      TracingString(stringInterpolationSegment: .init(forInterpolation: 1)), 
      TracingString(stringLiteral: "!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/adjacent interpolations") {
  expectEqual(
    "Hello, \(1)\(2)!", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: "Hello, "), 
      TracingString(stringInterpolationSegment: .init(forInterpolation: 1)), 
      TracingString(stringLiteral: ""),
      TracingString(stringInterpolationSegment: .init(forInterpolation: 2)),
      TracingString(stringLiteral: "!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/leading interpolation") {
  expectEqual(
    "\(1)!", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: ""), 
      TracingString(stringInterpolationSegment: .init(forInterpolation: 1)), 
      TracingString(stringLiteral: "!")
    )
  )
}


StringLiteralCallSequenceTests.test("StringLiteralCallSequence/trailing interpolation") {
  expectEqual(
    "Hello, \(1)", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: "Hello, "), 
      TracingString(stringInterpolationSegment: .init(forInterpolation: 1)), 
      TracingString(stringLiteral: "")
    )
  )
}

runAllTests()
