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
    TracingString(stringLiteral: 
      .stringLiteral("Hello, "), 
      .stringInterpolation(.init(forInterpolation: 1)), 
      .stringLiteral("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/multi-parameter interpolation") {
  expectEqual(
    "Hello, \(1, radix: 16, uppercase: true)!", 
    TracingString(stringLiteral: 
      .stringLiteral("Hello, "), 
      .stringInterpolation(.init(1, radix: 16, uppercase: true)), 
      .stringLiteral("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/multi-parameter defaulted interpolation") {
  expectEqual(
    "Hello, \(1, uppercase: true)!", 
    TracingString(stringLiteral: 
      .stringLiteral("Hello, "), 
      .stringInterpolation(.init(1, uppercase: true)), 
      .stringLiteral("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/adjacent interpolations") {
  expectEqual(
    "Hello, \(1)\(2)!", 
    TracingString(stringLiteral:
      .stringLiteral("Hello, "), 
      .stringInterpolation(.init(forInterpolation: 1)), 
      .stringLiteral(""),
      .stringInterpolation(.init(forInterpolation: 2)),
      .stringLiteral("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/leading interpolation") {
  expectEqual(
    "\(1)!", 
    TracingString(stringLiteral:
      .stringLiteral(""), 
      .stringInterpolation(.init(forInterpolation: 1)), 
      .stringLiteral("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/trailing interpolation") {
  expectEqual(
    "Hello, \(1)", 
    TracingString(stringLiteral:
      .stringLiteral("Hello, "), 
      .stringInterpolation(.init(forInterpolation: 1)), 
      .stringLiteral("")
    )
  )
}

runAllTests()
