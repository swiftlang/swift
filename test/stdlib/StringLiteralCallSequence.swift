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
      .literal("Hello, "), 
      .interpolation(.init(stringInterpolationSegment: 1)), 
      .literal("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/multi-parameter interpolation") {
  expectEqual(
    "Hello, \(1, radix: 16, uppercase: true)!", 
    TracingString(stringInterpolation: 
      .literal("Hello, "), 
      .interpolation(.init(1, radix: 16, uppercase: true)), 
      .literal("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/multi-parameter defaulted interpolation") {
  expectEqual(
    "Hello, \(1, uppercase: true)!", 
    TracingString(stringInterpolation: 
      .literal("Hello, "), 
      .interpolation(.init(1, uppercase: true)), 
      .literal("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/adjacent interpolations") {
  expectEqual(
    "Hello, \(1)\(2)!", 
    TracingString(stringInterpolation:
      .literal("Hello, "), 
      .interpolation(.init(stringInterpolationSegment: 1)), 
      .literal(""),
      .interpolation(.init(stringInterpolationSegment: 2)),
      .literal("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/leading interpolation") {
  expectEqual(
    "\(1)!", 
    TracingString(stringInterpolation:
      .literal(""), 
      .interpolation(.init(stringInterpolationSegment: 1)), 
      .literal("!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/trailing interpolation") {
  expectEqual(
    "Hello, \(1)", 
    TracingString(stringInterpolation:
      .literal("Hello, "), 
      .interpolation(.init(stringInterpolationSegment: 1)), 
      .literal("")
    )
  )
}

runAllTests()
