// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for Swift 4's ExpressibleByStringInterpolation redesign.
//

import StdlibUnittest

struct TracingString {
  enum Initializer {
    case stringLiteral(String)
    case unicodeScalarLiteral(UnicodeScalar)
    case extendedGraphemeClusterLiteral(Character)
    case stringInterpolationSegment(AnyHashable)
    case stringInterpolation([TracingString])
  }
  var initializer: Initializer
}

extension TracingString: ExpressibleByStringInterpolation {
  init(stringLiteral value: String) {
    initializer = .stringLiteral(value)
  }

  init(unicodeScalarLiteral value: UnicodeScalar) {
    initializer = .unicodeScalarLiteral(value)
  }

  init(extendedGraphemeClusterLiteral value: Character) {
    initializer = .extendedGraphemeClusterLiteral(value)
  }

  init<T>(stringInterpolationSegment value: T) {
    guard let hashable = value as? AnyHashable else {
      preconditionFailure("Only supports Hashable interpolated values")
    }
    initializer = .stringInterpolationSegment(hashable)
  }

  init(stringInterpolation segments: TracingString...) {
    initializer = .stringInterpolation(segments)
  }
}

extension TracingString: Hashable {
  static func == (lhs: TracingString, rhs: TracingString) -> Bool {
    switch (lhs.initializer, rhs.initializer) {
    case let (.stringLiteral(l), .stringLiteral(r)):
      return l == r
    case let (.unicodeScalarLiteral(l), .unicodeScalarLiteral(r)):
      return l == r
    case let (.extendedGraphemeClusterLiteral(l), .extendedGraphemeClusterLiteral(r)):
      return l == r
    case let (.stringInterpolationSegment(l), .stringInterpolationSegment(r)):
      return l == r
    case let (.stringInterpolation(l), .stringInterpolation(r)):
      return l == r
    case (.stringLiteral, _),
        (.unicodeScalarLiteral, _),
        (.extendedGraphemeClusterLiteral, _),
        (.stringInterpolationSegment, _),
        (.stringInterpolation, _):
        return false
    }
  }
  
  var hashValue: Int {
    switch initializer {
    case .stringLiteral(let value):
      return value.hashValue
    case .unicodeScalarLiteral(let value):
      return value.hashValue
    case .extendedGraphemeClusterLiteral(let value):
      return value.hashValue
    case .stringInterpolationSegment(let value):
      return value.hashValue
    case .stringInterpolation(let segments):
      return segments.reduce(segments.count) { $0 ^ $1.hashValue }
    }
  }
}

extension TracingString: CustomDebugStringConvertible {
  var debugDescription: String {
    switch initializer {
    case .stringLiteral(let value):
      return "TracingString(stringLiteral: \(String(reflecting: value)))"
    case .unicodeScalarLiteral(let value):
      return "TracingString(unicodeScalarLiteral: \(String(reflecting: value)))"
    case .extendedGraphemeClusterLiteral(let value):
      return "TracingString(extendedGraphemeClusterLiteral: \(String(reflecting: value)))"
    case .stringInterpolationSegment(let value):
      return "TracingString(stringInterpolationSegment: \(String(reflecting: value)))"
    case .stringInterpolation(let segments):
      let segmentsString = segments.map(String.init(reflecting:)).joined(separator: ", ")
      return "TracingString(stringInterpolation: \(segmentsString))"
    }
  }
}

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
      TracingString(stringInterpolationSegment: 1), 
      TracingString(stringLiteral: "!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/adjacent interpolations") {
  expectEqual(
    "Hello, \(1)\(2)!", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: "Hello, "), 
      TracingString(stringInterpolationSegment: 1), 
      TracingString(stringLiteral: ""),
      TracingString(stringInterpolationSegment: 2),
      TracingString(stringLiteral: "!")
    )
  )
}

StringLiteralCallSequenceTests.test("StringLiteralCallSequence/leading interpolation") {
  expectEqual(
    "\(1)!", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: ""), 
      TracingString(stringInterpolationSegment: 1), 
      TracingString(stringLiteral: "!")
    )
  )
}


StringLiteralCallSequenceTests.test("StringLiteralCallSequence/trailing interpolation") {
  expectEqual(
    "Hello, \(1)", 
    TracingString(stringInterpolation:
      TracingString(stringLiteral: "Hello, "), 
      TracingString(stringInterpolationSegment: 1), 
      TracingString(stringLiteral: "")
    )
  )
}

runAllTests()
