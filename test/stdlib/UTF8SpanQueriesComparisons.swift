// RUN: %target-run-stdlib-swift(-enable-experimental-feature Span) -enable-experimental-feature Span %S/Inputs/

// REQUIRES: executable_test

import Swift
import StdlibUnittest

var suite = TestSuite("UTF8SpanQueriesComparisons")
defer { runAllTests() }

@available(SwiftStdlib 6.1, *)
extension Array where Element == UInt8 {
  func withSpan<R>(_ f: (Span<Element>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
  func withUTF8Span<R>(_ f: (UTF8Span) throws -> R) rethrows -> R {
    try self.withSpan { span in
      try f(try! UTF8Span(_validating: span))
    }
  }
}

if #available(SwiftStdlib 6.1, *) {
  suite.test("UTF8Span/tilde equals") {
    Array("abcdefg".utf8).withUTF8Span { utf8Span in
      switch utf8Span {
      case "def":
        expectationFailure(
          "unexpected pattern match",
          trace: "",
          stackTrace: SourceLocStack().withCurrentLoc())
      case "abcdef":
        expectationFailure(
          "unexpected pattern match",
          trace: "",
          stackTrace: SourceLocStack().withCurrentLoc())
      case "abcdefg ":
        expectationFailure(
          "unexpected pattern match",
          trace: "",
          stackTrace: SourceLocStack().withCurrentLoc())
      case "abcdefg\0":
        expectationFailure(
          "unexpected pattern match",
          trace: "",
          stackTrace: SourceLocStack().withCurrentLoc())
      case "abcdefg":
        break
      default:
        expectationFailure(
          "expected a pattern match",
          trace: "",
          stackTrace: SourceLocStack().withCurrentLoc())
      }
    }
  }
}

// TODO: Rest of this file is in-progress TODOs


/*


isASCII
isKnownNFC
checkForNFC(quickCheck:)
isKnownSingleScalarCharacters
checkForSingleScalarCharacters(quickCheck:)

public func bytesEqual(to other: UTF8Span) -> Bool
public func bytesEqual(to other: some Sequence<UInt8>) -> Bool

public func scalarsEqual(
  to other: some Sequence<Unicode.Scalar>
) -> Bool

public func charactersEqual(
  to other: some Sequence<Character>
) -> Bool

public func isCanonicallyEquivalent(
  to other: UTF8Span
) -> Bool

public func isCanonicallyLessThan(
  _ other: UTF8Span
) -> Bool

*/

// @available(SwiftStdlib 6.1, *)
// private struct QueryTestCase {
//   var content: String

//   var loc: SourceLocStack

//   var isASCII: Bool

//   // TODO: This might become API, or otherwise calculated at init time
//   var isLatinyNFC: Bool {
//     bytes.allSatisfy { $0 < 0xCC }
//   }

//   var isQuickNFC: Bool
//   var isNFC: Bool

//   var isQuickSSC: Bool
//   var isSSC: Bool
// }

// if #available(SwiftStdlib 6.1, *) {
//   suite.test("UTF8Span/queries") {
//   }
// }

// enum ComparisonResult {
//   binaryEqual
//   canonicallyEqual
//   canonicallyLess
//   inequal
// }

// private struct ComparisonTestCase {
//   var content: String
//   var comparisons: [(String, ComparisonResult)]

//   var loc: SourceLocStack
// }

// if #available(SwiftStdlib 6.1, *) {
//   suite.test("UTF8Span/comparisons") {
//     func test()
//   }
// }


/*

  input string, to check the bits and relevant info
  comparison string and expected comparison level

*/


// }