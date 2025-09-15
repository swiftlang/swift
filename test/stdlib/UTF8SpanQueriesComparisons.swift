// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test

import Swift
import StdlibUnittest

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  static func ~=(_ lhs: StaticString, _ rhs: UTF8Span) -> Bool {
    return lhs.withUTF8Buffer { str in
      rhs._withUnsafeBufferPointer { span in
        str.elementsEqual(span)
      }
    }
  }
}

var suite = TestSuite("UTF8SpanQueriesComparisons")
defer { runAllTests() }

@available(SwiftStdlib 6.2, *)
extension Array where Element == UInt8 {
  func withSpan<R>(_ f: (Span<Element>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
  func withUTF8Span<R>(_ f: (UTF8Span) throws -> R) rethrows -> R {
    try self.withSpan { span in
      try f(try! UTF8Span(validating: span))
    }
  }
}

if #available(SwiftStdlib 6.2, *) {
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

  suite.test("UTF8Span/Sequence equal") {
    // // A string and its canonical equivalent
    // let testCases: [(String, String?)] = [
    //   ("abdefg", nil)
    //   ("café", "cafe\u{301}")
    // ]
  }

  suite.test("UTF8Span/isKnownASCII") {
    let tests: [(String, Bool)] = [
      ("abc", true),
      ("abcdefghil1235@#% _/.sladfj234 ", true),
      ("abcdefghil1\u{80}sladfj234 ", false),
    ]

    for (test, expected) in tests {
      Array(test.utf8).withUTF8Span {
        expectEqual(expected, $0.isKnownASCII)
      }
    }
  }

  suite.test("UTF8Span/isKnownNFC") {
    enum Normalness {
      case known
      case quickCheck
      case fullCheck
      case notNFC
    }

    let nfcQCNo = "\u{0374}"
    let nfcQCYes = "\u{0374}"

    let tests: [(String, Normalness)] = [
      ("abc", .known),
      ("abcdefghil123567890", .known),
      ("abcdefghil1\u{299}123345678 ", .quickCheck),
      ("abc日曜日xyz", .quickCheck),
      ("abcde日曜日\u{301}", .fullCheck),
      ("abcde\u{301}fghijkl", .notNFC),
    ]

    for (test, expected) in tests {
      Array(test.utf8).withUTF8Span {
        var span = $0
        if span.isKnownNFC {
          expectEqual(expected, .known)
        } else if span.checkForNFC(quickCheck: true) {
          expectEqual(expected, .quickCheck)
        } else if span.checkForNFC(quickCheck: false) {
          expectEqual(expected, .fullCheck)
        } else {
          expectEqual(expected, .notNFC)
        }
      }
    }
  }

  suite.test("UTF8Span/canonical equivalence") {

    // TODO: refactor to be test-case declaration driven, and add more tests...
    //   `(normalized: String, variants: [String], lessThan: String, greaterThan: String)`

    let precomposedStr = "café"
    let decomposedStr = "cafe\u{301}"

    let precomposed = Array(precomposedStr.utf8)
    let decomposed = Array(decomposedStr.utf8)

    precomposed.withSpan { pre in
      let utf8Precomposed = try! UTF8Span(validating: pre)
      decomposed.withSpan { de in
        let utf8Decomposed = try! UTF8Span(validating: de)

        // print("scalars for \(precomposedStr.unicodeScalars)")
        // var preScalars = utf8Precomposed.makeUnicodeScalarIterator()
        // while let s = preScalars.next() {
        //   print(s)
        // }

        // print("scalars for \(decomposedStr.unicodeScalars)")
        // var deScalars = utf8Decomposed.makeUnicodeScalarIterator()
        // while let s = deScalars.next() {
        //   print(s)
        // }
        
        expectTrue(utf8Precomposed.isCanonicallyEquivalent(to: utf8Decomposed))

        expectTrue(utf8Precomposed.bytesEqual(to: precomposedStr.utf8))
        expectFalse(utf8Precomposed.bytesEqual(to: decomposedStr.utf8))

        expectTrue(utf8Decomposed.bytesEqual(to: decomposedStr.utf8))
        expectFalse(utf8Decomposed.bytesEqual(to: precomposedStr.utf8))

        expectTrue(utf8Precomposed.unicodeScalarsEqual(to: precomposedStr.unicodeScalars))
        expectFalse(utf8Precomposed.unicodeScalarsEqual(to: decomposedStr.unicodeScalars))

        expectTrue(utf8Decomposed.unicodeScalarsEqual(to: decomposedStr.unicodeScalars))
        expectFalse(utf8Decomposed.unicodeScalarsEqual(to: precomposedStr.unicodeScalars))

        expectTrue(utf8Precomposed.charactersEqual(to: precomposedStr))
        expectTrue(utf8Precomposed.charactersEqual(to: decomposedStr))

        expectTrue(utf8Decomposed.charactersEqual(to: decomposedStr))
        expectTrue(utf8Decomposed.charactersEqual(to: precomposedStr))

        // Equivalence means no-one is less than the other
        expectFalse(utf8Decomposed.isCanonicallyLessThan(utf8Precomposed))
        expectFalse(utf8Precomposed.isCanonicallyLessThan(utf8Decomposed))

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

// @available(SwiftStdlib 6.2, *)
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

// if #available(SwiftStdlib 6.2, *) {
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

// if #available(SwiftStdlib 6.2, *) {
//   suite.test("UTF8Span/comparisons") {
//     func test()
//   }
// }


/*

  input string, to check the bits and relevant info
  comparison string and expected comparison level

*/


// }