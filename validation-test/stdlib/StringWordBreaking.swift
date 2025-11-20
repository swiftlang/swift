// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: objc_interop

// FIXME: Text segmentation test cases are only available when we have Foundation

import StdlibUnittest
import StdlibUnicodeUnittest
import Foundation

let StringWordBreaking = TestSuite("StringWordBreaking")
defer { runAllTests() }

extension String {
  /// Returns all word boundaries within the string, using a single word
  /// recognizer instance. This is the most efficient way to find word
  /// boundaries, as it processes each scalar exactly once.
  @available(StdlibDeploymentTarget 6.3, *)
  func fastWordBreaks() -> [String.Index] {
    var result: [String.Index] = []
    var i = self.startIndex
    var recognizer = Unicode._WordRecognizer()
    var candidate = i
    while i < self.endIndex {
      let (setCandidate, breakAtCandidate, breakHere) =
        recognizer.hasBreak(before: self.unicodeScalars[i])
      if setCandidate {
        candidate = i
      }
      if breakAtCandidate {
        result.append(candidate)
      }
      if breakHere {
        result.append(i)
      }
      self.unicodeScalars.formIndex(after: &i)
    }
    if recognizer.hasCandidateBreakAtEnd() {
      result.append(candidate)
    }
    result.append(i)
    return result
  }

  /// Return the word boundary position preceding a known boundary within this
  /// string.
  ///
  /// This implements the word boundary specification of [Unicode Annex
  /// #29](https://unicode.org/reports/tr29/#Default_Word_Boundaries). The
  /// algorithm is not stable, and it allows implementers to tailor it to their
  /// needs; accordingly, the result of this operation may vary between Unicode
  /// implementations and system configurations, including versions of the Swift
  /// Standard Library.
  ///
  /// - Note: If the input index is not on a word boundary, then it is first
  /// rounded down to the nearest boundary before starting this operation.
  ///
  /// - Warning: Using this method to iterate over the word breaks in a string
  ///    backward has worst-case complexity that is proportional to the _square_
  ///    of the length of the string. It is usually a better idea to keep a
  ///    cache of known word boundaries, calculated by iterating _forwards_ from
  ///    the start index, or a position returned by
  ///    `_wordIndex(somewhereAtOrBefore:)`.
  ///
  /// - Parameter i: A valid index addressing a word boundary within this
  ///    string.
  /// - Returns: The first word break strictly following `i` in the string.
  @available(StdlibDeploymentTarget 6.3, *)
  public func _wordIndex(before i: String.Index) -> String.Index {
    let i = self.unicodeScalars._index(roundingDown: i)
    var j = _wordIndex(somewhereAtOrBefore: unicodeScalars.index(before: i))

    // We know there is a stable break at `j`, however, the backward search may
    // have skipped over some conditional breaks that it could not fully
    // evaluate. Find the closest actual break that precedes `i` by iterating
    // forward until we reach or jump over it.
    precondition(j < i)
    var recognizer = Unicode._WordRecognizer()
    var bestBreak = j
    var candidate = j
    while j < self.endIndex {
      let r = recognizer.hasBreak(before: self.unicodeScalars[j])
      if r.setCandidate { candidate = j }
      if r.breakAtCandidate {
        guard candidate < i else { break }
        bestBreak = candidate
      }
      if r.breakHere {
        guard j < i else { break }
        bestBreak = j
      }
      self.unicodeScalars.formIndex(after: &j)
    }
    if j == self.endIndex, candidate < i, recognizer.hasCandidateBreakAtEnd() {
      bestBreak = candidate
    }
    precondition(bestBreak < i)
    return bestBreak
  }
}

extension String {
  @available(SwiftStdlib 6.3, *)
  var statefulWords: [String] {
    let breaks = fastWordBreaks()
    var prev = breaks[0]
    return breaks.dropFirst().map { next in
      defer { prev = next }
      return String(self[prev ..< next])
    }
  }

  @available(SwiftStdlib 5.9, *)
  var statelessWords: [String] {
    var result: [String] = []

    var i = startIndex

    while i < endIndex {
      let start = i
      let end = _wordIndex(after: i)

      let substr = self[start ..< end]
      result.append(String(substr))

      i = end
    }

    return result
  }

  @available(SwiftStdlib 6.3, *)
  var backwardWords: [String] {
    var result: [String] = []

    var i = endIndex

    while i > startIndex {
      let end = i
      let start = _wordIndex(before: i)

      let substr = self[start ..< end]
      result.append(String(substr))

      i = start
    }

    return result
  }
}

extension Unicode.Scalar {
  var unicodeNotation: String {
      let v = String(self.value, radix: 16, uppercase: true)
      return "U+\(String(repeating: "0", count: max(0, 4 - v.count)))\(v)"
  }
}

extension String {
  var scalarDescriptions: String {
    return self.unicodeScalars
      .lazy.map { $0.unicodeNotation }
      .joined(separator: " ")
  }
}

#if _runtime(_ObjC)
// The most simple subclass of NSString that CoreFoundation does not know
// about.
class NonContiguousNSString : NSString {
  required init(coder aDecoder: NSCoder) {
    fatalError("don't call this initializer")
  }
  required init(itemProviderData data: Data, typeIdentifier: String) throws {
    fatalError("don't call this initializer")
  }

  override init() {
    _value = []
    super.init()
  }

  @inline(never)
  init(_ value: some Sequence<UInt16>) {
    _value = Array(value)
    super.init()
  }

  @objc(copyWithZone:) override func copy(with zone: NSZone?) -> Any {
    // Ensure that copying this string produces a class that CoreFoundation
    // does not know about.
    return self
  }

  @objc override var length: Int {
    return _value.count
  }

  @objc override func character(at index: Int) -> unichar {
    return _value[index]
  }

  var _value: [UInt16]
}

extension _StringGuts {
  @_silgen_name("$ss11_StringGutsV9isForeignSbvg")
  func _isForeign() -> Bool
}
#endif

func testCases() -> [(String, [String])] {
  var tests = StdlibUnicodeUnittest.wordBreakTests
  if #available(SwiftStdlib 5.10, *) {
    // rdar://116652595
    //
    // We were accidentally hanging when rounding word indices for some
    // concoctions of strings. In particular, where we had a pair of scalars
    // create a constraint for the preceding pair, but the preceding extend
    // rules were not taking the constraint into consideration.
    tests += [
      ("日\u{FE0F}:X ", ["日\u{FE0F}", ":", "X", " "]),
      ("👨‍👨‍👧‍👦\u{FE0F}:X ", ["👨‍👨‍👧‍👦\u{FE0F}", ":", "X", " "]),
      ("⛔️:X ", ["⛔️", ":", "X", " "]),
      ("⛔️·X ", ["⛔️", "·", "X", " "]),
      ("⛔️：X ", ["⛔️", "：", "X", " "]),
    ]
  }
  if #available(SwiftStdlib 6.3, *) {
    tests += [
      // https://github.com/swiftlang/swift-experimental-string-processing/issues/818
      // rdar://154902007
      ("\u{2060}\u{2018}\u{2060}\u{2060}example.com\u{2060}\u{2060}\u{2019}",
       ["\u{2060}", "\u{2018}\u{2060}\u{2060}", "example.com\u{2060}\u{2060}", "\u{2019}"]),
    ]
  }
  return tests
}

if #available(SwiftStdlib 6.1, *) {
  StringWordBreaking.test("word breaking") {
    for (input, expectedWords) in testCases() {
      expectEqual(
        input.statelessWords,
        expectedWords,
        "input: \(input.debugDescription) \(input.scalarDescriptions)")
      if #available(SwiftStdlib 6.3, *) {
        expectEqual(
          input.statefulWords,
          expectedWords,
          "input: \(input.debugDescription) \(input.scalarDescriptions)")
        expectEqual(
          input.backwardWords,
          expectedWords.reversed(),
          "input: \(input.debugDescription) \(input.scalarDescriptions)")
      }
    }
  }
}

if #available(SwiftStdlib 6.1, *) {
  StringWordBreaking.test("word breaking foreign") {
    for (nativeString, expectedWords) in testCases() {
      let input = NonContiguousNSString(nativeString.utf16) as String

      expectTrue(input._guts._isForeign())
      expectEqual(
        input.statelessWords,
        expectedWords,
        "input: \(nativeString.debugDescription) \(nativeString.scalarDescriptions)")
      if #available(SwiftStdlib 6.3, *) {
        expectEqual(
          input.statefulWords,
          expectedWords,
          "input: \(nativeString.debugDescription) \(nativeString.scalarDescriptions)")
        expectEqual(
          input.backwardWords,
          expectedWords.reversed(),
          "input: \(nativeString.debugDescription) \(nativeString.scalarDescriptions)")
      }
    }
  }
}
