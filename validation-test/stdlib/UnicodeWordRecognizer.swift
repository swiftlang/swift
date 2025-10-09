// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: optimized_stdlib

// Validate that the various forms of word breaking all lead to consistent
// results by exhaustively enumerating all possible state machine inputs up to
// an adequately high length.
//
// The word breaking algorithm only cares about word break properties, not
// specific scalar values. This lets us only use a single representative sample
// in each class, drastically cutting down the input space to iterate through.
// This makes it practical to do this up to limits that give us practically
// useful results.

import StdlibUnittest

let suite = TestSuite("UnicodeWordRecognizer")
defer { runAllTests() }

// One representative sample from each character class that's relevant to word breaking
let samples: [Unicode.Scalar] = [
  "\u{000D}", // CR
  "\u{000A}", // LF
  "\u{2028}", // Newline (LINE SEPARATOR)
  "\u{0041}", // ALetter (LATIN CAPITAL LETTER A)
  "\u{0022}", // Double_Quote (QUOTATION MARK)
  "\u{0027}", // Single_Quote (APOSTROPHE)
  "\u{200D}", // ZWJ (ZERO WIDTH JOINER)
  "\u{1F1E6}", // RI (REGIONAL INDICATOR SYMBOL LETTER A)
  "\u{05D0}", // Hebrew_Letter (HEBREW LETTER ALEF)
  "\u{0300}", // Extend (COMBINING GRAVE ACCENT)
  "\u{00AD}", // Format (SOFT HYPHEN)
  "\u{3031}", // Katakana (VERTICAL KANA REPEAT MARK)
  "\u{003A}", // MidLetter (COLON)
  "\u{002C}", // MidNum (COMMA)
  "\u{002E}", // MidNumLet (FULL STOP)
  "\u{0030}", // Numeric (DIGIT ZERO)
  "\u{005F}", // ExtendNumLet (LOW LINE)
  "\u{0020}", // WSegSpace (SPACE)
  "\u{00A9}", // \p{Extended_Pictographic} (COPYRIGHT)
  "\u{0021}", // Any (EXCLAMATION MARK)
]

/// Call `body` with every array of the specified count consisting of
/// integer elements in the given range. This is calculating the
/// Cartesian `n`-ary power of the `range` argument.
///
///     withEveryArray(of: 1 ..< 3, count: 3) { print($0) }
///     // [1, 1, 1]
///     // [2, 1, 1]
///     // [1, 2, 1]
///     // [2, 2, 1]
///     // [1, 1, 2]
///     // [2, 1, 2]
///     // [1, 2, 2]
///     // [2, 2, 2]
func withEveryArray<E: Error>(
  of range: Range<Int>,
  count n: Int,
  _ body: ([Int]) throws(E) -> Void
) throws(E) {
  var vector: [Int] = .init(repeating: range.lowerBound, count: n)
  guard n > 0 else {
    try body(vector)
    return
  }
  while true {
    try body(vector)
    var i = 0
    while true {
      vector[i] += 1
      if vector[i] < range.upperBound {
        break
      }
      vector[i] = range.lowerBound
      i += 1
      if i == n {
        return // done
      }
    }
  }
}

func string(for vector: [Int]) -> String {
  var s = ""
  for digit in vector {
    s.unicodeScalars.append(samples[digit])
  }
  return s
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

extension Collection {
  /// Return a sorted array of all valid indices in the collection, including
  /// the end index.
  func allIndices() -> [Index] {
    var result: [Index] = []
    result.reserveCapacity(count + 1)
    result.append(contentsOf: indices)
    result.append(endIndex)
    return result
  }
}

extension Sequence where Element: Equatable {
  /// Returns true if the elements of `self` form a sub-sequence of `other`,
  /// where both inputs are monotonic. `self` is not allowed to contain
  /// more than a single copy of any item in `other`.
  func isMonotonicSubsequence(of other: some Sequence<Element>) -> Bool {
    var i = makeIterator()
    var j = other.makeIterator()
    var b = j.next()
    while let a = i.next() {
      while true {
        if b == nil { return false }
        if a == b {
          b = j.next()
          break
        }
        b = j.next()
      }
    }
    return true
  }

  /// Returns true if the elements of `self` form a sub-sequence of `other`,
  /// where both inputs are monotonic. `self` is allowed to contain duplicate
  /// elements.
  func isMonotonicRepeatingSubsequence(of other: some Sequence<Element>) -> Bool {
    var i = makeIterator()
    var j = other.makeIterator()
    var b = j.next()
    while let a = i.next() {
      while true {
        if b == nil { return false }
        if a == b { break }
        b = j.next()
      }
    }
    return true
  }

}

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

  /// Returns all word breaks without keeping persistent state, using
  /// `_wordIndex(after:)`. This forgets lookahead information after each word
  /// boundary, so it needs to process some scalars twice, resulting in a
  /// performance regression vs `fastWordBreaks()`. However, both variants are
  /// supposed to have the same results.
  @available(StdlibDeploymentTarget 5.7, *)
  func slowWordBreaks() -> [String.Index] {
    var result: [String.Index] = []
    var i = self.startIndex
    while i < self.endIndex {
      result.append(i)
      i = self._wordIndex(after: i)
    }
    result.append(i)
    return result
  }

  /// Return all "safe" word breaks in this string by using the backwards word
  /// recognizer state machine, starting from the end, feeding it every Unicode
  /// scalar in the string, and collecting all word boundaries detected.
  ///
  /// This is expected to sometimes skip over word boundaries that are detected
  /// when going forward. However, it must never report a word boundary at a
  /// position that isn't also detected by the forward recognizer.
  @available(StdlibDeploymentTarget 6.3, *)
  func safeWordBreaks() -> [String.Index] {
    var result: [String.Index] = []
    guard !self.isEmpty else { return result }
    result.append(self.endIndex) // There is always an implicit wordbreak at the end.
    var i = self.unicodeScalars.index(before: self.endIndex)
    var recognizer = Unicode._RandomAccessWordRecognizer(before: self.unicodeScalars[i])
    var candidate = i
    while i > self.startIndex {
      let j = self.unicodeScalars.index(before: i)
      let r = recognizer.hasGuaranteedBreak(after: self.unicodeScalars[j])
      if r.setCandidate {
        candidate = i
      }
      if r.breakAtCandidate {
        result.append(candidate)
      }
      if r.breakHere {
        result.append(i)
      }
      i = j
    }
    result.reverse()
    return result
  }

  /// Return an array of "safe" word boundaries detected by the backwards word
  /// recognizer state machine, invoked through
  /// `_wordIndex(somewhereAtOrBefore:)`, one result per each scalar position in
  /// the string (including its end index).
  ///
  /// This is expected to be some monotonically increasing subsequence of word
  /// boundaries detected in the forward direction, allowing some repeated
  /// items.
  @available(StdlibDeploymentTarget 6.3, *)
  func randomAccessWordBreaks() -> [String.Index] {
    unicodeScalars.allIndices().map { self._wordIndex(somewhereAtOrBefore: $0) }
  }
}

@available(StdlibDeploymentTarget 6.3, *)
func check(length: Int) {
  withEveryArray(of: 0 ..< samples.count, count: length) { vector in
    let str = string(for: vector)

    let fastBreaks = str.fastWordBreaks()
    let slowBreaks = str.slowWordBreaks()
    expectEqual(
      fastBreaks, slowBreaks,
      """
      Inconsistent word boundaries in stateful vs stateless iteration:
        input: \(str.debugDescription) (\(str.scalarDescriptions))
      """)

    let safeBreaks = str.safeWordBreaks()
    expectTrue(
      safeBreaks.isMonotonicSubsequence(of: fastBreaks),
      """
      Inconsistent safe word boundaries:
        input: \(str.debugDescription) (\(str.scalarDescriptions))")
      """)

    let randomAccessBreaks = str.randomAccessWordBreaks()
    expectTrue(
      randomAccessBreaks.isMonotonicRepeatingSubsequence(of: fastBreaks),
      """
      Inconsistent random-access word boundaries:
        input: \(str.debugDescription) (\(str.scalarDescriptions))
        breaks:               \(fastBreaks)
        random-access breaks: \(randomAccessBreaks)
      """)
  }
}

if #available(StdlibDeploymentTarget 6.3, *) {
  suite.test("Exhaustive consistency checks, length 1") {
    check(length: 1)
  }

  suite.test("Exhaustive consistency checks, length 2") {
    check(length: 2)
  }

  suite.test("Exhaustive consistency checks, length 3") {
    check(length: 3)
  }

  suite.test("Exhaustive consistency checks, length 4") {
    check(length: 4)
  }

  suite.test("Exhaustive consistency checks, length 5") {
    check(length: 5)
  }
}
