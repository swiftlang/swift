// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-run-stdlib-swift

// XFAIL: interpret

import StdlibUnittest
import Swift
import SwiftPrivate

//===---
// Utilities.
//===---

// Single Unicode scalars that occupy a variety of bits in UTF-8.
//
// These scalars should be "base characters" with regards to their position in
// a grapheme cluster.
let baseScalars = [
  // U+0065 LATIN SMALL LETTER E
  "\u{0065}",

  // U+006F LATIN SMALL LETTER O
  "\u{006f}",

  // U+00A9 COPYRIGHT SIGN
  "\u{00a9}",

  // U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
  "\u{0122}",

  // U+0521 CYRILLIC SMALL LETTER EL WITH MIDDLE HOOK
  "\u{0521}",

  // U+0ED2 LAO DIGIT TWO
  "\u{0ed2}",

  // U+4587 CJK UNIFIED IDEOGRAPH-4587
  "\u{4587}",

  // U+B977 HANGUL SYLLABLE REUGS
  "\u{b977}",

  // U+BF01 HANGUL SYLLABLE BBENG
  "\u{bf01}",

  // U+1D452 MATHEMATICAL ITALIC SMALL E
  "\u{1d452}",

  // U+1E825 MENDE KIKAKUI SYLLABLE M163 EE
  "\u{1e825}",

  // U+10B9C4 (private use)
  "\u{10b9c4}",
]

// Single Unicode scalars that are "continuing characters" with regards to
// their position in a grapheme cluster.
let continuingScalars = [
  // U+0300 COMBINING GRAVE ACCENT
  "\u{0300}",

  // U+0308 COMBINING DIAERESIS
  "\u{0308}",

  // U+0903 DEVANAGARI SIGN VISARGA
  "\u{0903}",

  // U+200D ZERO WIDTH JOINER
  "\u{200D}",
]

let testCharacters = [
  // U+000D CARRIAGE RETURN (CR)
  // U+000A LINE FEED (LF)
  "\u{000d}\u{000a}",

  // Grapheme clusters that have UTF-8 representations of length 1..10 bytes.

  // U+0061 LATIN SMALL LETTER A
  // U+0300 COMBINING GRAVE ACCENT
  "\u{0061}", // UTF-8: 1 byte
  "\u{0061}\u{0300}", // UTF-8: 3 bytes
  "\u{0061}\u{0300}\u{0300}", // UTF-8: 5 bytes
  "\u{0061}\u{0300}\u{0300}\u{0300}", // UTF-8: 7 bytes
  "\u{0061}\u{0300}\u{0300}\u{0300}\u{0300}", // UTF-8: 9 bytes

  // U+00A9 COPYRIGHT SIGN
  // U+0300 COMBINING GRAVE ACCENT
  "\u{00a9}", // UTF-8: 2 bytes
  "\u{00a9}\u{0300}", // UTF-8: 4 bytes
  "\u{00a9}\u{0300}\u{0300}", // UTF-8: 6 bytes
  "\u{00a9}\u{0300}\u{0300}\u{0300}", // UTF-8: 8 bytes
  "\u{00a9}\u{0300}\u{0300}\u{0300}\u{0300}", // UTF-8: 10 bytes
]

func randomGraphemeCluster(minSize: Int, _ maxSize: Int) -> String {
  let n = pickRandom((minSize + 1)..<maxSize)
  var result = pickRandom(baseScalars)
  for _ in 0..<n {
    result += pickRandom(continuingScalars)
  }
  return result
}

//===---
// Tests.
//===---

var CharacterTests = TestSuite("Character")

CharacterTests.test("literal") {
  if true {
    // U+0041 LATIN CAPITAL LETTER A
    let ch: Character = "A"
    expectEqual("\u{0041}", String(ch))
  }

  if true {
    // U+3042 HIRAGANA LETTER A
    let ch: Character = "あ"
    expectEqual("\u{3042}", String(ch))
  }

  if true {
    // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
    let ch: Character = "例"
    expectEqual("\u{4F8B}", String(ch))
  }

  if true {
    // U+304B HIRAGANA LETTER KA
    // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
    let ch: Character = "\u{304b}\u{3099}"
    expectEqual("\u{304b}\u{3099}", String(ch))
  }
}

CharacterTests.test("sizeof") {
  // FIXME: should be 8.
  // <rdar://problem/16754935> sizeof(Character.self) is 9, should be 8

  let size1 = sizeof(Character.self)
  expectTrue(size1 == 8 || size1 == 9)

  var a: Character = "a"
  let size2 = sizeofValue(a)
  expectTrue(size2 == 8 || size2 == 9)

  expectEqual(size1, size2)
}

CharacterTests.test("Hashable") {
  for characters in [
    baseScalars,
    continuingScalars,
    testCharacters
  ] {
    for i in characters.indices {
      for j in characters.indices {
        var ci = Character(characters[i])
        var cj = Character(characters[j])
        checkHashable(i == j, ci, cj, SourceLocStack().withCurrentLoc()) {
          "i=\(i), j=\(j)"
        }
      }
    }
  }
}

/// Test that a given `String` can be transformed into a `Character` and back
/// without loss of information.
func checkRoundTripThroughCharacter(s: String) {
  let c = Character(s)
  var s2 = String(c)
  expectEqual(Array(s.unicodeScalars), Array(s2.unicodeScalars)) {
    "round-tripping error: \"\(s)\" != \"\(s2)\""
  }
}

func isSmallRepresentation(s: String) -> Bool {
  switch(Character(s)._representation) {
    case .Small:
      return true
    default:
      return false
  }
}

func checkRepresentation(s: String) {
  let expectSmall = s.utf8.count <= 8
  let isSmall = isSmallRepresentation(s)

  expectEqual(expectSmall, isSmall) {
    let expectedSize = expectSmall ? "small" : "large"
    return "expected \"\(s)\" to use the \(expectedSize) representation"
  }
}

CharacterTests.test("RoundTripping") {
  // Single Unicode Scalar Value tests
  for s in baseScalars {
    checkRepresentation(s)
    checkRoundTripThroughCharacter(s)
  }

  // Edge case tests
  for s in testCharacters {
    checkRepresentation(s)
    checkRoundTripThroughCharacter(s)
  }
}

CharacterTests.test("RoundTripping/Random") {
  // Random tests
  for x in 0..<500 {
    // Character's small representation variant has 63 bits. Making
    // the maximum length 9 scalars tests both sides of the limit.
    var s = randomGraphemeCluster(1, 9)
    checkRepresentation(s)
    checkRoundTripThroughCharacter(s)
  }
}

CharacterTests.test("forall x: ASCII . String(Character(x)) == String(x)") {
  // For all ASCII chars, constructing a Character then a String should be the
  // same as constructing a String directly.
  let asciiDomain = Array(0..<128).map({ UnicodeScalar(Int($0)) })
  expectEqualFunctionsForDomain(asciiDomain,
    { String($0) },
    { String(Character($0)) })
}

CharacterTests.test(
  "forall x: (0..ASCII_MAX-1) . String(x) < String(y+1) == String(Character(x)) < String(Character(y+1))") {
  // For all ASCII chars, constructing a Character then a String should ordered
  // the same as constructing a String directly.
  let asciiDomain = Array(0..<127)
  let ascii0to126 = asciiDomain.map({ UnicodeScalar(Int($0)) })
  let ascii1to127 = asciiDomain.map({ UnicodeScalar(Int($0 + 1)) })
  typealias PredicateFn = (UnicodeScalar) -> (UnicodeScalar) -> Bool
  expectEqualMethodsForDomain(
    ascii0to126,
    ascii1to127,
    { x in { String(x) < String($0) } } as PredicateFn,
    { x in { String(Character(x)) < String(Character($0)) } } as PredicateFn)
}

var UnicodeScalarTests = TestSuite("UnicodeScalar")

UnicodeScalarTests.test("UInt8(ascii: UnicodeScalar)") {
  for i in 0..<0x7f {
    let us = UnicodeScalar(i)
    expectEqual(UInt8(i), UInt8(ascii: us))
  }
}

UnicodeScalarTests.test("UInt8(ascii: UnicodeScalar)/non-ASCII should trap")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let us: UnicodeScalar = "\u{E5}"
  expectCrashLater()
  _blackHole(UInt8(ascii: us))
}

UnicodeScalarTests.test("UInt32(_: UnicodeScalar),UInt64(_: UnicodeScalar)") {
  for us in baseScalars.map({ $0.unicodeScalars.first! }) {
    expectEqual(us.value, UInt32(us))
    expectEqual(UInt64(us.value), UInt64(us))
  }
}

UnicodeScalarTests.test("isASCII()") {
  expectTrue(UnicodeScalar(0).isASCII())
  expectTrue(("A" as UnicodeScalar).isASCII())
  expectTrue(UnicodeScalar(127).isASCII())
  expectFalse(UnicodeScalar(128).isASCII())
  expectFalse(UnicodeScalar(256).isASCII())
}

UnicodeScalarTests.test("Comparable") {
  // FIXME: these tests are insufficient.

  var CharA: UnicodeScalar = "A"

  expectTrue(CharA == "A")
  expectTrue("A" == CharA)
  expectTrue(CharA != "B")
  expectTrue("B" != CharA)

  expectTrue(CharA < "B")
  expectTrue("B" > CharA)
  expectTrue(CharA <= "B")
  expectTrue("B" >= CharA)
}

runAllTests()

