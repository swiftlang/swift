// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

//
// Tests for the non-Foundation API of String
//

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
import Foundation
import StdlibUnittestFoundationExtras
#endif

var StringTests = TestSuite("StringTests")

struct ComparisonTest {
  let expectedUnicodeCollation: ExpectedComparisonResult
  let lhs: String
  let rhs: String
  let loc: SourceLoc
  let xfail: TestRunPredicate

  init(
    _ expectedUnicodeCollation: ExpectedComparisonResult,
    _ lhs: String, _ rhs: String,
    xfail: TestRunPredicate = .Custom({false}, reason: ""),
    file: String = #file, line: UInt = #line
  ) {
    self.expectedUnicodeCollation = expectedUnicodeCollation
    self.lhs = lhs
    self.rhs = rhs
    self.loc = SourceLoc(file, line, comment: "test data")
    self.xfail = xfail
  }
}

let comparisonTests = [
  ComparisonTest(.EQ, "", ""),
  ComparisonTest(.LT, "", "a"),

  // ASCII cases
  ComparisonTest(.LT, "t", "tt"),
  ComparisonTest(.GT, "t", "Tt",
    xfail: .NativeRuntime(
      "Compares in reverse with ICU, https://bugs.swift.org/browse/SR-530")),
  ComparisonTest(.GT, "\u{0}", "",
    xfail: .NativeRuntime(
      "Null-related issue: https://bugs.swift.org/browse/SR-630")),
  ComparisonTest(.EQ, "\u{0}", "\u{0}"),
  // Currently fails:
  // ComparisonTest(.LT, "\r\n", "t"),
  // ComparisonTest(.GT, "\r\n", "\n"),
  // ComparisonTest(.LT, "\u{0}", "\u{0}\u{0}"),

  // Whitespace
  // U+000A LINE FEED (LF)
  // U+000B LINE TABULATION
  // U+000C FORM FEED (FF)
  // U+0085 NEXT LINE (NEL)
  // U+2028 LINE SEPARATOR
  // U+2029 PARAGRAPH SEPARATOR
  ComparisonTest(.GT, "\u{0085}", "\n"),
  ComparisonTest(.GT, "\u{000b}", "\n"),
  ComparisonTest(.GT, "\u{000c}", "\n"),
  ComparisonTest(.GT, "\u{2028}", "\n"),
  ComparisonTest(.GT, "\u{2029}", "\n"),
  ComparisonTest(.GT, "\r\n\r\n", "\r\n"),

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  ComparisonTest(.EQ, "a\u{301}", "\u{e1}"),
  ComparisonTest(.LT, "a", "a\u{301}"),
  ComparisonTest(.LT, "a", "\u{e1}"),

  // U+304B HIRAGANA LETTER KA
  // U+304C HIRAGANA LETTER GA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  ComparisonTest(.EQ, "\u{304b}", "\u{304b}"),
  ComparisonTest(.EQ, "\u{304c}", "\u{304c}"),
  ComparisonTest(.LT, "\u{304b}", "\u{304c}"),
  ComparisonTest(.LT, "\u{304b}", "\u{304c}\u{3099}"),
  ComparisonTest(.EQ, "\u{304c}", "\u{304b}\u{3099}"),
  ComparisonTest(.LT, "\u{304c}", "\u{304c}\u{3099}"),

  // U+212B ANGSTROM SIGN
  // U+030A COMBINING RING ABOVE
  // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  ComparisonTest(.EQ, "\u{212b}", "A\u{30a}"),
  ComparisonTest(.EQ, "\u{212b}", "\u{c5}"),
  ComparisonTest(.EQ, "A\u{30a}", "\u{c5}"),
  ComparisonTest(.LT, "A\u{30a}", "a",
    xfail: .NativeRuntime(
      "Compares in reverse with ICU, https://bugs.swift.org/browse/SR-530")),
  ComparisonTest(.LT, "A", "A\u{30a}"),

  // U+2126 OHM SIGN
  // U+03A9 GREEK CAPITAL LETTER OMEGA
  ComparisonTest(.EQ, "\u{2126}", "\u{03a9}"),

  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  // U+1E69 LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
  ComparisonTest(.EQ, "\u{1e69}", "s\u{323}\u{307}"),
  ComparisonTest(.EQ, "\u{1e69}", "s\u{307}\u{323}"),
  ComparisonTest(.EQ, "\u{1e69}", "\u{1e63}\u{307}"),
  ComparisonTest(.EQ, "\u{1e63}", "s\u{323}"),
  ComparisonTest(.EQ, "\u{1e63}\u{307}", "s\u{323}\u{307}"),
  ComparisonTest(.EQ, "\u{1e63}\u{307}", "s\u{307}\u{323}"),
  ComparisonTest(.LT, "s\u{323}", "\u{1e69}"),

  // U+FB01 LATIN SMALL LIGATURE FI
  ComparisonTest(.EQ, "\u{fb01}", "\u{fb01}"),
  ComparisonTest(.LT, "fi", "\u{fb01}"),

  // Test that Unicode collation is performed in deterministic mode.
  //
  // U+0301 COMBINING ACUTE ACCENT
  // U+0341 COMBINING ACUTE TONE MARK
  // U+0954 DEVANAGARI ACUTE ACCENT
  //
  // Collation elements from DUCET:
  // 0301  ; [.0000.0024.0002] # COMBINING ACUTE ACCENT
  // 0341  ; [.0000.0024.0002] # COMBINING ACUTE TONE MARK
  // 0954  ; [.0000.0024.0002] # DEVANAGARI ACUTE ACCENT
  //
  // U+0301 and U+0954 don't decompose in the canonical decomposition mapping.
  // U+0341 has a canonical decomposition mapping of U+0301.
  ComparisonTest(.EQ, "\u{0301}", "\u{0341}"),
  ComparisonTest(.LT, "\u{0301}", "\u{0954}",
    xfail: .NativeRuntime("Compares as equal with ICU")),
  ComparisonTest(.LT, "\u{0341}", "\u{0954}",
    xfail: .NativeRuntime("Compares as equal with ICU")),
]

func checkStringComparison(
  expected: ExpectedComparisonResult,
  _ lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
  // String / String
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expected.isEQ(), lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

  expectEqual(expected.isLT(), lhs < rhs, stackTrace: stackTrace)
  expectEqual(expected.isLE(), lhs <= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGE(), lhs >= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGT(), lhs > rhs, stackTrace: stackTrace)
  checkComparable(expected, lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

#if _runtime(_ObjC)
  // NSString / NSString
  let lhsNSString = lhs as NSString
  let rhsNSString = rhs as NSString
  let expectedEqualUnicodeScalars =
    Array(lhs.unicodeScalars) == Array(rhs.unicodeScalars)
  // FIXME: Swift String and NSString comparison may not be equal.
  expectEqual(
    expectedEqualUnicodeScalars, lhsNSString == rhsNSString,
    stackTrace: stackTrace)
  expectEqual(
    !expectedEqualUnicodeScalars, lhsNSString != rhsNSString,
    stackTrace: stackTrace)
  checkHashable(
    expectedEqualUnicodeScalars, lhsNSString, rhsNSString,
    stackTrace: stackTrace.withCurrentLoc())
#endif
}

for test in comparisonTests {
  StringTests.test("String.{Equatable,Hashable,Comparable}: line \(test.loc.line)")
  .xfail(test.xfail)
  .code {
    checkStringComparison(
      test.expectedUnicodeCollation, test.lhs, test.rhs,
      test.loc.withCurrentLoc())
    checkStringComparison(
      test.expectedUnicodeCollation.flip(), test.rhs, test.lhs,
      test.loc.withCurrentLoc())
  }
}

func checkCharacterComparison(
  expected: ExpectedComparisonResult,
  _ lhs: Character, _ rhs: Character, _ stackTrace: SourceLocStack
) {
  // Character / Character
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expected.isEQ(), lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

  expectEqual(expected.isLT(), lhs < rhs, stackTrace: stackTrace)
  expectEqual(expected.isLE(), lhs <= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGE(), lhs >= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGT(), lhs > rhs, stackTrace: stackTrace)
  checkComparable(expected, lhs, rhs, stackTrace: stackTrace.withCurrentLoc())
}

for test in comparisonTests {
  if test.lhs.characters.count == 1 && test.rhs.characters.count == 1 {
    StringTests.test("Character.{Equatable,Hashable,Comparable}: line \(test.loc.line)")
    .xfail(test.xfail)
    .code {
      let lhsCharacter = Character(test.lhs)
      let rhsCharacter = Character(test.rhs)
      checkCharacterComparison(
        test.expectedUnicodeCollation, lhsCharacter, rhsCharacter,
        test.loc.withCurrentLoc())
      checkCharacterComparison(
        test.expectedUnicodeCollation.flip(), rhsCharacter, lhsCharacter,
        test.loc.withCurrentLoc())
    }
  }
}

func checkHasPrefixHasSuffix(
  lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
#if _runtime(_ObjC)
  if lhs == "" {
    return
  }
  if rhs == "" {
    expectFalse(lhs.hasPrefix(rhs), stackTrace: stackTrace)
    expectFalse(lhs.hasSuffix(rhs), stackTrace: stackTrace)
    return
  }

  // To determine the expected results, compare grapheme clusters,
  // scalar-to-scalar, of the NFD form of the strings.
  let lhsNFDGraphemeClusters =
    lhs.decomposedStringWithCanonicalMapping.characters.map {
      Array(String($0).unicodeScalars)
    }
  let rhsNFDGraphemeClusters =
    rhs.decomposedStringWithCanonicalMapping.characters.map {
      Array(String($0).unicodeScalars)
    }
  let expectHasPrefix = lhsNFDGraphemeClusters.startsWith(
    rhsNFDGraphemeClusters, isEquivalent: (==))
  let expectHasSuffix =
    lhsNFDGraphemeClusters.lazy.reverse().startsWith(
      rhsNFDGraphemeClusters.lazy.reverse(), isEquivalent: (==))

  expectEqual(expectHasPrefix, lhs.hasPrefix(rhs), stackTrace: stackTrace)
  expectEqual(
    expectHasPrefix, (lhs + "abc").hasPrefix(rhs), stackTrace: stackTrace)
  expectEqual(expectHasSuffix, lhs.hasSuffix(rhs), stackTrace: stackTrace)
  expectEqual(
    expectHasSuffix, ("abc" + lhs).hasSuffix(rhs), stackTrace: stackTrace)
#endif
}

StringTests.test("hasPrefix,hasSuffix")
  .skip(.NativeRuntime(
    "String.has{Prefix,Suffix} defined when _runtime(_ObjC)"))
  .code {
  for test in comparisonTests {
    checkHasPrefixHasSuffix(test.lhs, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(test.rhs, test.lhs, test.loc.withCurrentLoc())
  }
}

StringTests.test("Failures{hasPrefix,hasSuffix}-CF")
  .xfail(.Custom({ true }, reason: "rdar://problem/19034601"))
  .skip(.NativeRuntime(
    "String.has{Prefix,Suffix} defined when _runtime(_ObjC)"))
  .code {
  let test = ComparisonTest(.LT, "\u{0}", "\u{0}\u{0}")
  checkHasPrefixHasSuffix(test.lhs, test.rhs, test.loc.withCurrentLoc())
}

StringTests.test("Failures{hasPrefix,hasSuffix}")
  .xfail(.Custom({ true }, reason: "blocked on rdar://problem/19036555"))
  .skip(.NativeRuntime(
    "String.has{Prefix,Suffix} defined when _runtime(_ObjC)"))
  .code {
  let tests =
    [ComparisonTest(.LT, "\r\n", "t"), ComparisonTest(.GT, "\r\n", "\n")]
  tests.forEach {
    checkHasPrefixHasSuffix($0.lhs, $0.rhs, $0.loc.withCurrentLoc())
  }
}

StringTests.test("SameTypeComparisons") {
  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  let xs = "\u{1e69}"
  expectTrue(xs == "s\u{323}\u{307}")
  expectFalse(xs != "s\u{323}\u{307}")
  expectTrue("s\u{323}\u{307}" == xs)
  expectFalse("s\u{323}\u{307}" != xs)
  expectTrue("\u{1e69}" == "s\u{323}\u{307}")
  expectFalse("\u{1e69}" != "s\u{323}\u{307}")
  expectTrue(xs == xs)
  expectFalse(xs != xs)
}

StringTests.test("CompareStringsWithUnpairedSurrogates")
  .xfail(
    .Custom({ true },
    reason: "<rdar://problem/18029104> Strings referring to underlying " +
      "storage with unpaired surrogates compare unequal"))
  .code {
  let donor = "abcdef"
  let acceptor = "\u{1f601}\u{1f602}\u{1f603}"

  expectEqual("\u{fffd}\u{1f602}\u{fffd}",
    acceptor[donor.startIndex.advancedBy(1)..<donor.startIndex.advancedBy(5)])
}

var CStringTests = TestSuite("CStringTests")

func getNullCString() -> UnsafeMutablePointer<CChar> {
  return nil
}

func getASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<CChar>.alloc(100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (up, { up.dealloc(100) })
}

func getNonASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String1(
) -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x80
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String2(
) -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x81
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func asCCharArray(a: [UInt8]) -> [CChar] {
  return a.map { CChar(bitPattern: $0) }
}

CStringTests.test("String.fromCString") {
  do {
    let s = getNullCString()
    expectEmpty(String.fromCString(s))
  }
  do {
    let (s, dealloc) = getASCIICString()
    expectOptionalEqual("ab", String.fromCString(s))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIICString()
    expectOptionalEqual("аб", String.fromCString(s))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    expectEmpty(String.fromCString(s))
    dealloc()
  }
}

CStringTests.test("String.fromCStringRepairingIllFormedUTF8") {
  do {
    let s = getNullCString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectEmpty(result)
    expectFalse(hadError)
  }
  do {
    let (s, dealloc) = getASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("ab", result)
    expectFalse(hadError)
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("аб", result)
    expectFalse(hadError)
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
    expectTrue(hadError)
    dealloc()
  }
}

runAllTests()

