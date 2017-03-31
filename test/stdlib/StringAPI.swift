// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for the non-Foundation API of String
//

import StdlibUnittest

#if _runtime(_ObjC)
import Foundation
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
    xfail: TestRunPredicate = .never,
    file: String = #file, line: UInt = #line
  ) {
    self.expectedUnicodeCollation = expectedUnicodeCollation
    self.lhs = lhs
    self.rhs = rhs
    self.loc = SourceLoc(file, line, comment: "test data")
    self.xfail = xfail
  }

  func replacingPredicate(_ xfail: TestRunPredicate) -> ComparisonTest {
    return ComparisonTest(expectedUnicodeCollation, lhs, rhs,
      xfail: xfail, file: loc.file, line: loc.line)
  }
}

// List test cases for comparisons and prefix/suffix. Ideally none fail.

let tests = [
  ComparisonTest(.eq, "", ""),
  ComparisonTest(.lt, "", "a"),

  // ASCII cases
  ComparisonTest(.lt, "t", "tt"),
  ComparisonTest(.gt, "t", "Tt"),
  ComparisonTest(.gt, "\u{0}", ""),
  ComparisonTest(.eq, "\u{0}", "\u{0}"),

  ComparisonTest(.lt, "\r\n", "t"),
  ComparisonTest(.gt, "\r\n", "\n"),
  ComparisonTest(.lt, "\u{0}", "\u{0}\u{0}"),

  // Whitespace
  // U+000A LINE FEED (LF)
  // U+000B LINE TABULATION
  // U+000C FORM FEED (FF)
  // U+0085 NEXT LINE (NEL)
  // U+2028 LINE SEPARATOR
  // U+2029 PARAGRAPH SEPARATOR
  ComparisonTest(.gt, "\u{0085}", "\n"),
  ComparisonTest(.gt, "\u{000b}", "\n"),
  ComparisonTest(.gt, "\u{000c}", "\n"),
  ComparisonTest(.gt, "\u{2028}", "\n"),
  ComparisonTest(.gt, "\u{2029}", "\n"),
  ComparisonTest(.gt, "\r\n\r\n", "\r\n"),

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  ComparisonTest(.eq, "a\u{301}", "\u{e1}"),
  ComparisonTest(.lt, "a", "a\u{301}"),
  ComparisonTest(.lt, "a", "\u{e1}"),

  // U+304B HIRAGANA LETTER KA
  // U+304C HIRAGANA LETTER GA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  ComparisonTest(.eq, "\u{304b}", "\u{304b}"),
  ComparisonTest(.eq, "\u{304c}", "\u{304c}"),
  ComparisonTest(.lt, "\u{304b}", "\u{304c}"),
  ComparisonTest(.lt, "\u{304b}", "\u{304c}\u{3099}"),
  ComparisonTest(.eq, "\u{304c}", "\u{304b}\u{3099}"),
  ComparisonTest(.lt, "\u{304c}", "\u{304c}\u{3099}"),

  // U+212B ANGSTROM SIGN
  // U+030A COMBINING RING ABOVE
  // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  ComparisonTest(.eq, "\u{212b}", "A\u{30a}"),
  ComparisonTest(.eq, "\u{212b}", "\u{c5}"),
  ComparisonTest(.eq, "A\u{30a}", "\u{c5}"),
  ComparisonTest(.lt, "A\u{30a}", "a"),
  ComparisonTest(.lt, "A", "A\u{30a}"),

  // U+2126 OHM SIGN
  // U+03A9 GREEK CAPITAL LETTER OMEGA
  ComparisonTest(.eq, "\u{2126}", "\u{03a9}"),

  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  // U+1E69 LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
  ComparisonTest(.eq, "\u{1e69}", "s\u{323}\u{307}"),
  ComparisonTest(.eq, "\u{1e69}", "s\u{307}\u{323}"),
  ComparisonTest(.eq, "\u{1e69}", "\u{1e63}\u{307}"),
  ComparisonTest(.eq, "\u{1e63}", "s\u{323}"),
  ComparisonTest(.eq, "\u{1e63}\u{307}", "s\u{323}\u{307}"),
  ComparisonTest(.eq, "\u{1e63}\u{307}", "s\u{307}\u{323}"),
  ComparisonTest(.lt, "s\u{323}", "\u{1e69}"),

  // U+FB01 LATIN SMALL LIGATURE FI
  ComparisonTest(.eq, "\u{fb01}", "\u{fb01}"),
  ComparisonTest(.lt, "fi", "\u{fb01}"),

  // U+1F1E7 REGIONAL INDICATOR SYMBOL LETTER B
  // \u{1F1E7}\u{1F1E7} Flag of Barbados
  ComparisonTest(.lt, "\u{1F1E7}", "\u{1F1E7}\u{1F1E7}"),

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
  ComparisonTest(.eq, "\u{0301}", "\u{0341}"),
  ComparisonTest(.lt, "\u{0301}", "\u{0954}"),
  ComparisonTest(.lt, "\u{0341}", "\u{0954}"),
]

func checkStringComparison(
  _ expected: ExpectedComparisonResult,
  _ lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
  // String / String
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expectedEqual: expected.isEQ(),
    lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

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
    expectedEqual: expectedEqualUnicodeScalars,
    lhsNSString, rhsNSString,
    stackTrace: stackTrace.withCurrentLoc())
#endif
}

// Mark the test cases that are expected to fail in checkStringComparison

let comparisonTests = tests.map {
  (test: ComparisonTest) -> ComparisonTest in
  switch (test.expectedUnicodeCollation, test.lhs, test.rhs) {
  case (.gt, "t", "Tt"), (.lt, "A\u{30a}", "a"):
    return test.replacingPredicate(.nativeRuntime(
      "Comparison reversed between ICU and CFString, https://bugs.swift.org/browse/SR-530"))

  case (.gt, "\u{0}", ""), (.lt, "\u{0}", "\u{0}\u{0}"):
    return test.replacingPredicate(.nativeRuntime(
      "Null-related issue: https://bugs.swift.org/browse/SR-630"))

  case (.lt, "\u{0301}", "\u{0954}"), (.lt, "\u{0341}", "\u{0954}"):
    return test.replacingPredicate(.nativeRuntime(
      "Compares as equal with ICU"))

  default:
    return test
  }
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
  _ expected: ExpectedComparisonResult,
  _ lhs: Character, _ rhs: Character, _ stackTrace: SourceLocStack
) {
  // Character / Character
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expectedEqual: expected.isEQ(),
    lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

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
  _ lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
#if _runtime(_ObjC)
  if rhs == "" {
    expectTrue(lhs.hasPrefix(rhs), stackTrace: stackTrace)
    expectTrue(lhs.hasSuffix(rhs), stackTrace: stackTrace)
    return
  }
  if lhs == "" {
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
  let expectHasPrefix = lhsNFDGraphemeClusters.starts(
    with: rhsNFDGraphemeClusters, by: (==))

  let expectHasSuffix = lhsNFDGraphemeClusters.lazy.reversed()
    .starts(with: rhsNFDGraphemeClusters.lazy.reversed(), by: (==))

  expectEqual(expectHasPrefix, lhs.hasPrefix(rhs), stackTrace: stackTrace)
  expectEqual(expectHasSuffix, lhs.hasSuffix(rhs), stackTrace: stackTrace)
#endif
}

StringTests.test("LosslessStringConvertible") {
  checkLosslessStringConvertible(comparisonTests.map { $0.lhs })
  checkLosslessStringConvertible(comparisonTests.map { $0.rhs })
}

// Mark the test cases that are expected to fail in checkHasPrefixHasSuffix

let substringTests = tests.map {
  (test: ComparisonTest) -> ComparisonTest in
  switch (test.expectedUnicodeCollation, test.lhs, test.rhs) {
  case (.eq, "\u{0}", "\u{0}"):
    return test.replacingPredicate(.objCRuntime(
      "https://bugs.swift.org/browse/SR-332"))

  case (.gt, "\r\n", "\n"):
    return test.replacingPredicate(.objCRuntime(
      "blocked on rdar://problem/19036555"))

  case (.eq, "\u{0301}", "\u{0341}"):
    return test.replacingPredicate(.objCRuntime(
      "https://bugs.swift.org/browse/SR-243"))

  case (.lt, "\u{1F1E7}", "\u{1F1E7}\u{1F1E7}"):
    return test.replacingPredicate(.objCRuntime(
      "https://bugs.swift.org/browse/SR-367"))

  default:
    return test
  }
}

for test in substringTests {
  StringTests.test("hasPrefix,hasSuffix: line \(test.loc.line)")
    .skip(.nativeRuntime(
        "String.has{Prefix,Suffix} defined when _runtime(_ObjC)"))
    .xfail(test.xfail)
    .code {
    checkHasPrefixHasSuffix(test.lhs, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(test.rhs, test.lhs, test.loc.withCurrentLoc())

    let fragment = "abc"
    let combiner = "\u{0301}" // combining acute accent

    checkHasPrefixHasSuffix(test.lhs + fragment, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(fragment + test.lhs, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(test.lhs + combiner, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(combiner + test.lhs, test.rhs, test.loc.withCurrentLoc())
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
    .always("<rdar://problem/18029104> Strings referring to underlying " +
      "storage with unpaired surrogates compare unequal"))
  .code {
  let donor = "abcdef"
  let acceptor = "\u{1f601}\u{1f602}\u{1f603}"

  expectEqual("\u{fffd}\u{1f602}\u{fffd}",
    String(
      acceptor[
        donor.index(donor.startIndex, offsetBy: 1) ..<
        donor.index(donor.startIndex, offsetBy: 5)
      ]
    )
  )
}

var CStringTests = TestSuite("CStringTests")

func getNullUTF8() -> UnsafeMutablePointer<UInt8>? {
  return nil
}

func getASCIIUTF8() -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (up, { up.deallocate(capacity: 100) })
}

func getNonASCIIUTF8() -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (UnsafeMutablePointer(up), { up.deallocate(capacity: 100) })
}

func getIllFormedUTF8String1(
) -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x80
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.deallocate(capacity: 100) })
}

func getIllFormedUTF8String2(
) -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x41
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x81
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.deallocate(capacity: 100) })
}

func asCCharArray(_ a: [UInt8]) -> [CChar] {
  return a.map { CChar(bitPattern: $0) }
}

func getUTF8Length(_ cString: UnsafePointer<UInt8>) -> Int {
  var length = 0
  while cString[length] != 0 {
    length += 1
  }
  return length
}

func bindAsCChar(_ utf8: UnsafePointer<UInt8>) -> UnsafePointer<CChar> {
  return UnsafeRawPointer(utf8).bindMemory(to: CChar.self,
    capacity: getUTF8Length(utf8))
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: UnsafePointer<UInt8>) {

  var index = 0
  while lhs[index] != 0 {
    expectEqual(lhs[index], rhs[index])
    index += 1
  }
  expectEqual(0, rhs[index])
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: ContiguousArray<UInt8>) {
  rhs.withUnsafeBufferPointer {
    expectEqualCString(lhs, $0.baseAddress)
  }
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: ContiguousArray<CChar>) {
  rhs.withUnsafeBufferPointer {
    $0.baseAddress.withMemoryRebound(
      to: UInt8.self, capacity: rhs.count) {
      expectEqualCString(lhs, $0)
    }
  }
}

CStringTests.test("String.init(validatingUTF8:)") {
  do {
    let (s, dealloc) = getASCIIUTF8()
    expectOptionalEqual("ab", String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIIUTF8()
    expectOptionalEqual("аб", String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    expectNil(String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
}

CStringTests.test("String(cString:)") {
  do {
    let (s, dealloc) = getASCIIUTF8()
    let result = String(cString: s)
    expectEqual("ab", result)
    let su = bindAsCChar(s)
    expectEqual("ab", String(cString: su))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIIUTF8()
    let result = String(cString: s)
    expectEqual("аб", result)
    let su = bindAsCChar(s)
    expectEqual("аб", String(cString: su))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    let result = String(cString: s)
    expectEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
    let su = bindAsCChar(s)
    expectEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", String(cString: su))
    dealloc()
  }
}

CStringTests.test("String.decodeCString") {
  do {
    let s = getNullUTF8()
    let result = String.decodeCString(s, as: UTF8.self)
    expectNil(result)
  }
  do { // repairing
    let (s, dealloc) = getIllFormedUTF8String1()
    if let (result, repairsMade) = String.decodeCString(
      s, as: UTF8.self, repairingInvalidCodeUnits: true) {
      expectOptionalEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
      expectTrue(repairsMade)
    } else {
      expectUnreachable("Expected .some()")
    }
    dealloc()
  }
  do { // non repairing
    let (s, dealloc) = getIllFormedUTF8String1()
    let result = String.decodeCString(
      s, as: UTF8.self, repairingInvalidCodeUnits: false)
    expectNil(result)
    dealloc()
  }
}

CStringTests.test("String.utf8CString") {
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    let str = String(cString: cstr)
    expectEqualCString(cstr, str.utf8CString)
    dealloc()
  }
  do {
    let (cstr, dealloc) = getNonASCIIUTF8()
    let str = String(cString: cstr)
    expectEqualCString(cstr, str.utf8CString)
    dealloc()
  }
}

runAllTests()

