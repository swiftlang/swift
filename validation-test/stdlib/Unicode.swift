// RUN: %target-run-simple-swift

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import SwiftPrivate
import StdlibUnittest
import Foundation

var UTF16APIs = TestSuite("UTF16APIs")

UTF16APIs.test("width") {
  expectEqual(1, UTF16.width("x"))
  expectEqual(2, UTF16.width("\u{101010}"))
  expectEqual(2, UTF16.width("𝄞"))
}

UTF16APIs.test("leadSurrogate,trailSurrogate") {
  if true {
    let us: UnicodeScalar = "𝄞"
    expectEqual(0xD834, UTF16.leadSurrogate(us))
    expectEqual(0xDD1E, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{10000}"
    expectEqual(0xD800, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{20000}"
    expectEqual(0xD840, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{80000}"
    expectEqual(0xD9C0, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{F0000}"
    expectEqual(0xDB80, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{100000}"
    expectEqual(0xDBC0, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  if true {
    let us: UnicodeScalar = "\u{10FFFF}"
    expectEqual(0xDBFF, UTF16.leadSurrogate(us))
    expectEqual(0xDFFF, UTF16.trailSurrogate(us))
  }
}

UTF16APIs.test("leadSurrogate/trap/U+0000") {
  let us: UnicodeScalar = "\u{00}"
  expectCrashLater()
  UTF16.leadSurrogate(us)
}

UTF16APIs.test("leadSurrogate/trap/U+005A") {
  let us: UnicodeScalar = "\u{5A}"
  expectCrashLater()
  UTF16.leadSurrogate(us)
}

UTF16APIs.test("leadSurrogate/trap/U+FFFF") {
  let us: UnicodeScalar = "\u{FFFF}"
  expectCrashLater()
  UTF16.leadSurrogate(us)
}

UTF16APIs.test("trailSurrogate/trap/U+0000") {
  let us: UnicodeScalar = "\u{00}"
  expectCrashLater()
  UTF16.trailSurrogate(us)
}

UTF16APIs.test("trailSurrogate/trap/U+005A") {
  let us: UnicodeScalar = "\u{5A}"
  expectCrashLater()
  UTF16.trailSurrogate(us)
}

UTF16APIs.test("trailSurrogate/trap/U+FFFF") {
  let us: UnicodeScalar = "\u{FFFF}"
  expectCrashLater()
  UTF16.trailSurrogate(us)
}

class EOFCountingGenerator<T> : GeneratorType {
  var array: [T]
  var index: Int = 0
  var numTimesReturnedEOF: Int = 0

  init(_ array: [T]) {
    self.array = array
  }

  func next() -> T? {
    if index == array.count {
      ++numTimesReturnedEOF
      return .None
    }
    return array[index++]
  }
}

struct ArraySinkOf<T> : SinkType {
  init() {}

  init(_ array: [T]) {
    self.array = array
  }

  mutating func put(x: T) {
    array.append(x)
  }

  var array: [T] = []
}

func checkDecodeUTF<Codec : UnicodeCodecType>(
    codec: Codec.Type, expectedHead: [UInt32],
    expectedRepairedTail: [UInt32], utfStr: [Codec.CodeUnit]
) -> AssertionResult {
  if true {
    var decoded = ArraySinkOf<UInt32>()
    var g = EOFCountingGenerator(utfStr)
    transcode(codec, UTF32.self, g, &decoded, stopOnError: true)
    expectGE(1, g.numTimesReturnedEOF)
    if expectedHead != decoded.array {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expectedHead: \(asHex(expectedHead))\n")
          .withDescription("actual:       \(asHex(decoded.array))")
    }
  }

  if true {
    var expected = expectedHead
    expected += expectedRepairedTail

    var decoded = ArraySinkOf<UInt32>()
    var g = EOFCountingGenerator(utfStr)
    transcode(codec, UTF32.self, g, &decoded, stopOnError: false)
    expectEqual(1, g.numTimesReturnedEOF)
    if expected != decoded.array {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expected: \(asHex(expected))\n")
          .withDescription("actual:   \(asHex(decoded.array))")
    }
  }

  return assertionSuccess()
}

func checkDecodeUTF8(
    expectedHead: [UInt32],
    expectedRepairedTail: [UInt32], utf8Str: [UInt8]
) -> AssertionResult {
  return checkDecodeUTF(UTF8.self, expectedHead, expectedRepairedTail, utf8Str)
}

func checkDecodeUTF16(
    expectedHead: [UInt32],
    expectedRepairedTail: [UInt32], utf16Str: [UInt16]
) -> AssertionResult {
  return checkDecodeUTF(UTF16.self, expectedHead, expectedRepairedTail,
      utf16Str)
}

func checkDecodeUTF32(
    expectedHead: [UInt32],
    expectedRepairedTail: [UInt32], utf32Str: [UInt32]
) -> AssertionResult {
  return checkDecodeUTF(UTF32.self, expectedHead, expectedRepairedTail,
      utf32Str)
}

func checkEncodeUTF8(expected: [UInt8], scalars: [UInt32]) -> AssertionResult {
  var encoded = ArraySinkOf<UInt8>()
  var g = EOFCountingGenerator(scalars)
  let hadError =
    transcode(UTF32.self, UTF8.self, g, &encoded, stopOnError: true)
  expectFalse(hadError)
  expectGE(1, g.numTimesReturnedEOF)
  if expected != encoded.array {
    return assertionFailure()
        .withDescription("\n")
        .withDescription("expected: \(asHex(expected))\n")
        .withDescription("actual:   \(asHex(encoded.array))")
  }

  return assertionSuccess()
}

struct UTF8Test {
  let scalars: [UInt32]
  let encoded: [UInt8]
  let loc: SourceLoc

  init(_ scalars: [UInt32], _ encoded: [UInt8],
       file: String = __FILE__, line: UWord = __LINE__) {
    self.scalars = scalars
    self.encoded = encoded
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

let UTF8TestsSmokeTest = [
  //
  // 1-byte sequences
  //

  // U+0041 LATIN CAPITAL LETTER A
  UTF8Test([ 0x0041 ], [ 0x41 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  UTF8Test([ 0x0041, 0x0042 ], [ 0x41, 0x42 ]),

  // U+0061 LATIN SMALL LETTER A
  // U+0062 LATIN SMALL LETTER B
  // U+0063 LATIN SMALL LETTER C
  UTF8Test([ 0x0061, 0x0062, 0x0063 ], [ 0x61, 0x62, 0x63 ]),

  // U+0000 NULL
  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0000 NULL
  UTF8Test(
      [ 0x0000, 0x0041, 0x0042, 0x0000 ],
      [ 0x00, 0x41, 0x42, 0x00 ]),

  //
  // 2-byte sequences
  //

  // U+0283 LATIN SMALL LETTER ESH
  UTF8Test([ 0x0283 ], [ 0xca, 0x83 ]),

  // U+03BA GREEK SMALL LETTER KAPPA
  // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
  // U+03C3 GREEK SMALL LETTER SIGMA
  // U+03BC GREEK SMALL LETTER MU
  // U+03B5 GREEK SMALL LETTER EPSILON
  UTF8Test(
      [ 0x03ba, 0x1f79, 0x03c3, 0x03bc, 0x03b5 ],
      [ 0xce, 0xba, 0xe1, 0xbd, 0xb9, 0xcf, 0x83, 0xce, 0xbc, 0xce, 0xb5 ]),

  // U+0430 CYRILLIC SMALL LETTER A
  // U+0431 CYRILLIC SMALL LETTER BE
  // U+0432 CYRILLIC SMALL LETTER VE
  UTF8Test([ 0x0430, 0x0431, 0x0432 ], [ 0xd0, 0xb0, 0xd0, 0xb1, 0xd0, 0xb2 ]),

  //
  // 3-byte sequences
  //

  // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
  // U+6587 CJK UNIFIED IDEOGRAPH-6587
  UTF8Test(
      [ 0x4f8b, 0x6587 ],
      [ 0xe4, 0xbe, 0x8b, 0xe6, 0x96, 0x87 ]),

  // U+D55C HANGUL SYLLABLE HAN
  // U+AE00 HANGUL SYLLABLE GEUL
  UTF8Test(
      [ 0xd55c, 0xae00 ],
      [ 0xed, 0x95, 0x9c, 0xea, 0xb8, 0x80 ]),

  // U+1112 HANGUL CHOSEONG HIEUH
  // U+1161 HANGUL JUNGSEONG A
  // U+11AB HANGUL JONGSEONG NIEUN
  // U+1100 HANGUL CHOSEONG KIYEOK
  // U+1173 HANGUL JUNGSEONG EU
  // U+11AF HANGUL JONGSEONG RIEUL
  UTF8Test(
      [ 0x1112, 0x1161, 0x11ab, 0x1100, 0x1173, 0x11af ],
      [ 0xe1, 0x84, 0x92, 0xe1, 0x85, 0xa1, 0xe1, 0x86, 0xab,
        0xe1, 0x84, 0x80, 0xe1, 0x85, 0xb3, 0xe1, 0x86, 0xaf ]),

  // U+3042 HIRAGANA LETTER A
  // U+3044 HIRAGANA LETTER I
  // U+3046 HIRAGANA LETTER U
  // U+3048 HIRAGANA LETTER E
  // U+304A HIRAGANA LETTER O
  UTF8Test(
      [ 0x3042, 0x3044, 0x3046, 0x3048, 0x304a ],
      [ 0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86,
        0xe3, 0x81, 0x88, 0xe3, 0x81, 0x8a ]),

  //
  // 4-byte sequences
  //

  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0001F425 ],
      [ 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0001F425 ],
      [ 0x41, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0001F425 ],
      [ 0x41, 0x42, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+0048 LATIN CAPITAL LETTER H
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048,
        0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+0048 LATIN CAPITAL LETTER H
  // U+0049 LATIN CAPITAL LETTER I
  // U+1F425 FRONT-FACING BABY CHICK
  UTF8Test(
      [ 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0x0049,
        0x0001F425 ],
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0xf0, 0x9f, 0x90, 0xa5 ]),

  // U+E0100 VARIATION SELECTOR-17
  UTF8Test(
      [ 0x000E0100 ],
      [ 0xf3, 0xa0, 0x84, 0x80 ]),
]

struct UTF16Test {
  let scalarsHead: [UInt32]
  let scalarsRepairedTail: [UInt32]
  let encoded: [UInt16]
  let loc: SourceLoc

  init(_ scalarsHead: [UInt32], _ scalarsRepairedTail: [UInt32],
       _ encoded: [UInt16],
       file: String = __FILE__, line: UWord = __LINE__) {
    self.scalarsHead = scalarsHead
    self.scalarsRepairedTail = scalarsRepairedTail
    self.encoded = encoded
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

let UTF16Tests = [
  "Empty": [
    UTF16Test([], [], []),
  ],

  "SmokeTest": [
    //
    // 1-word sequences
    //

    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([ 0x0041 ], [], [ 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+0042 LATIN CAPITAL LETTER B
    UTF16Test([ 0x0041, 0x0042 ], [], [ 0x0041, 0x0042 ]),

    // U+0000 NULL
    // U+0041 LATIN CAPITAL LETTER A
    // U+0042 LATIN CAPITAL LETTER B
    // U+0000 NULL
    UTF16Test(
        [ 0x0000, 0x0041, 0x0042, 0x0000 ], [],
        [ 0x0000, 0x0041, 0x0042, 0x0000 ]),

    // U+0283 LATIN SMALL LETTER ESH
    UTF16Test([ 0x0283 ], [], [ 0x0283 ]),

    // U+03BA GREEK SMALL LETTER KAPPA
    // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
    // U+03C3 GREEK SMALL LETTER SIGMA
    // U+03BC GREEK SMALL LETTER MU
    // U+03B5 GREEK SMALL LETTER EPSILON
    UTF16Test(
        [ 0x03ba, 0x1f79, 0x03c3, 0x03bc, 0x03b5 ], [],
        [ 0x03ba, 0x1f79, 0x03c3, 0x03bc, 0x03b5 ]),

    // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
    // U+6587 CJK UNIFIED IDEOGRAPH-6587
    UTF16Test(
        [ 0x4f8b, 0x6587 ], [],
        [ 0x4f8b, 0x6587 ]),

    // U+D55C HANGUL SYLLABLE HAN
    // U+AE00 HANGUL SYLLABLE GEUL
    UTF16Test(
        [ 0xd55c, 0xae00 ], [],
        [ 0xd55c, 0xae00 ]),

    // U+1112 HANGUL CHOSEONG HIEUH
    // U+1161 HANGUL JUNGSEONG A
    // U+11AB HANGUL JONGSEONG NIEUN
    // U+1100 HANGUL CHOSEONG KIYEOK
    // U+1173 HANGUL JUNGSEONG EU
    // U+11AF HANGUL JONGSEONG RIEUL
    UTF16Test(
        [ 0x1112, 0x1161, 0x11ab, 0x1100, 0x1173, 0x11af ], [],
        [ 0x1112, 0x1161, 0x11ab, 0x1100, 0x1173, 0x11af ]),

    // U+D7FF (unassigned)
    UTF16Test([ 0xd7ff ], [], [ 0xd7ff ]),

    // U+E000 (private use)
    UTF16Test([ 0xe000 ], [], [ 0xe000 ]),

    // U+FFFD REPLACEMENT CHARACTER
    UTF16Test([ 0xfffd ], [], [ 0xfffd ]),

    // U+FFFF (noncharacter)
    UTF16Test([ 0xffff ], [], [ 0xffff ]),

    //
    // 2-word sequences
    //

    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test([ 0x00010000 ], [], [ 0xd800, 0xdc00 ]),

    // U+10100 AEGEAN WORD SEPARATOR LINE
    UTF16Test([ 0x00010100 ], [], [ 0xd800, 0xdd00 ]),

    // U+103FF (unassigned)
    UTF16Test([ 0x000103ff ], [], [ 0xd800, 0xdfff ]),


    // U+E0000 (unassigned)
    UTF16Test([ 0x000e0000 ], [], [ 0xdb40, 0xdc00 ]),

    // U+E0100 VARIATION SELECTOR-17
    UTF16Test([ 0x000e0100 ], [], [ 0xdb40, 0xdd00 ]),

    // U+E03FF (unassigned)
    UTF16Test([ 0x000e03ff ], [], [ 0xdb40, 0xdfff ]),


    // U+10FC00 (private use)
    UTF16Test([ 0x0010fc00 ], [], [ 0xdbff, 0xdc00 ]),

    // U+10FD00 (private use)
    UTF16Test([ 0x0010fd00 ], [], [ 0xdbff, 0xdd00 ]),

    // U+10FFFF (private use, noncharacter)
    UTF16Test([ 0x0010ffff ], [], [ 0xdbff, 0xdfff ]),
  ],

  "Incomplete": [
    //
    // Incomplete sequences that end right before EOF.
    //

    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xfffd ], [ 0xd800 ]),

    // U+D800 (high-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xd800, 0xd800 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    UTF16Test([ 0x0041 ], [ 0xfffd ], [ 0x0041, 0xd800 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+D800 (high-surrogate)
    UTF16Test(
        [ 0x00010000 ], [ 0xfffd ],
        [ 0xd800, 0xdc00, 0xd800 ]),

    //
    // Incomplete sequences with more code units following them.
    //

    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xfffd, 0x0041 ], [ 0xd800, 0x0041 ]),

    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xfffd, 0x00010000 ],
        [ 0xd800, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0x0041 ],
        [ 0x0041, 0xd800, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0x00010000 ],
        [ 0x0041, 0xd800, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0x0041 ],
        [ 0x0041, 0xd800, 0xdb40, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0x00010000 ],
        [ 0x0041, 0xd800, 0xdb40, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
        [ 0x0041, 0xd800, 0xdb40, 0xdbff, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0xfffd, 0x00010000 ],
        [ 0x0041, 0xd800, 0xdb40, 0xdbff, 0xd800, 0xdc00 ]),
  ],

  "IllFormed": [
    //
    // Low-surrogate right before EOF.
    //

    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xfffd ], [ 0xdc00 ]),

    // U+DC00 (low-surrogate)
    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdc00, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    UTF16Test([ 0x0041 ], [ 0xfffd ], [ 0x0041, 0xdc00 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+DC00 (low-surrogate)
    UTF16Test(
        [ 0x00010000 ], [ 0xfffd ],
        [ 0xd800, 0xdc00, 0xdc00 ]),

    //
    // Low-surrogate with more code units following it.
    //

    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xfffd, 0x0041 ], [ 0xdc00, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xfffd, 0x00010000 ],
        [ 0xdc00, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0x0041 ],
        [ 0x0041, 0xdc00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0x00010000 ],
        [ 0x0041, 0xdc00, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0x0041 ],
        [ 0x0041, 0xdc00, 0xdd00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0x00010000 ],
        [ 0x0041, 0xdc00, 0xdd00, 0xd800, 0xdc00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
        [ 0x0041, 0xdc00, 0xdd00, 0xdfff, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xfffd, 0xfffd, 0xfffd, 0x00010000 ],
        [ 0x0041, 0xdc00, 0xdd00, 0xdfff, 0xd800, 0xdc00 ]),

    //
    // Low-surrogate followed by high-surrogate.
    //

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdc00, 0xd800 ]),

    // U+DC00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdc00, 0xdb40 ]),

    // U+DC00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdc00, 0xdbff ]),


    // U+DD00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdd00, 0xd800 ]),

    // U+DD00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdd00, 0xdb40 ]),

    // U+DD00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdd00, 0xdbff ]),


    // U+DFFF (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdfff, 0xd800 ]),

    // U+DFFF (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdfff, 0xdb40 ]),

    // U+DFFF (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xfffd, 0xfffd ], [ 0xdfff, 0xdbff ]),


    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [], [ 0xfffd, 0xfffd, 0x0041 ],
        [ 0xdc00, 0xd800, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xfffd, 0xfffd, 0x10000 ],
        [ 0xdc00, 0xd800, 0xd800, 0xdc00 ]),
  ],
]

var UnicodeScalarTests = TestSuite("UnicodeScalarTests")

UnicodeScalarTests.test("literal") {
  if true {
    // U+0041 LATIN CAPITAL LETTER A
    let us: UnicodeScalar = "A"
    expectEqual(0x0041, us.value)
  }

  if true {
    // U+3042 HIRAGANA LETTER A
    let us: UnicodeScalar = "あ"
    expectEqual(0x3042, us.value)
  }

  if true {
    // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
    let us: UnicodeScalar = "例"
    expectEqual(0x4F8B, us.value)
  }
}

UnicodeScalarTests.test("init") {
  expectEqual("f", UnicodeScalar(102))
  expectEqual("g", UnicodeScalar(UInt32(103)))
  expectEqual("h", UnicodeScalar(UInt16(104)))
  expectEqual("i", UnicodeScalar(UInt8(105)))
}

var UTF8Decoder = TestSuite("UTF8Decoder")

UTF8Decoder.test("Internal/_numTrailingBytes") {
  for i in UInt8(0x00)...UInt8(0x7f) {
    expectEqual(0, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0x80)...UInt8(0xc1) {
    expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xc2)...UInt8(0xdf) {
    expectEqual(1, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xe0)...UInt8(0xef) {
    expectEqual(2, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xf0)...UInt8(0xf4) {
    expectEqual(3, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xf5)...UInt8(0xfe) {
    expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  // Separate test for 0xff because of:
  // <rdar://problem/17376512> Range UInt8(0x00)...UInt8(0xff) invokes a
  // runtime trap
  var i = UInt8(0xff)
  expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
}

UTF8Decoder.test("Empty") {
  expectTrue(checkDecodeUTF8([], [], []))
}

UTF8Decoder.test("SmokeTest") {
  for test in UTF8TestsSmokeTest {
    expectTrue(checkDecodeUTF8(test.scalars, [], test.encoded),
        stackTrace: test.loc.withCurrentLoc())
  }
}

UTF8Decoder.test("FirstPossibleSequence") {
  //
  // First possible sequence of a certain length
  //

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // U+0080 PADDING CHARACTER
  expectTrue(checkDecodeUTF8([ 0x0080 ], [], [ 0xc2, 0x80 ]))

  // U+0800 SAMARITAN LETTER ALAF
  expectTrue(checkDecodeUTF8(
      [ 0x0800 ], [],
      [ 0xe0, 0xa0, 0x80 ]))

  // U+10000 LINEAR B SYLLABLE B008 A
  expectTrue(checkDecodeUTF8(
      [ 0x10000 ], [],
      [ 0xf0, 0x90, 0x80, 0x80 ]))

  // U+200000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80, 0x80 ]))

  // U+4000000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80, 0x80 ]))
}

UTF8Decoder.test("LastPossibleSequence") {
  //
  // Last possible sequence of a certain length
  //

  // U+007F DELETE
  expectTrue(checkDecodeUTF8([ 0x007f ], [], [ 0x7f ]))

  // U+07FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0x07ff ], [], [ 0xdf, 0xbf ]))

  // U+FFFF (noncharacter)
  expectTrue(checkDecodeUTF8(
      [ 0xffff ], [],
      [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf, 0xbf ]))

  // U+3FFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf, 0xbf ]))

  // U+7FFFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("CodeSpaceBoundaryConditions") {
  //
  // Other boundary conditions
  //

  // U+D7FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0xd7ff ], [], [ 0xed, 0x9f, 0xbf ]))

  // U+E000 (private use)
  expectTrue(checkDecodeUTF8([ 0xe000 ], [], [ 0xee, 0x80, 0x80 ]))

  // U+FFFD REPLACEMENT CHARACTER
  expectTrue(checkDecodeUTF8([ 0xfffd ], [], [ 0xef, 0xbf, 0xbd ]))

  // U+10FFFF (noncharacter)
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

  // U+110000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80, 0x80 ]))
}

UTF8Decoder.test("UnexpectedContinuationBytes") {
  //
  // Unexpected continuation bytes
  //

  // A sequence of unexpected continuation bytes that don't follow a first
  // byte, every byte is a maximal subpart.

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x82, 0xbf, 0xaa ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0, 0x8f ]))

  // All continuation bytes (0x80--0xbf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
        0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
        0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
        0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
        0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
        0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
        0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
        0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf ]))
}

UTF8Decoder.test("LonelyStartBytes") {
  //
  // Lonely start bytes
  //

  // Start bytes of 2-byte sequences (0xc0--0xdf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
        0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
        0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
        0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xc2, 0x20, 0xc3, 0x20,
        0xc4, 0x20, 0xc5, 0x20, 0xc6, 0x20, 0xc7, 0x20,
        0xc8, 0x20, 0xc9, 0x20, 0xca, 0x20, 0xcb, 0x20,
        0xcc, 0x20, 0xcd, 0x20, 0xce, 0x20, 0xcf, 0x20,
        0xd0, 0x20, 0xd1, 0x20, 0xd2, 0x20, 0xd3, 0x20,
        0xd4, 0x20, 0xd5, 0x20, 0xd6, 0x20, 0xd7, 0x20,
        0xd8, 0x20, 0xd9, 0x20, 0xda, 0x20, 0xdb, 0x20,
        0xdc, 0x20, 0xdd, 0x20, 0xde, 0x20, 0xdf, 0x20 ]))

  // Start bytes of 3-byte sequences (0xe0--0xef).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
        0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xe0, 0x20, 0xe1, 0x20, 0xe2, 0x20, 0xe3, 0x20,
        0xe4, 0x20, 0xe5, 0x20, 0xe6, 0x20, 0xe7, 0x20,
        0xe8, 0x20, 0xe9, 0x20, 0xea, 0x20, 0xeb, 0x20,
        0xec, 0x20, 0xed, 0x20, 0xee, 0x20, 0xef, 0x20 ]))

  // Start bytes of 4-byte sequences (0xf0--0xf7).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf0, 0x20, 0xf1, 0x20, 0xf2, 0x20, 0xf3, 0x20,
        0xf4, 0x20, 0xf5, 0x20, 0xf6, 0x20, 0xf7, 0x20 ]))

  // Start bytes of 5-byte sequences (0xf8--0xfb).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xf9, 0xfa, 0xfb ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf8, 0x20, 0xf9, 0x20, 0xfa, 0x20, 0xfb, 0x20 ]))

  // Start bytes of 6-byte sequences (0xfc--0xfd).
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xfd ]))

  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xfc, 0x20, 0xfd, 0x20 ]))
}

UTF8Decoder.test("InvalidStartBytes") {
  //
  // Other bytes (0xc0--0xc1, 0xfe--0xff).
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfe ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xfe, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0xfe, 0xff, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xff, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xfe, 0x20, 0xff, 0x20 ]))
}

UTF8Decoder.test("MissingContinuationBytes") {
  //
  // Sequences with one continuation byte missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xdf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xc2, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xdf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xa0, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xec, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe1, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xec, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x9f ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x9f, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xee, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xef, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xee, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xef, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0x90, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf1, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf3, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x8f, 0xbf, 0x41 ]))

  // Overlong sequences with one trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x9f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with one trailing byte missing.
  // High-surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xac ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xaf ]))
  // Low-surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb4 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xbf ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+1100xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80 ]))
  // U+13FBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf5, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf6, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0x80, 0x80 ]))
  // U+1FFBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+2000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80, 0x80 ]))
  // U+3FFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10uzzzzz 10zzzyyyy 10yyyyxx 10xxxxxx
  // U+40000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80, 0x80 ]))
  // U+7FFFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with two continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f ]))

  // Overlong sequences with two trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x8f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with two trailing bytes missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+110yxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0x90 ]))
  // U+13Fyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf5, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf6, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0x80 ]))
  // U+1FFyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+200yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80 ]))
  // U+3FFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+4000yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80 ]))
  // U+7FFFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with three continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4 ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80 ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+14yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf5 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf6 ]))
  // U+1Cyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf7 ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+20yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x88 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf9, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfa, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0x80 ]))
  // U+3FCyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+400yyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80 ]))
  // U+7FFCyyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf ]))

  //
  // Sequences with four continuation bytes missing
  //

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf9 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfa ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))
  // U+3zyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x80 ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x84 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0x80 ]))
  // U+7Fzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0xbf ]))

  //
  // Sequences with five continuation bytes missing
  //

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfc ]))
  // U+uuzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfd ]))

  //
  // Consecutive sequences with trailing bytes missing
  //

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, /**/ 0xfffd, 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, /**/ 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, /**/ 0xe0, 0x80, /**/ 0xf0, 0x80, 0x80,
        0xf8, 0x80, 0x80, 0x80,
        0xfc, 0x80, 0x80, 0x80, 0x80,
        0xdf, /**/ 0xef, 0xbf, /**/ 0xf7, 0xbf, 0xbf,
        0xfb, 0xbf, 0xbf, 0xbf,
        0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("OverlongSequences") {
  //
  // Overlong UTF-8 sequences
  //

  // U+002F SOLIDUS
  expectTrue(checkDecodeUTF8([ 0x002f ], [], [ 0x2f ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0xaf ]))

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  // Other overlong and ill-formed sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x9f, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x87, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x83, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("IsolatedSurrogates") {
  // Unicode 6.3.0:
  //
  //    D71.  High-surrogate code point: A Unicode code point in the range
  //    U+D800 to U+DBFF.
  //
  //    D73.  Low-surrogate code point: A Unicode code point in the range
  //    U+DC00 to U+DFFF.

  // Note: U+E0100 is <DB40 DD00> in UTF-16.

  // High-surrogates

  // U+D800
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [ 0x0041 ],
      [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
      [ 0x41, 0xed, 0xa0, 0x80, 0x41 ]))

  // U+DB40
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0 ]))

  // U+DBFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf ]))

  // Low-surrogates

  // U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb0, 0x80 ]))

  // U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb4, 0x80 ]))

  // U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("SurrogatePairs") {
  // Surrogate pairs

  // U+D800 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb0, 0x80 ]))

  // U+D800 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb4, 0x80 ]))

  // U+D800 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xbf, 0xbf ]))

  // U+DB40 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb0, 0x80 ]))

  // U+DB40 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb4, 0x80 ]))

  // U+DB40 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xbf, 0xbf ]))

  // U+DBFF U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb0, 0x80 ]))

  // U+DBFF U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb4, 0x80 ]))

  // U+DBFF U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("Noncharacters") {
  //
  // Noncharacters
  //

  // Unicode 6.3.0:
  //
  //    D14.  Noncharacter: A code point that is permanently reserved for
  //    internal use and that should never be interchanged. Noncharacters
  //    consist of the values U+nFFFE and U+nFFFF (where n is from 0 to 1016)
  //    and the values U+FDD0..U+FDEF.

  // U+FFFE
  expectTrue(checkDecodeUTF8([ 0xfffe ], [], [ 0xef, 0xbf, 0xbe ]))

  // U+FFFF
  expectTrue(checkDecodeUTF8([ 0xffff ], [], [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFE
  expectTrue(checkDecodeUTF8([ 0x1fffe ], [], [ 0xf0, 0x9f, 0xbf, 0xbe ]))

  // U+1FFFF
  expectTrue(checkDecodeUTF8([ 0x1ffff ], [], [ 0xf0, 0x9f, 0xbf, 0xbf ]))

  // U+2FFFE
  expectTrue(checkDecodeUTF8([ 0x2fffe ], [], [ 0xf0, 0xaf, 0xbf, 0xbe ]))

  // U+2FFFF
  expectTrue(checkDecodeUTF8([ 0x2ffff ], [], [ 0xf0, 0xaf, 0xbf, 0xbf ]))

  // U+3FFFE
  expectTrue(checkDecodeUTF8([ 0x3fffe ], [], [ 0xf0, 0xbf, 0xbf, 0xbe ]))

  // U+3FFFF
  expectTrue(checkDecodeUTF8([ 0x3ffff ], [], [ 0xf0, 0xbf, 0xbf, 0xbf ]))

  // U+4FFFE
  expectTrue(checkDecodeUTF8([ 0x4fffe ], [], [ 0xf1, 0x8f, 0xbf, 0xbe ]))

  // U+4FFFF
  expectTrue(checkDecodeUTF8([ 0x4ffff ], [], [ 0xf1, 0x8f, 0xbf, 0xbf ]))

  // U+5FFFE
  expectTrue(checkDecodeUTF8([ 0x5fffe ], [], [ 0xf1, 0x9f, 0xbf, 0xbe ]))

  // U+5FFFF
  expectTrue(checkDecodeUTF8([ 0x5ffff ], [], [ 0xf1, 0x9f, 0xbf, 0xbf ]))

  // U+6FFFE
  expectTrue(checkDecodeUTF8([ 0x6fffe ], [], [ 0xf1, 0xaf, 0xbf, 0xbe ]))

  // U+6FFFF
  expectTrue(checkDecodeUTF8([ 0x6ffff ], [], [ 0xf1, 0xaf, 0xbf, 0xbf ]))

  // U+7FFFE
  expectTrue(checkDecodeUTF8([ 0x7fffe ], [], [ 0xf1, 0xbf, 0xbf, 0xbe ]))

  // U+7FFFF
  expectTrue(checkDecodeUTF8([ 0x7ffff ], [], [ 0xf1, 0xbf, 0xbf, 0xbf ]))

  // U+8FFFE
  expectTrue(checkDecodeUTF8([ 0x8fffe ], [], [ 0xf2, 0x8f, 0xbf, 0xbe ]))

  // U+8FFFF
  expectTrue(checkDecodeUTF8([ 0x8ffff ], [], [ 0xf2, 0x8f, 0xbf, 0xbf ]))

  // U+9FFFE
  expectTrue(checkDecodeUTF8([ 0x9fffe ], [], [ 0xf2, 0x9f, 0xbf, 0xbe ]))

  // U+9FFFF
  expectTrue(checkDecodeUTF8([ 0x9ffff ], [], [ 0xf2, 0x9f, 0xbf, 0xbf ]))

  // U+AFFFE
  expectTrue(checkDecodeUTF8([ 0xafffe ], [], [ 0xf2, 0xaf, 0xbf, 0xbe ]))

  // U+AFFFF
  expectTrue(checkDecodeUTF8([ 0xaffff ], [], [ 0xf2, 0xaf, 0xbf, 0xbf ]))

  // U+BFFFE
  expectTrue(checkDecodeUTF8([ 0xbfffe ], [], [ 0xf2, 0xbf, 0xbf, 0xbe ]))

  // U+BFFFF
  expectTrue(checkDecodeUTF8([ 0xbffff ], [], [ 0xf2, 0xbf, 0xbf, 0xbf ]))

  // U+CFFFE
  expectTrue(checkDecodeUTF8([ 0xcfffe ], [], [ 0xf3, 0x8f, 0xbf, 0xbe ]))

  // U+CFFFF
  expectTrue(checkDecodeUTF8([ 0xcfffF ], [], [ 0xf3, 0x8f, 0xbf, 0xbf ]))

  // U+DFFFE
  expectTrue(checkDecodeUTF8([ 0xdfffe ], [], [ 0xf3, 0x9f, 0xbf, 0xbe ]))

  // U+DFFFF
  expectTrue(checkDecodeUTF8([ 0xdffff ], [], [ 0xf3, 0x9f, 0xbf, 0xbf ]))

  // U+EFFFE
  expectTrue(checkDecodeUTF8([ 0xefffe ], [], [ 0xf3, 0xaf, 0xbf, 0xbe ]))

  // U+EFFFF
  expectTrue(checkDecodeUTF8([ 0xeffff ], [], [ 0xf3, 0xaf, 0xbf, 0xbf ]))

  // U+FFFFE
  expectTrue(checkDecodeUTF8([ 0xffffe ], [], [ 0xf3, 0xbf, 0xbf, 0xbe ]))

  // U+FFFFF
  expectTrue(checkDecodeUTF8([ 0xfffff ], [], [ 0xf3, 0xbf, 0xbf, 0xbf ]))

  // U+10FFFE
  expectTrue(checkDecodeUTF8([ 0x10fffe ], [], [ 0xf4, 0x8f, 0xbf, 0xbe ]))

  // U+10FFFF
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

  // U+FDD0
  expectTrue(checkDecodeUTF8([ 0xfdd0 ], [], [ 0xef, 0xb7, 0x90 ]))

  // U+FDD1
  expectTrue(checkDecodeUTF8([ 0xfdd1 ], [], [ 0xef, 0xb7, 0x91 ]))

  // U+FDD2
  expectTrue(checkDecodeUTF8([ 0xfdd2 ], [], [ 0xef, 0xb7, 0x92 ]))

  // U+FDD3
  expectTrue(checkDecodeUTF8([ 0xfdd3 ], [], [ 0xef, 0xb7, 0x93 ]))

  // U+FDD4
  expectTrue(checkDecodeUTF8([ 0xfdd4 ], [], [ 0xef, 0xb7, 0x94 ]))

  // U+FDD5
  expectTrue(checkDecodeUTF8([ 0xfdd5 ], [], [ 0xef, 0xb7, 0x95 ]))

  // U+FDD6
  expectTrue(checkDecodeUTF8([ 0xfdd6 ], [], [ 0xef, 0xb7, 0x96 ]))

  // U+FDD7
  expectTrue(checkDecodeUTF8([ 0xfdd7 ], [], [ 0xef, 0xb7, 0x97 ]))

  // U+FDD8
  expectTrue(checkDecodeUTF8([ 0xfdd8 ], [], [ 0xef, 0xb7, 0x98 ]))

  // U+FDD9
  expectTrue(checkDecodeUTF8([ 0xfdd9 ], [], [ 0xef, 0xb7, 0x99 ]))

  // U+FDDA
  expectTrue(checkDecodeUTF8([ 0xfdda ], [], [ 0xef, 0xb7, 0x9a ]))

  // U+FDDB
  expectTrue(checkDecodeUTF8([ 0xfddb ], [], [ 0xef, 0xb7, 0x9b ]))

  // U+FDDC
  expectTrue(checkDecodeUTF8([ 0xfddc ], [], [ 0xef, 0xb7, 0x9c ]))

  // U+FDDD
  expectTrue(checkDecodeUTF8([ 0xfddd ], [], [ 0xef, 0xb7, 0x9d ]))

  // U+FDDE
  expectTrue(checkDecodeUTF8([ 0xfdde ], [], [ 0xef, 0xb7, 0x9e ]))

  // U+FDDF
  expectTrue(checkDecodeUTF8([ 0xfddf ], [], [ 0xef, 0xb7, 0x9f ]))

  // U+FDE0
  expectTrue(checkDecodeUTF8([ 0xfde0 ], [], [ 0xef, 0xb7, 0xa0 ]))

  // U+FDE1
  expectTrue(checkDecodeUTF8([ 0xfde1 ], [], [ 0xef, 0xb7, 0xa1 ]))

  // U+FDE2
  expectTrue(checkDecodeUTF8([ 0xfde2 ], [], [ 0xef, 0xb7, 0xa2 ]))

  // U+FDE3
  expectTrue(checkDecodeUTF8([ 0xfde3 ], [], [ 0xef, 0xb7, 0xa3 ]))

  // U+FDE4
  expectTrue(checkDecodeUTF8([ 0xfde4 ], [], [ 0xef, 0xb7, 0xa4 ]))

  // U+FDE5
  expectTrue(checkDecodeUTF8([ 0xfde5 ], [], [ 0xef, 0xb7, 0xa5 ]))

  // U+FDE6
  expectTrue(checkDecodeUTF8([ 0xfde6 ], [], [ 0xef, 0xb7, 0xa6 ]))

  // U+FDE7
  expectTrue(checkDecodeUTF8([ 0xfde7 ], [], [ 0xef, 0xb7, 0xa7 ]))

  // U+FDE8
  expectTrue(checkDecodeUTF8([ 0xfde8 ], [], [ 0xef, 0xb7, 0xa8 ]))

  // U+FDE9
  expectTrue(checkDecodeUTF8([ 0xfde9 ], [], [ 0xef, 0xb7, 0xa9 ]))

  // U+FDEA
  expectTrue(checkDecodeUTF8([ 0xfdea ], [], [ 0xef, 0xb7, 0xaa ]))

  // U+FDEB
  expectTrue(checkDecodeUTF8([ 0xfdeb ], [], [ 0xef, 0xb7, 0xab ]))

  // U+FDEC
  expectTrue(checkDecodeUTF8([ 0xfdec ], [], [ 0xef, 0xb7, 0xac ]))

  // U+FDED
  expectTrue(checkDecodeUTF8([ 0xfded ], [], [ 0xef, 0xb7, 0xad ]))

  // U+FDEE
  expectTrue(checkDecodeUTF8([ 0xfdee ], [], [ 0xef, 0xb7, 0xae ]))

  // U+FDEF
  expectTrue(checkDecodeUTF8([ 0xfdef ], [], [ 0xef, 0xb7, 0xaf ]))

  // U+FDF0
  expectTrue(checkDecodeUTF8([ 0xfdf0 ], [], [ 0xef, 0xb7, 0xb0 ]))

  // U+FDF1
  expectTrue(checkDecodeUTF8([ 0xfdf1 ], [], [ 0xef, 0xb7, 0xb1 ]))

  // U+FDF2
  expectTrue(checkDecodeUTF8([ 0xfdf2 ], [], [ 0xef, 0xb7, 0xb2 ]))

  // U+FDF3
  expectTrue(checkDecodeUTF8([ 0xfdf3 ], [], [ 0xef, 0xb7, 0xb3 ]))

  // U+FDF4
  expectTrue(checkDecodeUTF8([ 0xfdf4 ], [], [ 0xef, 0xb7, 0xb4 ]))

  // U+FDF5
  expectTrue(checkDecodeUTF8([ 0xfdf5 ], [], [ 0xef, 0xb7, 0xb5 ]))

  // U+FDF6
  expectTrue(checkDecodeUTF8([ 0xfdf6 ], [], [ 0xef, 0xb7, 0xb6 ]))

  // U+FDF7
  expectTrue(checkDecodeUTF8([ 0xfdf7 ], [], [ 0xef, 0xb7, 0xb7 ]))

  // U+FDF8
  expectTrue(checkDecodeUTF8([ 0xfdf8 ], [], [ 0xef, 0xb7, 0xb8 ]))

  // U+FDF9
  expectTrue(checkDecodeUTF8([ 0xfdf9 ], [], [ 0xef, 0xb7, 0xb9 ]))

  // U+FDFA
  expectTrue(checkDecodeUTF8([ 0xfdfa ], [], [ 0xef, 0xb7, 0xba ]))

  // U+FDFB
  expectTrue(checkDecodeUTF8([ 0xfdfb ], [], [ 0xef, 0xb7, 0xbb ]))

  // U+FDFC
  expectTrue(checkDecodeUTF8([ 0xfdfc ], [], [ 0xef, 0xb7, 0xbc ]))

  // U+FDFD
  expectTrue(checkDecodeUTF8([ 0xfdfd ], [], [ 0xef, 0xb7, 0xbd ]))

  // U+FDFE
  expectTrue(checkDecodeUTF8([ 0xfdfe ], [], [ 0xef, 0xb7, 0xbe ]))

  // U+FDFF
  expectTrue(checkDecodeUTF8([ 0xfdff ], [], [ 0xef, 0xb7, 0xbf ]))
}

var UTF16Decoder = TestSuite("UTF16Decoder")

UTF16Decoder.test("measure") {
  if true {
    var u8: [UTF8.CodeUnit] = [ 0, 1, 2, 3, 4, 5 ]
    let (count, isASCII) = UTF16.measure(
      UTF8.self, input: u8.generate(), repairIllFormedSequences: false)!
    expectEqual(6, count)
    expectTrue(isASCII)
  }

  if true {
    // "€" == U+20AC.
    var u8: [UTF8.CodeUnit] = [ 0xF0, 0xA4, 0xAD, 0xA2 ]
    let (count, isASCII) = UTF16.measure(
      UTF8.self, input: u8.generate(), repairIllFormedSequences: false)!
    expectEqual(2, count)
    expectFalse(isASCII)
  }

  if true {
    let u16: [UTF16.CodeUnit] = [ 6, 7, 8, 9, 10, 11 ]
    let (count, isASCII) = UTF16.measure(
      UTF16.self, input: u16.generate(), repairIllFormedSequences: false)!
    expectEqual(6, count)
    expectTrue(isASCII)
  }
}

UTF16Decoder.test("Decoding") {
  for (name, batch) in UTF16Tests {
    println("Batch: \(name)")
    for test in batch {
      expectTrue(checkDecodeUTF16(test.scalarsHead, test.scalarsRepairedTail,
          test.encoded), stackTrace: test.loc.withCurrentLoc())
    }
  }
}

var UTF32Decoder = TestSuite("UTF32Decoder")

UTF32Decoder.test("Empty") {
  expectTrue(checkDecodeUTF32([], [], []))
}

UTF32Decoder.test("SmokeTest") {
  // U+0041 LATIN CAPITAL LETTER A
  expectTrue(checkDecodeUTF32([ 0x0041 ], [], [ 0x0000_0041 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  expectTrue(checkDecodeUTF32(
      [ 0x0041, 0x0042 ], [],
      [ 0x0000_0041, 0x0000_0042 ]))

  // U+0000 NULL
  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0000 NULL
  expectTrue(checkDecodeUTF32(
      [ 0x0000, 0x0041, 0x0042, 0x0000 ], [],
      [ 0x0000_0000, 0x0000_0041, 0x0000_0042, 0x0000_0000 ]))

  // U+0283 LATIN SMALL LETTER ESH
  expectTrue(checkDecodeUTF32([ 0x0283 ], [], [ 0x0000_0283 ]))

  // U+03BA GREEK SMALL LETTER KAPPA
  // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
  // U+03C3 GREEK SMALL LETTER SIGMA
  // U+03BC GREEK SMALL LETTER MU
  // U+03B5 GREEK SMALL LETTER EPSILON
  expectTrue(checkDecodeUTF32(
      [ 0x03ba, 0x1f79, 0x03c3, 0x03bc, 0x03b5 ], [],
      [ 0x0000_03ba, 0x0000_1f79, 0x0000_03c3, 0x0000_03bc, 0x0000_03b5 ]))

  // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
  // U+6587 CJK UNIFIED IDEOGRAPH-6587
  expectTrue(checkDecodeUTF32(
      [ 0x4f8b, 0x6587 ], [],
      [ 0x0000_4f8b, 0x0000_6587 ]))

  // U+D55C HANGUL SYLLABLE HAN
  // U+AE00 HANGUL SYLLABLE GEUL
  expectTrue(checkDecodeUTF32(
      [ 0xd55c, 0xae00 ], [],
      [ 0x0000_d55c, 0x0000_ae00 ]))

  // U+1112 HANGUL CHOSEONG HIEUH
  // U+1161 HANGUL JUNGSEONG A
  // U+11AB HANGUL JONGSEONG NIEUN
  // U+1100 HANGUL CHOSEONG KIYEOK
  // U+1173 HANGUL JUNGSEONG EU
  // U+11AF HANGUL JONGSEONG RIEUL
  expectTrue(checkDecodeUTF32(
      [ 0x1112, 0x1161, 0x11ab, 0x1100, 0x1173, 0x11af ], [],
      [ 0x0000_1112, 0x0000_1161, 0x0000_11ab, 0x0000_1100, 0x0000_1173,
        0x0000_11af ]))

  // U+D7FF (unassigned)
  expectTrue(checkDecodeUTF16([ 0xd7ff ], [], [ 0x0000_d7ff ]))

  // U+E000 (private use)
  expectTrue(checkDecodeUTF16([ 0xe000 ], [], [ 0x0000_e000 ]))

  // U+FFFD REPLACEMENT CHARACTER
  expectTrue(checkDecodeUTF16([ 0xfffd ], [], [ 0x0000_fffd ]))

  // U+FFFF (noncharacter)
  expectTrue(checkDecodeUTF16([ 0xffff ], [], [ 0x0000_ffff ]))

  // U+10000 LINEAR B SYLLABLE B008 A
  expectTrue(checkDecodeUTF32([ 0x00010000 ], [], [ 0x0001_0000 ]))

  // U+10100 AEGEAN WORD SEPARATOR LINE
  expectTrue(checkDecodeUTF32([ 0x00010100 ], [], [ 0x0001_0100 ]))

  // U+103FF (unassigned)
  expectTrue(checkDecodeUTF32([ 0x000103ff ], [], [ 0x0001_03ff ]))

  // U+1D800 (unassigned)
  expectTrue(checkDecodeUTF32([ 0x0001d800 ], [], [ 0x0001_d800 ]))


  // U+E0000 (unassigned)
  expectTrue(checkDecodeUTF32([ 0x000e0000 ], [], [ 0x000e_0000 ]))

  // U+E0100 VARIATION SELECTOR-17
  expectTrue(checkDecodeUTF32([ 0x000e0100 ], [], [ 0x000e_0100 ]))

  // U+E03FF (unassigned)
  expectTrue(checkDecodeUTF32([ 0x000e03ff ], [], [ 0x000e_03ff ]))


  // U+10FC00 (private use)
  expectTrue(checkDecodeUTF32([ 0x0010fc00 ], [], [ 0x0010_fc00 ]))

  // U+10FD00 (private use)
  expectTrue(checkDecodeUTF32([ 0x0010fd00 ], [], [ 0x0010_fd00 ]))

  // U+10FFFF (private use, noncharacter)
  expectTrue(checkDecodeUTF32([ 0x0010ffff ], [], [ 0x0010_ffff ]))
}

UTF32Decoder.test("IllFormed") {
  // U+D800 (high-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_d800 ]))

  // U+DB40 (high-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_db40 ]))

  // U+DBFF (high-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_dbff ]))

  // U+DC00 (low-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_dc00 ]))

  // U+DD00 (low-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_dd00 ]))

  // U+DFFF (low-surrogate)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0000_dfff ]))

  // U+110000 (invalid)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0011_0000 ]))

  // U+1000000 (invalid)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x0100_0000 ]))

  // U+80000000 (invalid)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0x8000_0000 ]))

  // U+FFFF0000 (invalid)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0xffff_0000 ]))

  // U+FFFFFFFF (invalid)
  expectTrue(checkDecodeUTF32([], [ 0xfffd ], [ 0xffff_ffff ]))
}

var UTF8Encoder = TestSuite("UTF8Encoder")

UTF8Encoder.test("SmokeTest") {
  for test in UTF8TestsSmokeTest {
    expectTrue(checkEncodeUTF8(test.encoded, test.scalars),
        stackTrace: test.loc.withCurrentLoc())
  }
}

struct ArraySinkStruct<T> : SinkType {
  mutating func put(x: T) {
    array.append(x)
  }
  var array: [T] = []
}

class ArraySinkClass<T> : SinkType {
  func put(x: T) {
    array.append(x)
  }
  var array: [T] = []
}

var UnicodeAPIs = TestSuite("UnicodeAPIs")

UnicodeAPIs.test("transcode/MutableSink") {
  var input: [UInt16] = [ 0x0041, 0x0042 ]
  var sink = ArraySinkStruct<UInt16>()
  transcode(UTF16.self, UTF16.self, input.generate(), &sink, stopOnError: true)
  expectEqual(input, sink.array)
}

UnicodeAPIs.test("transcode/ReferenceTypedSink") {
  var input: [UInt16] = [ 0x0041, 0x0042 ]
  var sink = ArraySinkClass<UInt16>()
  transcode(UTF16.self, UTF16.self, input.generate(), &sink, stopOnError: true)
  expectEqual(input, sink.array)
}

// The most simple subclass of NSString that CoreFoundation does not know
// about.
class NonContiguousNSString : NSString {
  override init() {
    _value = []
    super.init()
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("don't call this initializer")
  }

  convenience init(_ utf8: [UInt8]) {
    var encoded = ArraySinkOf<UInt16>()
    var g = utf8.generate()
    let hadError =
      transcode(UTF8.self, UTF16.self, g, &encoded, stopOnError: true)
    expectFalse(hadError)
    self.init(encoded.array)
  }

  init(_ value: [UInt16]) {
    _value = value
    super.init()
  }

  convenience init(_ scalars: [UInt32]) {
    var encoded = ArraySinkOf<UInt16>()
    var g = scalars.generate()
    let hadError =
      transcode(UTF32.self, UTF16.self, g, &encoded, stopOnError: true)
    expectFalse(hadError)
    self.init(encoded.array)
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    // Ensure that copying this string produces a class that CoreFoundation
    // does not know about.
    return self
  }

  @objc override var length: Int {
    return _value.count
  }

  @objc override func characterAtIndex(index: Int) -> unichar {
    return _value[index]
  }

  var _value: [UInt16]
}

func checkUTF8View(expected: [UInt8], subject: String,
    stackTrace: SourceLocStack) {
  checkCollection(expected, subject.utf8, stackTrace.withCurrentLoc())
}

func checkUTF16View(expected: [UInt16], subject: String,
    stackTrace: SourceLocStack) {
  checkSliceableWithBidirectionalIndex(expected, subject.utf16,
      stackTrace.withCurrentLoc())
}

func forStringsWithUnpairedSurrogates(checkClosure: (UTF16Test, String) -> ()) {
  for (name, batch) in UTF16Tests {
    println("Batch: \(name)")
    for test in batch {
      let subject = NonContiguousNSString(test.encoded) as String
      checkClosure(test, subject)
    }
  }
}

var StringCookedViews = TestSuite("StringCookedViews")

StringCookedViews.test("UTF8ForContiguousUTF16") {
  for test in UTF8TestsSmokeTest {
    // Add a non-ASCII character at the beginning to force Swift String and
    // CoreFoundation off the ASCII fast path.
    //
    // U+0283 LATIN SMALL LETTER ESH
    var backingStorage = ArraySinkOf<UInt16>([ 0x0283 ])
    let expected: [UInt8] = [ 0xca, 0x83 ] + test.encoded

    var g = test.scalars.generate()
    transcode(UTF32.self, UTF16.self, g, &backingStorage, stopOnError: false)

    backingStorage.array.withUnsafeBufferPointer {
      (ptr) -> () in
      let cfstring = CFStringCreateWithCharactersNoCopy(kCFAllocatorDefault,
          ptr.baseAddress, backingStorage.array.count, kCFAllocatorNull)
      expectFalse(CFStringGetCStringPtr(cfstring,
          CFStringBuiltInEncodings.ASCII.rawValue) != nil)
      expectTrue(CFStringGetCharactersPtr(cfstring) != nil)
      checkUTF8View(
        expected, cfstring as NSString as String, test.loc.withCurrentLoc())
      return ()
    }
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> () in
    var expected = ArraySinkOf<UInt8>()
    var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF8.self, g, &expected, stopOnError: false)

    checkUTF8View(expected.array, subject, test.loc.withCurrentLoc())
  }
}

func verifyThatStringIsOpaqueForCoreFoundation(nss: NSString) {
  // Sanity checks to make sure we are testing the code path that does UTF-8
  // encoding itself, instead of dispatching to CF.  Both the original string
  // itself and its copies should be resilient to CF's fast path functions,
  // because Swift bridging may copy the string to ensure that it is not
  // mutated.
  let cfstring = unsafeBitCast(nss, CFString.self)
  assert(
    CFStringGetCStringPtr(
      cfstring, CFStringBuiltInEncodings.ASCII.rawValue) == nil)
  assert(
    CFStringGetCStringPtr(
      cfstring, CFStringBuiltInEncodings.UTF8.rawValue) == nil)

  assert(CFStringGetCharactersPtr(cfstring) == nil)

  let copy = CFStringCreateCopy(nil, cfstring)
  assert(
    CFStringGetCStringPtr(
      copy, CFStringBuiltInEncodings.ASCII.rawValue) == nil)
  assert(
    CFStringGetCStringPtr(
      copy, CFStringBuiltInEncodings.UTF8.rawValue) == nil)
  assert(CFStringGetCharactersPtr(copy) == nil)
}

StringCookedViews.test("UTF8ForNonContiguousUTF16") {
  for test in UTF8TestsSmokeTest {
    var nss = NonContiguousNSString(test.scalars)
    verifyThatStringIsOpaqueForCoreFoundation(nss)
    checkUTF8View(test.encoded, nss as String, test.loc.withCurrentLoc())
  }

  for (name, batch) in UTF16Tests {
    println("Batch: \(name)")
    for test in batch {
      var expected = ArraySinkOf<UInt8>()
      var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
      var g = expectedScalars.generate()
      transcode(UTF32.self, UTF8.self, g, &expected, stopOnError: false)

      var nss = NonContiguousNSString(test.encoded)
      verifyThatStringIsOpaqueForCoreFoundation(nss)
      checkUTF8View(expected.array, nss as String, test.loc.withCurrentLoc())
    }
  }
}

StringCookedViews.test("UTF8ForNonContiguousUTF16Extra") {
  // These tests don't add much additional value as long as tests above
  // actually test the code path we care about.
  if true {
    var bytes: [UInt8] = [ 97, 98, 99 ]
    var cfstring: CFString = CFStringCreateWithBytesNoCopy(
      kCFAllocatorDefault, bytes, bytes.count,
      CFStringBuiltInEncodings.MacRoman.rawValue, 0, kCFAllocatorNull)

    // Sanity checks to make sure we are testing the code path that does UTF-8
    // encoding itself, instead of dispatching to CF.
    // GetCStringPtr fails because our un-copied bytes aren't zero-terminated.
    // GetCharactersPtr fails because our un-copied bytes aren't UTF-16.
    assert(
      CFStringGetCStringPtr(
        cfstring, CFStringBuiltInEncodings.ASCII.rawValue) == nil)
    assert(
      CFStringGetCStringPtr(
        cfstring, CFStringBuiltInEncodings.UTF8.rawValue) == nil)
    assert(CFStringGetCharactersPtr(cfstring) == nil)

    checkUTF8View(
        bytes, cfstring as NSString as String,
        SourceLocStack().withCurrentLoc())
    _fixLifetime(bytes)
  }

  if true {
    var bytes: [UInt8] = [ 97, 98, 99 ]
    var cfstring: CFString = CFStringCreateWithBytes(kCFAllocatorDefault,
        bytes, bytes.count, CFStringBuiltInEncodings.MacRoman.rawValue, 0)

    // Sanity checks to make sure we are testing the code path that does UTF-8
    // encoding itself, instead of dispatching to CF.
    // CFStringCreateWithBytes() usually allocates zero-terminated ASCII
    // or UTF-16, in which case one of the fast paths will succeed.
    // This test operates only when CF creates a tagged pointer string object.
    if (object_getClassName(cfstring) == "NSTaggedPointerString") {
      assert(
        CFStringGetCStringPtr(
          cfstring, CFStringBuiltInEncodings.ASCII.rawValue) == nil)
      assert(
        CFStringGetCStringPtr(
          cfstring, CFStringBuiltInEncodings.UTF8.rawValue) == nil)
      assert(CFStringGetCharactersPtr(cfstring) == nil)

      checkUTF8View(
        bytes, cfstring as NSString as String,
        SourceLocStack().withCurrentLoc())
    }
  }
}

StringCookedViews.test("UTF16") {
  for test in UTF8TestsSmokeTest {
    var expected = ArraySinkOf<UInt16>()
    var expectedScalars = test.scalars
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF16.self, g, &expected, stopOnError: false)

    var nss = NonContiguousNSString(test.scalars)
    checkUTF16View(expected.array, nss as String, test.loc.withCurrentLoc())
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> () in
    var expected = ArraySinkOf<UInt16>()
    var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF16.self, g, &expected, stopOnError: false)

    checkUTF16View(expected.array, subject, test.loc.withCurrentLoc())
  }
}

StringCookedViews.test("UnicodeScalars") {
  for test in UTF8TestsSmokeTest {
    let expectedScalars = map(test.scalars) { UnicodeScalar($0) }
    let subject = NonContiguousNSString(test.scalars) as String
    checkSliceableWithBidirectionalIndex(expectedScalars,
        subject.unicodeScalars, test.loc.withCurrentLoc())
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> () in
    let expectedScalars = map(test.scalarsHead + test.scalarsRepairedTail) {
      UnicodeScalar($0)
    }
    checkSliceableWithBidirectionalIndex(expectedScalars,
        subject.unicodeScalars, test.loc.withCurrentLoc())
  }
}

var StringTests = TestSuite("StringTests")

StringTests.test("StreamableConformance") {
  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> () in
    let expected = test.scalarsHead + test.scalarsRepairedTail
    let printedSubject = String(subject)
    let actual = map(printedSubject.unicodeScalars) { $0.value }
    expectEqual(expected, actual)
  }
}

runAllTests()

