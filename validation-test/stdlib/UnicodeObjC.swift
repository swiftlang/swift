// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

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
  do {
    let us: UnicodeScalar = "𝄞"
    expectEqual(0xD834, UTF16.leadSurrogate(us))
    expectEqual(0xDD1E, UTF16.trailSurrogate(us))
  }
  do {
    let us: UnicodeScalar = "\u{10000}"
    expectEqual(0xD800, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  do {
    let us: UnicodeScalar = "\u{20000}"
    expectEqual(0xD840, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  do {
    let us: UnicodeScalar = "\u{80000}"
    expectEqual(0xD9C0, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  do {
    let us: UnicodeScalar = "\u{F0000}"
    expectEqual(0xDB80, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  do {
    let us: UnicodeScalar = "\u{100000}"
    expectEqual(0xDBC0, UTF16.leadSurrogate(us))
    expectEqual(0xDC00, UTF16.trailSurrogate(us))
  }
  do {
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
      numTimesReturnedEOF += 1
      return .None
    }
    return array[index++]
  }
}

func checkDecodeUTF<Codec : UnicodeCodecType>(
    codec: Codec.Type, _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utfStr: [Codec.CodeUnit]
) -> AssertionResult {
  do {
    var decoded = [UInt32]()
    let output: (UInt32) -> Void = { decoded.append($0) }
    let g = EOFCountingGenerator(utfStr)
    transcode(codec, UTF32.self, g, output, stopOnError: true)
    expectGE(1, g.numTimesReturnedEOF)
    if expectedHead != decoded {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expectedHead: \(asHex(expectedHead))\n")
          .withDescription("actual:       \(asHex(decoded))")
    }
  }

  do {
    var expected = expectedHead
    expected += expectedRepairedTail

    var decoded = [UInt32]()
    let output: (UInt32) -> Void = { decoded.append($0) }
    let g = EOFCountingGenerator(utfStr)
    transcode(codec, UTF32.self, g, output, stopOnError: false)
    expectEqual(1, g.numTimesReturnedEOF)
    if expected != decoded {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expected: \(asHex(expected))\n")
          .withDescription("actual:   \(asHex(decoded))")
    }
  }

  return assertionSuccess()
}

func checkDecodeUTF8(
    expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf8Str: [UInt8]
) -> AssertionResult {
  return checkDecodeUTF(UTF8.self, expectedHead, expectedRepairedTail, utf8Str)
}

func checkDecodeUTF16(
    expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf16Str: [UInt16]
) -> AssertionResult {
  return checkDecodeUTF(UTF16.self, expectedHead, expectedRepairedTail,
      utf16Str)
}

func checkDecodeUTF32(
    expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf32Str: [UInt32]
) -> AssertionResult {
  return checkDecodeUTF(UTF32.self, expectedHead, expectedRepairedTail,
      utf32Str)
}

func checkEncodeUTF8(expected: [UInt8],
                     _ scalars: [UInt32]) -> AssertionResult {
  var encoded = [UInt8]()
  let output: (UInt8) -> Void = { encoded.append($0) }
  let g = EOFCountingGenerator(scalars)
  let hadError =
    transcode(UTF32.self, UTF8.self, g, output, stopOnError: true)
  expectFalse(hadError)
  expectGE(1, g.numTimesReturnedEOF)
  if expected != encoded {
    return assertionFailure()
        .withDescription("\n")
        .withDescription("expected: \(asHex(expected))\n")
        .withDescription("actual:   \(asHex(encoded))")
  }

  return assertionSuccess()
}

struct UTF8Test {
  let scalars: [UInt32]
  let encoded: [UInt8]
  let loc: SourceLoc

  init(
    _ scalars: [UInt32], _ encoded: [UInt8],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
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

  init(
    _ scalarsHead: [UInt32], _ scalarsRepairedTail: [UInt32],
    _ encoded: [UInt16],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
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
    var encoded = [UInt16]()
    let output: (UInt16) -> Void = { encoded.append($0) }
    let g = utf8.generate()
    let hadError =
      transcode(UTF8.self, UTF16.self, g, output, stopOnError: true)
    expectFalse(hadError)
    self.init(encoded)
  }

  init(_ value: [UInt16]) {
    _value = value
    super.init()
  }

  convenience init(_ scalars: [UInt32]) {
    var encoded = [UInt16]()
    let output: (UInt16) -> Void = { encoded.append($0) }
    let g = scalars.generate()
    let hadError =
      transcode(UTF32.self, UTF16.self, g, output, stopOnError: true)
    expectFalse(hadError)
    self.init(encoded)
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

func checkUTF8View(expected: [UInt8], _ subject: String,
    _ stackTrace: SourceLocStack) {
  checkForwardCollection(expected, subject.utf8)
}

func checkUTF16View(expected: [UInt16], _ subject: String,
    _ stackTrace: SourceLocStack) {
  checkSliceableWithBidirectionalIndex(expected, subject.utf16)
}

func forStringsWithUnpairedSurrogates(checkClosure: (UTF16Test, String) -> Void) {
  for (name, batch) in UTF16Tests {
    print("Batch: \(name)")
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
    var backingStorage: [UInt16] = [ 0x0283 ]
    let expected: [UInt8] = [ 0xca, 0x83 ] + test.encoded
    let output: (UInt16) -> Void = { backingStorage.append($0) }

    var g = test.scalars.generate()
    transcode(UTF32.self, UTF16.self, g, output, stopOnError: false)

    backingStorage.withUnsafeBufferPointer {
      (ptr) -> Void in
      let cfstring = CFStringCreateWithCharactersNoCopy(kCFAllocatorDefault,
          ptr.baseAddress, backingStorage.count, kCFAllocatorNull)
      expectFalse(CFStringGetCStringPtr(cfstring,
          CFStringBuiltInEncodings.ASCII.rawValue) != nil)
      expectTrue(CFStringGetCharactersPtr(cfstring) != nil)
      checkUTF8View(
        expected, cfstring as NSString as String, test.loc.withCurrentLoc())
      return ()
    }
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> Void in
    var expected = [UInt8]()
    let output: (UInt8) -> Void = { expected.append($0) }
    var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF8.self, g, output, stopOnError: false)

    checkUTF8View(expected, subject, test.loc.withCurrentLoc())
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
    print("Batch: \(name)")
    for test in batch {
      var expected = [UInt8]()
      let output: (UInt8) -> Void = { expected.append($0) }
      var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
      var g = expectedScalars.generate()
      transcode(UTF32.self, UTF8.self, g, output, stopOnError: false)

      var nss = NonContiguousNSString(test.encoded)
      verifyThatStringIsOpaqueForCoreFoundation(nss)
      checkUTF8View(expected, nss as String, test.loc.withCurrentLoc())
    }
  }
}

StringCookedViews.test("UTF8ForNonContiguousUTF16Extra") {
  // These tests don't add much additional value as long as tests above
  // actually test the code path we care about.
  do {
    var bytes: [UInt8] = [ 97, 98, 99 ]
    var cfstring: CFString = CFStringCreateWithBytesNoCopy(
      kCFAllocatorDefault, bytes, bytes.count,
      CFStringBuiltInEncodings.MacRoman.rawValue, false, kCFAllocatorNull)

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

  do {
    var bytes: [UInt8] = [ 97, 98, 99 ]
    var cfstring: CFString = CFStringCreateWithBytes(kCFAllocatorDefault,
        bytes, bytes.count, CFStringBuiltInEncodings.MacRoman.rawValue, false)

    // Sanity checks to make sure we are testing the code path that does UTF-8
    // encoding itself, instead of dispatching to CF.
    // CFStringCreateWithBytes() usually allocates zero-terminated ASCII
    // or UTF-16, in which case one of the fast paths will succeed.
    // This test operates only when CF creates a tagged pointer string object.
    if strcmp(object_getClassName(cfstring), "NSTaggedPointerString") == 0 {
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
    var expected = [UInt16]()
    let output: (UInt16) -> Void = { expected.append($0) }
    var expectedScalars = test.scalars
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF16.self, g, output, stopOnError: false)

    var nss = NonContiguousNSString(test.scalars)
    checkUTF16View(expected, nss as String, test.loc.withCurrentLoc())
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> Void in
    var expected = [UInt16]()
    let output: (UInt16) -> Void = { expected.append($0) }
    var expectedScalars = test.scalarsHead + test.scalarsRepairedTail
    var g = expectedScalars.generate()
    transcode(UTF32.self, UTF16.self, g, output, stopOnError: false)

    checkUTF16View(expected, subject, test.loc.withCurrentLoc())
  }
}

StringCookedViews.test("UnicodeScalars") {
  for test in UTF8TestsSmokeTest {
    let expectedScalars = test.scalars.map { UnicodeScalar($0) }
    let subject = NonContiguousNSString(test.scalars) as String
    checkSliceableWithBidirectionalIndex(
      expectedScalars, subject.unicodeScalars)
  }

  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> Void in
    let expectedScalars = (test.scalarsHead + test.scalarsRepairedTail).map {
      UnicodeScalar($0)
    }
    checkSliceableWithBidirectionalIndex(
      expectedScalars, subject.unicodeScalars)
  }
}

var StringTests = TestSuite("StringTests")

StringTests.test("StreamableConformance") {
  forStringsWithUnpairedSurrogates {
    (test: UTF16Test, subject: String) -> Void in
    let expected = test.scalarsHead + test.scalarsRepairedTail
    let printedSubject = String(subject)
    let actual = printedSubject.unicodeScalars.map { $0.value }
    expectEqual(expected, actual)
  }
}

runAllTests()

