//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import StdlibUnittest

extension String {
  func parseUTF8CodeUnits() -> [UInt8] {
    var utf8 = [UInt8]()
    let units = self.split(separator: " ")
    let scalars = units.compactMap { string -> Unicode.Scalar? in
      let i = Int(string, radix: 16)!
      return Unicode.Scalar(i)

    }

    for scalar in scalars {
      utf8 += String(scalar).utf8
    }
    return utf8
  }
  
  func parseUTF16CodeUnits() -> [UInt16] {
    var utf16 = [UInt16]()
    let units = self.split(separator: " ")
    let scalars = units.compactMap { string -> Unicode.Scalar? in
      let i = Int(string, radix: 16)!
      return Unicode.Scalar(i)
    }
    
    for scalar in scalars {
      utf16 += scalar.utf16
    }
    return utf16
  }
}

public struct NormalizationTest {
  public let loc: SourceLoc
  public let sourceUTF16: [UInt16]
  public let source: [UInt8]
  public let NFC: [UInt8]
  public let NFD: [UInt8]
  public let NFKC: [UInt8]
  public let NFKD: [UInt8]

  init(
    loc: SourceLoc,
    source: String,
    NFC: String,
    NFD: String,
    NFKC: String,
    NFKD: String
  ) {
    self.loc = loc
    self.sourceUTF16 = source.parseUTF16CodeUnits()
    self.source = source.parseUTF8CodeUnits()
    self.NFC = NFC.parseUTF8CodeUnits()
    self.NFD = NFD.parseUTF8CodeUnits()
    self.NFKC = NFKC.parseUTF8CodeUnits()
    self.NFKD = NFKD.parseUTF8CodeUnits()
  }
}

// Normalization tests are currently only avaible on Darwin, awaiting a sensible
// file API...
#if _runtime(_ObjC)
import Foundation
public let normalizationTests: [NormalizationTest] = {
  var tests = [NormalizationTest]()

  let file = CommandLine.arguments[2]
  let fileURL = URL(fileURLWithPath: file)

  let fileContents = try! String(contentsOf: fileURL) + "" // go faster

  var lineNumber: UInt = 0
  for line in fileContents.split(separator: "\n") {
    lineNumber += 1
    guard line.hasPrefix("#") == false else {
      continue
    }

    let content = line.split(separator: "#").first!
  
    guard !content.isEmpty else {
      continue
    }
    guard !content.hasPrefix("@") else {
      continue
    }

    let columns = content.split(separator: ";").filter { $0 != " " }.map(String.init)
    let test = NormalizationTest(
      loc: SourceLoc(file, lineNumber),
      source: columns[0],
      NFC: columns[1],
      NFD: columns[2],
      NFKC: columns[3],
      NFKD: columns[4])

    tests.append(test)
  }
  
  return tests
}()
#endif

public struct UTFTest {
  public struct Flags : OptionSet {
    public let rawValue: Int

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }

    public static let utf8IsInvalid = Flags(rawValue: 1 << 0)
    public static let utf16IsInvalid = Flags(rawValue: 1 << 1)
  }

  public let string: String
  public let utf8: [UInt8]
  public let utf16: [UInt16]
  public let unicodeScalars: [Unicode.Scalar]
  public let unicodeScalarsRepairedTail: [Unicode.Scalar]
  public let flags: Flags
  public let loc: SourceLoc

  public var utf32: [UInt32] {
    return unicodeScalars.map(UInt32.init)
  }

  public var utf32RepairedTail: [UInt32] {
    return unicodeScalarsRepairedTail.map(UInt32.init)
  }

  public init(
    string: String,
    utf8: [UInt8],
    utf16: [UInt16],
    scalars: [UInt32],
    scalarsRepairedTail: [UInt32] = [],
    flags: Flags = [],
    file: String = #file, line: UInt = #line
  ) {
    self.string = string
    self.utf8 = utf8
    self.utf16 = utf16
    self.unicodeScalars = scalars.map { Unicode.Scalar($0)! }
    self.unicodeScalarsRepairedTail =
      scalarsRepairedTail.map { Unicode.Scalar($0)! }
    self.flags = flags
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public let utfTests: [UTFTest] = [
  //
  // Empty sequence.
  //

  UTFTest(
    string: "",
    utf8: [],
    utf16: [],
    scalars: []),

  //
  // 1-byte sequences.
  //

  // U+0000 NULL
  UTFTest(
    string: "\u{0000}",
    utf8: [ 0x00 ],
    utf16: [ 0x00 ],
    scalars: [ 0x00 ]),

  // U+0041 LATIN CAPITAL LETTER A
  UTFTest(
    string: "A",
    utf8: [ 0x41 ],
    utf16: [ 0x41 ],
    scalars: [ 0x41 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  UTFTest(
    string: "AB",
    utf8: [ 0x41, 0x42 ],
    utf16: [ 0x41, 0x42 ],
    scalars: [ 0x41, 0x42 ]),

  // U+0061 LATIN SMALL LETTER A
  // U+0062 LATIN SMALL LETTER B
  // U+0063 LATIN SMALL LETTER C
  UTFTest(
    string: "ABC",
    utf8: [ 0x41, 0x42, 0x43 ],
    utf16: [ 0x41, 0x42, 0x43 ],
    scalars: [ 0x41, 0x42, 0x43 ]),

  // U+0000 NULL
  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0000 NULL
  UTFTest(
    string: "\u{0000}AB\u{0000}",
    utf8: [ 0x00, 0x41, 0x42, 0x00 ],
    utf16: [ 0x00, 0x41, 0x42, 0x00 ],
    scalars: [ 0x00, 0x41, 0x42, 0x00 ]),

  // U+007F DELETE
  UTFTest(
    string: "\u{007F}",
    utf8: [ 0x7F ],
    utf16: [ 0x7F ],
    scalars: [ 0x7F ]),

  //
  // 2-byte sequences.
  //

  // U+0283 LATIN SMALL LETTER ESH
  UTFTest(
    string: "\u{0283}",
    utf8: [ 0xCA, 0x83 ],
    utf16: [ 0x0283 ],
    scalars: [ 0x0283 ]),

  // U+03BA GREEK SMALL LETTER KAPPA
  // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
  // U+03C3 GREEK SMALL LETTER SIGMA
  // U+03BC GREEK SMALL LETTER MU
  // U+03B5 GREEK SMALL LETTER EPSILON
  UTFTest(
    string: "\u{03BA}\u{1F79}\u{03C3}\u{03BC}\u{03B5}",
    utf8: [ 0xCE, 0xBA, 0xE1, 0xBD, 0xB9, 0xCF, 0x83, 0xCE, 0xBC, 0xCE, 0xB5 ],
    utf16: [ 0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5 ],
    scalars: [ 0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5 ]),

  // U+0430 CYRILLIC SMALL LETTER A
  // U+0431 CYRILLIC SMALL LETTER BE
  // U+0432 CYRILLIC SMALL LETTER VE
  UTFTest(
    string: "\u{0430}\u{0431}\u{0432}",
    utf8: [ 0xD0, 0xB0, 0xD0, 0xB1, 0xD0, 0xB2 ],
    utf16: [ 0x0430, 0x0431, 0x0432 ],
    scalars: [ 0x0430, 0x0431, 0x0432 ]),

  //
  // 3-byte sequences.
  //

  // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
  // U+6587 CJK UNIFIED IDEOGRAPH-6587
  UTFTest(
    string: "\u{4F8b}\u{6587}",
    utf8: [ 0xE4, 0xBE, 0x8B, 0xE6, 0x96, 0x87 ],
    utf16: [ 0x4F8B, 0x6587 ],
    scalars: [ 0x4F8B, 0x6587 ]),

  // U+D55C HANGUL SYLLABLE HAN
  // U+AE00 HANGUL SYLLABLE GEUL
  UTFTest(
    string: "\u{d55c}\u{ae00}",
    utf8: [ 0xED, 0x95, 0x9C, 0xEA, 0xB8, 0x80 ],
    utf16: [ 0xD55C, 0xAE00 ],
    scalars: [ 0xD55C, 0xAE00 ]),

  // U+1112 HANGUL CHOSEONG HIEUH
  // U+1161 HANGUL JUNGSEONG A
  // U+11AB HANGUL JONGSEONG NIEUN
  // U+1100 HANGUL CHOSEONG KIYEOK
  // U+1173 HANGUL JUNGSEONG EU
  // U+11AF HANGUL JONGSEONG RIEUL
  UTFTest(
    string: "\u{1112}\u{1161}\u{11ab}\u{1100}\u{1173}\u{11af}",
    utf8:
      [ 0xE1, 0x84, 0x92, 0xE1, 0x85, 0xA1, 0xE1, 0x86, 0xAB,
        0xE1, 0x84, 0x80, 0xE1, 0x85, 0xB3, 0xE1, 0x86, 0xAF ],
    utf16: [ 0x1112, 0x1161, 0x11AB, 0x1100, 0x1173, 0x11AF ],
    scalars: [ 0x1112, 0x1161, 0x11AB, 0x1100, 0x1173, 0x11AF ]),

  // U+3042 HIRAGANA LETTER A
  // U+3044 HIRAGANA LETTER I
  // U+3046 HIRAGANA LETTER U
  // U+3048 HIRAGANA LETTER E
  // U+304A HIRAGANA LETTER O
  UTFTest(
    string: "\u{3042}\u{3044}\u{3046}\u{3048}\u{304a}",
    utf8:
      [ 0xE3, 0x81, 0x82, 0xE3, 0x81, 0x84, 0xE3, 0x81, 0x86,
        0xE3, 0x81, 0x88, 0xE3, 0x81, 0x8A ],
    utf16: [ 0x3042, 0x3044, 0x3046, 0x3048, 0x304A ],
    scalars: [ 0x3042, 0x3044, 0x3046, 0x3048, 0x304A ]),

  // U+D7FF (unassigned)
  UTFTest(
    string: "\u{D7FF}",
    utf8: [ 0xED, 0x9F, 0xBF ],
    utf16: [ 0xD7FF ],
    scalars: [ 0xD7FF ]),

  // U+E000 (private use)
  UTFTest(
    string: "\u{E000}",
    utf8: [ 0xEE, 0x80, 0x80 ],
    utf16: [ 0xE000 ],
    scalars: [ 0xE000 ]),

  // U+FFFD REPLACEMENT CHARACTER
  UTFTest(
    string: "\u{FFFD}",
    utf8: [ 0xEF, 0xBF, 0xBD ],
    utf16: [ 0xFFFD ],
    scalars: [ 0xFFFD ]),

  // U+FFFF (noncharacter)
  UTFTest(
    string: "\u{FFFF}",
    utf8: [ 0xEF, 0xBF, 0xBF ],
    utf16: [ 0xFFFF ],
    scalars: [ 0xFFFF ]),

  //
  // 4-byte sequences.
  //

  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "\u{1F425}",
    utf8: [ 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0xD83D, 0xDC25 ],
    scalars: [ 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "A\u{1F425}",
    utf8: [ 0x41, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "AB\u{1F425}",
    utf8: [ 0x41, 0x42, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABC\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABCD\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABCDE\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABCDEF\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABCDEFG\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x0001_F425 ]),

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+0048 LATIN CAPITAL LETTER H
  // U+1F425 FRONT-FACING BABY CHICK
  UTFTest(
    string: "ABCDEFGH\u{1F425}",
    utf8:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0xF0, 0x9F, 0x90, 0xA5 ],
    utf16:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0xD83D, 0xDC25 ],
    scalars:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x0001_F425 ]),

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
  UTFTest(
    string: "ABCDEFGHI\u{1F425}",
    utf8:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0xF0, 0x9F, 0x90, 0xA5 ],
    utf16:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0xD83D, 0xDC25 ],
    scalars:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x0001_F425 ]),

  // U+10000 LINEAR B SYLLABLE B008 A
  UTFTest(
    string: "\u{10000}",
    utf8: [ 0xF0, 0x90, 0x80, 0x80 ],
    utf16: [ 0xD800, 0xDC00 ],
    scalars: [ 0x0001_0000 ]),

  // U+10100 AEGEAN WORD SEPARATOR LINE
  UTFTest(
    string: "\u{10100}",
    utf8: [ 0xF0, 0x90, 0x84, 0x80 ],
    utf16: [ 0xD800, 0xDD00 ],
    scalars: [ 0x0001_0100 ]),

  // U+103FF (unassigned)
  UTFTest(
    string: "\u{103FF}",
    utf8: [ 0xF0, 0x90, 0x8F, 0xBF ],
    utf16: [ 0xD800, 0xDFFF ],
    scalars: [ 0x0001_03FF ]),

  // U+E0000 (unassigned)
  UTFTest(
    string: "\u{E0000}",
    utf8: [ 0xF3, 0xA0, 0x80, 0x80 ],
    utf16: [ 0xDB40, 0xDC00 ],
    scalars: [ 0x000E_0000 ]),

  // U+E0100 VARIATION SELECTOR-17
  UTFTest(
    string: "\u{E0100}",
    utf8: [ 0xF3, 0xA0, 0x84, 0x80 ],
    utf16: [ 0xDB40, 0xDD00 ],
    scalars: [ 0x000E_0100 ]),

  // U+E03FF (unassigned)
  UTFTest(
    string: "\u{E03FF}",
    utf8: [ 0xF3, 0xA0, 0x8F, 0xBF ],
    utf16: [ 0xDB40, 0xDFFF ],
    scalars: [ 0x000E_03FF ]),

  // U+10FC00 (private use)
  UTFTest(
    string: "\u{10FC00}",
    utf8: [ 0xF4, 0x8F, 0xB0, 0x80 ],
    utf16: [ 0xDBFF, 0xDC00 ],
    scalars: [ 0x0010_FC00 ]),

  // U+10FD00 (private use)
  UTFTest(
    string: "\u{10FD00}",
    utf8: [ 0xF4, 0x8F, 0xB4, 0x80 ],
    utf16: [ 0xDBFF, 0xDD00 ],
    scalars: [ 0x0010_FD00 ]),

  // U+10FFFF (private use, noncharacter)
  UTFTest(
    string: "\u{10FFFF}",
    utf8: [ 0xF4, 0x8F, 0xBF, 0xBF ],
    utf16: [ 0xDBFF, 0xDFFF ],
    scalars: [ 0x0010_FFFF ]),
]


public struct UTF16Test {
  public let scalarsHead: [UInt32]
  public let scalarsRepairedTail: [UInt32]
  public let encoded: [UInt16]
  public let loc: SourceLoc

  public init(
    _ scalarsHead: [UInt32], _ scalarsRepairedTail: [UInt32],
    _ encoded: [UInt16],
    file: String = #file, line: UInt = #line
  ) {
    self.scalarsHead = scalarsHead
    self.scalarsRepairedTail = scalarsRepairedTail
    self.encoded = encoded
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public let utf16Tests = [
  "Incomplete": [
    //
    // Incomplete sequences that end right before EOF.
    //

    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD ], [ 0xD800 ]),

    // U+D800 (high-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xD800, 0xD800 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    UTF16Test([ 0x0041 ], [ 0xFFFD ], [ 0x0041, 0xD800 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+D800 (high-surrogate)
    UTF16Test(
        [ 0x0001_0000 ], [ 0xFFFD ],
        [ 0xD800, 0xDC00, 0xD800 ]),

    //
    // Incomplete sequences with more code units following them.
    //

    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xFFFD, 0x0041 ], [ 0xD800, 0x0041 ]),

    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0x0001_0000 ],
        [ 0xD800, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0xDB40, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xDB40, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0xDB40, 0xDBFF, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xDB40, 0xDBFF, 0xD800, 0xDC00 ]),
  ],

  "IllFormed": [
    //
    // Low-surrogate right before EOF.
    //

    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xFFFD ], [ 0xDC00 ]),

    // U+DC00 (low-surrogate)
    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    UTF16Test([ 0x0041 ], [ 0xFFFD ], [ 0x0041, 0xDC00 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+DC00 (low-surrogate)
    UTF16Test(
        [ 0x0001_0000 ], [ 0xFFFD ],
        [ 0xD800, 0xDC00, 0xDC00 ]),

    //
    // Low-surrogate with more code units following it.
    //

    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xFFFD, 0x0041 ], [ 0xDC00, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0x0001_0000 ],
        [ 0xDC00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0xDD00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xDFFF, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xDFFF, 0xD800, 0xDC00 ]),

    //
    // Low-surrogate followed by high-surrogate.
    //

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xD800 ]),

    // U+DC00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDB40 ]),

    // U+DC00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDBFF ]),


    // U+DD00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xD800 ]),

    // U+DD00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xDB40 ]),

    // U+DD00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xDBFF ]),


    // U+DFFF (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xD800 ]),

    // U+DFFF (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xDB40 ]),

    // U+DFFF (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xDBFF ]),


    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0xDC00, 0xD800, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0xFFFD, 0x10000 ],
        [ 0xDC00, 0xD800, 0xD800, 0xDC00 ]),
  ],
]

