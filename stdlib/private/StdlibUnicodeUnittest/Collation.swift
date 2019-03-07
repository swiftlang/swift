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

struct CollationTableEntry {
  let scalars: [Unicode.Scalar]
  let collationElements: [UInt64]
  let comment: String

  init(
    _ scalars: [UInt32],
    _ collationElements: [UInt64],
    _ comment: String
  ) {
    self.scalars = scalars.map { Unicode.Scalar($0)! }
    self.collationElements = collationElements
    self.comment = comment
  }
}

/// An excerpt from the DUCET (Default Unicode Collation Element Table).
///
/// The data was extracted from
/// http://www.unicode.org/Public/UCA/9.0.0/allkeys.txt.
let ducetExtractData: [CollationTableEntry] = [
  CollationTableEntry([0x00], [0x0000_0000_0000], "NULL"),
  CollationTableEntry([0x01], [0x0000_0000_0000], "START OF HEADING"),
  CollationTableEntry([0x02], [0x0000_0000_0000], "START OF TEXT"),
  CollationTableEntry([0x03], [0x0000_0000_0000], "END OF TEXT"),
  CollationTableEntry([0x04], [0x0000_0000_0000], "END OF TRANSMISSION"),
  CollationTableEntry([0x05], [0x0000_0000_0000], "ENQUIRY"),
  CollationTableEntry([0x06], [0x0000_0000_0000], "ACKNOWLEDGE"),
  CollationTableEntry([0x07], [0x0000_0000_0000], "BELL"),
  CollationTableEntry([0x08], [0x0000_0000_0000], "BACKSPACE"),
  CollationTableEntry([0x09], [0x0201_0020_0002], "HORIZONTAL TABULATION"),
  CollationTableEntry([0x0A], [0x0202_0020_0002], "LINE FEED"),
  CollationTableEntry([0x0B], [0x0203_0020_0002], "VERTICAL TABULATION"),
  CollationTableEntry([0x0C], [0x0204_0020_0002], "FORM FEED"),
  CollationTableEntry([0x0D], [0x0205_0020_0002], "CARRIAGE RETURN"),
  CollationTableEntry([0x0E], [0x0000_0000_0000], "SHIFT OUT"),
  CollationTableEntry([0x0F], [0x0000_0000_0000], "SHIFT IN"),
  CollationTableEntry([0x10], [0x0000_0000_0000], "DATA LINK ESCAPE"),
  CollationTableEntry([0x11], [0x0000_0000_0000], "DEVICE CONTROL ONE"),
  CollationTableEntry([0x12], [0x0000_0000_0000], "DEVICE CONTROL TWO"),
  CollationTableEntry([0x13], [0x0000_0000_0000], "DEVICE CONTROL THREE"),
  CollationTableEntry([0x14], [0x0000_0000_0000], "DEVICE CONTROL FOUR"),
  CollationTableEntry([0x15], [0x0000_0000_0000], "NEGATIVE ACKNOWLEDGE"),
  CollationTableEntry([0x16], [0x0000_0000_0000], "SYNCHRONOUS IDLE"),
  CollationTableEntry([0x17], [0x0000_0000_0000], "END OF TRANSMISSION BLOCK"),
  CollationTableEntry([0x18], [0x0000_0000_0000], "CANCEL"),
  CollationTableEntry([0x19], [0x0000_0000_0000], "END OF MEDIUM"),
  CollationTableEntry([0x1A], [0x0000_0000_0000], "SUBSTITUTE"),
  CollationTableEntry([0x1B], [0x0000_0000_0000], "ESCAPE"),
  CollationTableEntry([0x1C], [0x0000_0000_0000], "FILE SEPARATOR"),
  CollationTableEntry([0x1D], [0x0000_0000_0000], "GROUP SEPARATOR"),
  CollationTableEntry([0x1E], [0x0000_0000_0000], "RECORD SEPARATOR"),
  CollationTableEntry([0x1F], [0x0000_0000_0000], "UNIT SEPARATOR"),
  CollationTableEntry([0x20], [0x0209_0020_0002], "SPACE"),
  CollationTableEntry([0x21], [0x0260_0020_0002], "EXCLAMATION MARK"),
  CollationTableEntry([0x22], [0x030C_0020_0002], "QUOTATION MARK"),
  CollationTableEntry([0x23], [0x0398_0020_0002], "NUMBER SIGN"),
  CollationTableEntry([0x24], [0x1C12_0020_0002], "DOLLAR SIGN"),
  CollationTableEntry([0x25], [0x0399_0020_0002], "PERCENT SIGN"),
  CollationTableEntry([0x26], [0x0396_0020_0002], "AMPERSAND"),
  CollationTableEntry([0x27], [0x0305_0020_0002], "APOSTROPHE"),
  CollationTableEntry([0x28], [0x0317_0020_0002], "LEFT PARENTHESIS"),
  CollationTableEntry([0x29], [0x0318_0020_0002], "RIGHT PARENTHESIS"),
  CollationTableEntry([0x2A], [0x038F_0020_0002], "ASTERISK"),
  CollationTableEntry([0x2B], [0x0616_0020_0002], "PLUS SIGN"),
  CollationTableEntry([0x2C], [0x0222_0020_0002], "COMMA"),
  CollationTableEntry([0x2D], [0x020D_0020_0002], "HYPHEN-MINUS"),
  CollationTableEntry([0x2E], [0x0277_0020_0002], "FULL STOP"),
  CollationTableEntry([0x2F], [0x0394_0020_0002], "SOLIDUS"),
  CollationTableEntry([0x30], [0x1C3D_0020_0002], "DIGIT ZERO"),
  CollationTableEntry([0x31], [0x1C3E_0020_0002], "DIGIT ONE"),
  CollationTableEntry([0x32], [0x1C3F_0020_0002], "DIGIT TWO"),
  CollationTableEntry([0x33], [0x1C40_0020_0002], "DIGIT THREE"),
  CollationTableEntry([0x34], [0x1C41_0020_0002], "DIGIT FOUR"),
  CollationTableEntry([0x35], [0x1C42_0020_0002], "DIGIT FIVE"),
  CollationTableEntry([0x36], [0x1C43_0020_0002], "DIGIT SIX"),
  CollationTableEntry([0x37], [0x1C44_0020_0002], "DIGIT SEVEN"),
  CollationTableEntry([0x38], [0x1C45_0020_0002], "DIGIT EIGHT"),
  CollationTableEntry([0x39], [0x1C46_0020_0002], "DIGIT NINE"),
  CollationTableEntry([0x3A], [0x0239_0020_0002], "COLON"),
  CollationTableEntry([0x3B], [0x0234_0020_0002], "SEMICOLON"),
  CollationTableEntry([0x3C], [0x061A_0020_0002], "LESS-THAN SIGN"),
  CollationTableEntry([0x3D], [0x061B_0020_0002], "EQUALS SIGN"),
  CollationTableEntry([0x3E], [0x061C_0020_0002], "GREATER-THAN SIGN"),
  CollationTableEntry([0x3F], [0x0266_0020_0002], "QUESTION MARK"),
  CollationTableEntry([0x40], [0x038E_0020_0002], "COMMERCIAL AT"),
  CollationTableEntry([0x41], [0x1C47_0020_0008], "LATIN CAPITAL LETTER A"),
  CollationTableEntry([0x42], [0x1C60_0020_0008], "LATIN CAPITAL LETTER B"),
  CollationTableEntry([0x43], [0x1C7A_0020_0008], "LATIN CAPITAL LETTER C"),
  CollationTableEntry([0x44], [0x1C8F_0020_0008], "LATIN CAPITAL LETTER D"),
  CollationTableEntry([0x45], [0x1CAA_0020_0008], "LATIN CAPITAL LETTER E"),
  CollationTableEntry([0x46], [0x1CE5_0020_0008], "LATIN CAPITAL LETTER F"),
  CollationTableEntry([0x47], [0x1CF4_0020_0008], "LATIN CAPITAL LETTER G"),
  CollationTableEntry([0x48], [0x1D18_0020_0008], "LATIN CAPITAL LETTER H"),
  CollationTableEntry([0x49], [0x1D32_0020_0008], "LATIN CAPITAL LETTER I"),
  CollationTableEntry([0x4A], [0x1D4C_0020_0008], "LATIN CAPITAL LETTER J"),
  CollationTableEntry([0x4B], [0x1D65_0020_0008], "LATIN CAPITAL LETTER K"),
  CollationTableEntry([0x4C], [0x1D77_0020_0008], "LATIN CAPITAL LETTER L"),
  CollationTableEntry([0x4D], [0x1DAA_0020_0008], "LATIN CAPITAL LETTER M"),
  CollationTableEntry([0x4E], [0x1DB9_0020_0008], "LATIN CAPITAL LETTER N"),
  CollationTableEntry([0x4F], [0x1DDD_0020_0008], "LATIN CAPITAL LETTER O"),
  CollationTableEntry([0x50], [0x1E0C_0020_0008], "LATIN CAPITAL LETTER P"),
  CollationTableEntry([0x51], [0x1E21_0020_0008], "LATIN CAPITAL LETTER Q"),
  CollationTableEntry([0x52], [0x1E33_0020_0008], "LATIN CAPITAL LETTER R"),
  CollationTableEntry([0x53], [0x1E71_0020_0008], "LATIN CAPITAL LETTER S"),
  CollationTableEntry([0x54], [0x1E95_0020_0008], "LATIN CAPITAL LETTER T"),
  CollationTableEntry([0x55], [0x1EB5_0020_0008], "LATIN CAPITAL LETTER U"),
  CollationTableEntry([0x56], [0x1EE3_0020_0008], "LATIN CAPITAL LETTER V"),
  CollationTableEntry([0x57], [0x1EF5_0020_0008], "LATIN CAPITAL LETTER W"),
  CollationTableEntry([0x58], [0x1EFF_0020_0008], "LATIN CAPITAL LETTER X"),
  CollationTableEntry([0x59], [0x1F0B_0020_0008], "LATIN CAPITAL LETTER Y"),
  CollationTableEntry([0x5A], [0x1F21_0020_0008], "LATIN CAPITAL LETTER Z"),
  CollationTableEntry([0x5B], [0x0319_0020_0002], "LEFT SQUARE BRACKET"),
  CollationTableEntry([0x5C], [0x0395_0020_0002], "REVERSE SOLIDUS"),
  CollationTableEntry([0x5D], [0x031A_0020_0002], "RIGHT SQUARE BRACKET"),
  CollationTableEntry([0x5E], [0x0485_0020_0002], "CIRCUMFLEX ACCENT"),
  CollationTableEntry([0x5F], [0x020B_0020_0002], "LOW LINE"),
  CollationTableEntry([0x60], [0x0482_0020_0002], "GRAVE ACCENT"),
  CollationTableEntry([0x61], [0x1C47_0020_0002], "LATIN SMALL LETTER A"),
  CollationTableEntry([0x62], [0x1C60_0020_0002], "LATIN SMALL LETTER B"),
  CollationTableEntry([0x63], [0x1C7A_0020_0002], "LATIN SMALL LETTER C"),
  CollationTableEntry([0x64], [0x1C8F_0020_0002], "LATIN SMALL LETTER D"),
  CollationTableEntry([0x65], [0x1CAA_0020_0002], "LATIN SMALL LETTER E"),
  CollationTableEntry([0x66], [0x1CE5_0020_0002], "LATIN SMALL LETTER F"),
  CollationTableEntry([0x67], [0x1CF4_0020_0002], "LATIN SMALL LETTER G"),
  CollationTableEntry([0x68], [0x1D18_0020_0002], "LATIN SMALL LETTER H"),
  CollationTableEntry([0x69], [0x1D32_0020_0002], "LATIN SMALL LETTER I"),
  CollationTableEntry([0x6A], [0x1D4C_0020_0002], "LATIN SMALL LETTER J"),
  CollationTableEntry([0x6B], [0x1D65_0020_0002], "LATIN SMALL LETTER K"),
  CollationTableEntry([0x6C], [0x1D77_0020_0002], "LATIN SMALL LETTER L"),
  CollationTableEntry([0x6D], [0x1DAA_0020_0002], "LATIN SMALL LETTER M"),
  CollationTableEntry([0x6E], [0x1DB9_0020_0002], "LATIN SMALL LETTER N"),
  CollationTableEntry([0x6F], [0x1DDD_0020_0002], "LATIN SMALL LETTER O"),
  CollationTableEntry([0x70], [0x1E0C_0020_0002], "LATIN SMALL LETTER P"),
  CollationTableEntry([0x71], [0x1E21_0020_0002], "LATIN SMALL LETTER Q"),
  CollationTableEntry([0x72], [0x1E33_0020_0002], "LATIN SMALL LETTER R"),
  CollationTableEntry([0x73], [0x1E71_0020_0002], "LATIN SMALL LETTER S"),
  CollationTableEntry([0x74], [0x1E95_0020_0002], "LATIN SMALL LETTER T"),
  CollationTableEntry([0x75], [0x1EB5_0020_0002], "LATIN SMALL LETTER U"),
  CollationTableEntry([0x76], [0x1EE3_0020_0002], "LATIN SMALL LETTER V"),
  CollationTableEntry([0x77], [0x1EF5_0020_0002], "LATIN SMALL LETTER W"),
  CollationTableEntry([0x78], [0x1EFF_0020_0002], "LATIN SMALL LETTER X"),
  CollationTableEntry([0x79], [0x1F0B_0020_0002], "LATIN SMALL LETTER Y"),
  CollationTableEntry([0x7A], [0x1F21_0020_0002], "LATIN SMALL LETTER Z"),
  CollationTableEntry([0x7B], [0x031B_0020_0002], "LEFT CURLY BRACKET"),
  CollationTableEntry([0x7C], [0x061E_0020_0002], "VERTICAL LINE"),
  CollationTableEntry([0x7D], [0x031C_0020_0002], "RIGHT CURLY BRACKET"),
  CollationTableEntry([0x7E], [0x0620_0020_0002], "TILDE"),
  CollationTableEntry([0x7F], [0x0000_0000_0000], "DELETE"),

  // When String starts to use Latin-1 as one of its in-memory representations,
  // this table should be extended to cover all scalars in U+0080 ... U+00FF.
  CollationTableEntry([0x80], [0x0000_0000_0000], "<control>"),
  CollationTableEntry([0xE1], [0x1C47_0020_0002, 0x0000_0024_0002], "LATIN SMALL LETTER A WITH ACUTE"),
  CollationTableEntry([0xE2], [0x1C47_0020_0002, 0x0000_0027_0002], "LATIN SMALL LETTER A WITH CIRCUMFLEX"),
  CollationTableEntry([0xFF], [0x1F0B_0020_0002, 0x0000_002B_0002], "LATIN SMALL LETTER Y WITH DIAERESIS"),

  CollationTableEntry([0x3041], [0x3D5A_0020_000D], "HIRAGANA LETTER SMALL A"),
  CollationTableEntry([0x3042], [0x3D5A_0020_000E], "HIRAGANA LETTER A"),
  CollationTableEntry([0x30A1], [0x3D5A_0020_000F], "KATAKANA LETTER SMALL A"),
  CollationTableEntry([0xFF67], [0x3D5A_0020_0010], "HALFWIDTH KATAKANA LETTER SMALL A"),
  CollationTableEntry([0x30A2], [0x3D5A_0020_0011], "KATAKANA LETTER A"),
  CollationTableEntry([0xFF71], [0x3D5A_0020_0012], "HALFWIDTH KATAKANA LETTER A"),
  CollationTableEntry([0xFE00], [0x0000_0000_0000], "VARIATION SELECTOR-1"),
  CollationTableEntry([0xFE01], [0x0000_0000_0000], "VARIATION SELECTOR-2"),
  CollationTableEntry([0xE01EE], [0x0000_0000_0000], "VARIATION SELECTOR-255"),
  CollationTableEntry([0xE01EF], [0x0000_0000_0000], "VARIATION SELECTOR-256"),
]

let ducetExtract: [[Unicode.Scalar]: CollationTableEntry] = {
  () in
  var result: [[Unicode.Scalar]: CollationTableEntry] = [:]
  for entry in ducetExtractData {
    result[entry.scalars] = entry
  }
  return result
}()

extension String {
  /// Calculate collation elements for trivial cases.
  ///
  /// Warning: this implementation does not conform to Unicode TR10!
  /// It is a gross oversimplification that is only used to reduce the repetition
  /// of test inputs in this file.  Among other things, this algorithm does not
  /// handle contractions in the collation table, does not perform string
  /// normalization, does not synthesize derived collation weights etc.
  internal var _collationElements: [UInt64] {
    var result: [UInt64] = []
    for us in self.unicodeScalars {
      let scalars: [Unicode.Scalar] = [us]
      let collationElements = ducetExtract[scalars]!.collationElements
      if collationElements[0] != 0 {
        result += collationElements
      }
    }
    return result
  }
}

public struct StringComparisonTest {
  public let string: String
  public let collationElements: [UInt64]
  public let loc: SourceLoc

  public var order: Int?

  public init(
    _ string: String,
    inferCollationElements: Void,
    file: String = #file, line: UInt = #line
  ) {
    self.string = string
    self.collationElements = string._collationElements
    self.loc = SourceLoc(file, line, comment: "test data")
  }

  public init(
    _ string: String,
    _ collationElements: [UInt64],
    sourceLocation: SourceLoc
  ) {
    self.string = string
    self.collationElements = collationElements
    self.loc = sourceLocation
  }

  public init(
    _ string: String,
    _ collationElements: [UInt64],
    file: String = #file, line: UInt = #line
  ) {
    self.init(
      string,
      collationElements,
      sourceLocation: SourceLoc(file, line, comment: "test data"))
  }

  public static let testsFromDUCET: [StringComparisonTest] = {
    () in
    var result: [StringComparisonTest] = []
    for entry in ducetExtractData {
      var s = ""
      for c in entry.scalars {
        s.append(Character(c))
      }
      if entry.collationElements[0] != 0 {
        result.append(StringComparisonTest(s, entry.collationElements))
      }
    }
    return result
  }()

  public static let hardcodedTests: [StringComparisonTest] = [
    StringComparisonTest("", inferCollationElements: ()),

    // Completely ignorable characters in ASCII strings.
    StringComparisonTest("\u{00}\u{61}", inferCollationElements: ()),
    StringComparisonTest("\u{61}\u{00}", inferCollationElements: ()),
    StringComparisonTest("\u{00}\u{61}\u{00}", inferCollationElements: ()),
    StringComparisonTest("\u{61}\u{00}\u{62}", inferCollationElements: ()),

    // Completely ignorable characters in Latin-1 strings.
    StringComparisonTest("\u{00}\u{E1}", inferCollationElements: ()),
    StringComparisonTest("\u{E1}\u{00}", inferCollationElements: ()),
    StringComparisonTest("\u{00}\u{E1}\u{00}", inferCollationElements: ()),
    StringComparisonTest("\u{E1}\u{00}\u{E2}", inferCollationElements: ()),

    // Completely ignorable characters in non-Latin-1 strings.
    StringComparisonTest("\u{0000}\u{3041}", inferCollationElements: ()),
    StringComparisonTest("\u{3041}\u{0000}", inferCollationElements: ()),
    StringComparisonTest("\u{0000}\u{3041}\u{0000}", inferCollationElements: ()),
    StringComparisonTest("\u{3041}\u{0000}\u{3042}", inferCollationElements: ()),
    StringComparisonTest("\u{FE00}\u{3041}", inferCollationElements: ()),
    StringComparisonTest("\u{3041}\u{FE00}", inferCollationElements: ()),
    StringComparisonTest("\u{FE00}\u{3041}\u{FE00}", inferCollationElements: ()),
    StringComparisonTest("\u{3041}\u{FE00}\u{3042}", inferCollationElements: ()),
    StringComparisonTest("\u{E01EF}\u{3041}", inferCollationElements: ()),
    StringComparisonTest("\u{03041}\u{E01EF}", inferCollationElements: ()),
    StringComparisonTest("\u{E01EF}\u{03041}\u{E01EF}", inferCollationElements: ()),
    StringComparisonTest("\u{03041}\u{E01EF}\u{03042}", inferCollationElements: ()),

    // U+0061 LATIN SMALL LETTER A
    // U+0301 COMBINING ACUTE ACCENT
    // U+00E1 LATIN SMALL LETTER A WITH ACUTE
    StringComparisonTest("\u{61}\u{301}", "\u{E1}"._collationElements),
  ]

  public static let allTests: [StringComparisonTest] = {
    () in
    return testsFromDUCET + hardcodedTests
  }()
}

public func sortKey(forCollationElements ces: [UInt64]) -> ([UInt16], [UInt16], [UInt16]) {
  func L1(_ ce: UInt64) -> UInt16 {
    return UInt16(truncatingIfNeeded: ce &>> 32)
  }
  func L2(_ ce: UInt64) -> UInt16 {
    return UInt16(truncatingIfNeeded: ce &>> 16)
  }
  func L3(_ ce: UInt64) -> UInt16 {
    return UInt16(truncatingIfNeeded: ce)
  }

  var result1: [UInt16] = []
  for ce in ces { result1.append(L1(ce)) }

  var result2: [UInt16] = []
  for ce in ces { result2.append(L2(ce)) }

  var result3: [UInt16] = []
  for ce in ces { result3.append(L3(ce)) }

  return (result1, result2, result3)
}

public func collationElements(
  _ lhs: [UInt64], areLessThan rhs: [UInt64]
) -> Bool {
  let lhsKey = sortKey(forCollationElements: lhs)
  let rhsKey = sortKey(forCollationElements: rhs)

  if lhsKey.0 != rhsKey.0 {
    return lhsKey.0.lexicographicallyPrecedes(rhsKey.0)
  }
  if lhsKey.1 != rhsKey.1 {
    return lhsKey.1.lexicographicallyPrecedes(rhsKey.1)
  }
  return lhsKey.2.lexicographicallyPrecedes(rhsKey.2)
}
