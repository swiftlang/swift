//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

// WARNING: The values below must be kept in-sync with the stdlib code that
// retrieves these properties. If one should ever update this list below, be
// it reordering bits, adding new properties, etc., please update the
// stdlib code found at:
// 'stdlib/public/core/UnicodeScalarProperties.swift'.
internal struct BinProps: OptionSet {
  let rawValue: UInt64

  init(_ rawValue: UInt64) {
    self.rawValue = rawValue
  }

  // Because we defined the labelless init, we lose the memberwise one
  // generated, so define that here to satisfy the 'OptionSet' requirement.
  init(rawValue: UInt64) {
    self.rawValue = rawValue
  }
}

// swift-format-ignore
extension BinProps {
  static var changesWhenCaseFolded       : Self { Self(1 &<< 0) }
  static var changesWhenCaseMapped       : Self { Self(1 &<< 1) }
  static var changesWhenLowercased       : Self { Self(1 &<< 2) }
  static var changesWhenNFKCCaseFolded   : Self { Self(1 &<< 3) }
  static var changesWhenTitlecased       : Self { Self(1 &<< 4) }
  static var changesWhenUppercased       : Self { Self(1 &<< 5) }
  static var isASCIIHexDigit             : Self { Self(1 &<< 6) }
  static var isAlphabetic                : Self { Self(1 &<< 7) }
  static var isBidiControl               : Self { Self(1 &<< 8) }
  static var isBidiMirrored              : Self { Self(1 &<< 9) }
  static var isCaseIgnorable             : Self { Self(1 &<< 10) }
  static var isCased                     : Self { Self(1 &<< 11) }
  static var isDash                      : Self { Self(1 &<< 12) }
  static var isDefaultIgnorableCodePoint : Self { Self(1 &<< 13) }
  static var isDeprecated                : Self { Self(1 &<< 14) }
  static var isDiacritic                 : Self { Self(1 &<< 15) }
  static var isEmoji                     : Self { Self(1 &<< 16) }
  static var isEmojiModifier             : Self { Self(1 &<< 17) }
  static var isEmojiModifierBase         : Self { Self(1 &<< 18) }
  static var isEmojiPresentation         : Self { Self(1 &<< 19) }
  static var isExtender                  : Self { Self(1 &<< 20) }
  static var isFullCompositionExclusion  : Self { Self(1 &<< 21) }
  static var isGraphemeBase              : Self { Self(1 &<< 22) }
  static var isGraphemeExtend            : Self { Self(1 &<< 23) }
  static var isHexDigit                  : Self { Self(1 &<< 24) }
  static var isIDContinue                : Self { Self(1 &<< 25) }
  static var isIDSBinaryOperator         : Self { Self(1 &<< 26) }
  static var isIDSTrinaryOperator        : Self { Self(1 &<< 27) }
  static var isIDStart                   : Self { Self(1 &<< 28) }
  static var isIdeographic               : Self { Self(1 &<< 29) }
  static var isJoinControl               : Self { Self(1 &<< 30) }
  static var isLogicalOrderException     : Self { Self(1 &<< 31) }
  static var isLowercase                 : Self { Self(1 &<< 32) }
  static var isMath                      : Self { Self(1 &<< 33) }
  static var isNoncharacterCodePoint     : Self { Self(1 &<< 34) }
  static var isPatternSyntax             : Self { Self(1 &<< 35) }
  static var isPatternWhitespace         : Self { Self(1 &<< 36) }
  static var isQuotationMark             : Self { Self(1 &<< 37) }
  static var isRadical                   : Self { Self(1 &<< 38) }
  static var isSentenceTerminal          : Self { Self(1 &<< 39) }
  static var isSoftDotted                : Self { Self(1 &<< 40) }
  static var isTerminalPunctuation       : Self { Self(1 &<< 41) }
  static var isUnifiedIdeograph          : Self { Self(1 &<< 42) }
  static var isUppercase                 : Self { Self(1 &<< 43) }
  static var isVariationSelector         : Self { Self(1 &<< 44) }
  static var isWhitespace                : Self { Self(1 &<< 45) }
  static var isXIDContinue               : Self { Self(1 &<< 46) }
  static var isXIDStart                  : Self { Self(1 &<< 47) }
}

extension BinProps: Hashable {}

let binPropMappings: [String: BinProps] = [
  "Alphabetic": .isAlphabetic,
  "ASCII_Hex_Digit": .isASCIIHexDigit,
  "Bidi_Control": .isBidiControl,
  "Bidi_Mirrored": .isBidiMirrored,
  "Cased": .isCased,
  "Case_Ignorable": .isCaseIgnorable,
  "Changes_When_Casefolded": .changesWhenCaseFolded,
  "Changes_When_Casemapped": .changesWhenCaseMapped,
  "Changes_When_Lowercased": .changesWhenLowercased,
  "Changes_When_NFKC_Casefolded": .changesWhenNFKCCaseFolded,
  "Changes_When_Titlecased": .changesWhenTitlecased,
  "Changes_When_Uppercased": .changesWhenUppercased,
  "Dash": .isDash,
  "Default_Ignorable_Code_Point": .isDefaultIgnorableCodePoint,
  "Deprecated": .isDeprecated,
  "Diacritic": .isDiacritic,
  "Emoji": .isEmoji,
  "Emoji_Modifier": .isEmojiModifier,
  "Emoji_Modifier_Base": .isEmojiModifierBase,
  "Emoji_Presentation": .isEmojiPresentation,
  "Extender": .isExtender,
  "Full_Composition_Exclusion": .isFullCompositionExclusion,
  "Grapheme_Base": .isGraphemeBase,
  "Grapheme_Extend": .isGraphemeExtend,
  "Hex_Digit": .isHexDigit,
  "ID_Continue": .isIDContinue,
  "ID_Start": .isIDStart,
  "Ideographic": .isIdeographic,
  "IDS_Binary_Operator": .isIDSBinaryOperator,
  "IDS_Trinary_Operator": .isIDSTrinaryOperator,
  "Join_Control": .isJoinControl,
  "Logical_Order_Exception": .isLogicalOrderException,
  "Lowercase": .isLowercase,
  "Math": .isMath,
  "Noncharacter_Code_Point": .isNoncharacterCodePoint,
  "Other_Alphabetic": .isAlphabetic,
  "Other_Default_Ignorable_Code_Point": .isDefaultIgnorableCodePoint,
  "Other_Grapheme_Extend": .isGraphemeExtend,
  "Other_ID_Continue": .isIDContinue,
  "Other_ID_Start": .isIDStart,
  "Other_Lowercase": .isLowercase,
  "Other_Math": .isMath,
  "Other_Uppercase": .isUppercase,
  "Pattern_Syntax": .isPatternSyntax,
  "Pattern_White_Space": .isPatternWhitespace,
  "Quotation_Mark": .isQuotationMark,
  "Radical": .isRadical,
  "Sentence_Terminal": .isSentenceTerminal,
  "Soft_Dotted": .isSoftDotted,
  "Terminal_Punctuation": .isTerminalPunctuation,
  "Unified_Ideograph": .isUnifiedIdeograph,
  "Uppercase": .isUppercase,
  "Variation_Selector": .isVariationSelector,
  "White_Space": .isWhitespace,
  "XID_Continue": .isXIDContinue,
  "XID_Start": .isXIDStart,
]

func getBinaryProperties(
  from data: String,
  into result: inout [UInt32: BinProps]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    // Get the property first because we may not care about it.
    let filteredProperty = components[1].filter { !$0.isWhitespace }

    guard binPropMappings.keys.contains(filteredProperty) else {
      continue
    }

    let scalars: ClosedRange<UInt32>

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")

      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!

      scalars = scalar ... scalar
    }

    for scalar in scalars {
      result[scalar, default: []].insert(binPropMappings[filteredProperty]!)
    }
  }
}

func emitBinaryProps(
  _ data: [(ClosedRange<UInt32>, BinProps)],
  into result: inout String
) {
  result += """
    #define BIN_PROPS_COUNT \(data.count)


    """

  let combinations = Array(Set(data.map { $0.1 })).map { $0.rawValue }

  // Data combinations array

  emitCollection(
    combinations,
    name: "_swift_stdlib_scalar_binProps_data",
    into: &result
  )

  // Actual scalar + index array

  emitCollection(
    data,
    name: "_swift_stdlib_scalar_binProps",
    type: "__swift_uint32_t",
    into: &result
  ) {
    var value = $0.0.lowerBound
    value |= UInt32(combinations.firstIndex(of: $0.1.rawValue)!) << 21
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func generateBinaryProps(for platform: String, into result: inout String) {
  let derivedCoreProps: String

  switch platform {
  case "Apple":
    derivedCoreProps = readFile("Data/16/Apple/DerivedCoreProperties.txt")
  default:
    derivedCoreProps = readFile("Data/16/DerivedCoreProperties.txt")
  }

  let bidiMirrored = readFile("Data/16/DerivedBinaryProperties.txt")
  let normalization = readFile("Data/16/DerivedNormalizationProps.txt")
  let emoji = readFile("Data/16/emoji-data.txt")
  let propList = readFile("Data/16/PropList.txt")

  var binProps: [UInt32: BinProps] = [:]
  getBinaryProperties(from: derivedCoreProps, into: &binProps)
  getBinaryProperties(from: bidiMirrored, into: &binProps)
  getBinaryProperties(from: normalization, into: &binProps)
  getBinaryProperties(from: emoji, into: &binProps)
  getBinaryProperties(from: propList, into: &binProps)

  // This loop inserts the ranges of scalars who have no binary properties to
  // fill in the holes for binary search.
  for i in 0x0 ... 0x10FFFF {
    guard let scalar = Unicode.Scalar(i) else {
      continue
    }

    if !binProps.keys.contains(scalar.value) {
      binProps[scalar.value] = []
    }
  }

  let data = flatten(Array(binProps))

  emitBinaryProps(data, into: &result)
}
