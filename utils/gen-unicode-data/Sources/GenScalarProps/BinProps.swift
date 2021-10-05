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

struct BinProps: OptionSet {
  var rawValue: UInt64
  
  static let changesWhenCaseFolded       = BinProps(rawValue: 1 << 0)
  static let changesWhenCaseMapped       = BinProps(rawValue: 1 << 1)
  static let changesWhenLowercased       = BinProps(rawValue: 1 << 2)
  static let changesWhenNFKCCaseFolded   = BinProps(rawValue: 1 << 3)
  static let changesWhenTitlecased       = BinProps(rawValue: 1 << 4)
  static let changesWhenUppercased       = BinProps(rawValue: 1 << 5)
  static let isASCIIHexDigit             = BinProps(rawValue: 1 << 6)
  static let isAlphabetic                = BinProps(rawValue: 1 << 7)
  static let isBidiControl               = BinProps(rawValue: 1 << 8)
  static let isBidiMirrored              = BinProps(rawValue: 1 << 9)
  static let isCaseIgnorable             = BinProps(rawValue: 1 << 10)
  static let isCased                     = BinProps(rawValue: 1 << 11)
  static let isDash                      = BinProps(rawValue: 1 << 12)
  static let isDefaultIgnorableCodePoint = BinProps(rawValue: 1 << 13)
  static let isDeprecated                = BinProps(rawValue: 1 << 14)
  static let isDiacritic                 = BinProps(rawValue: 1 << 15)
  static let isEmoji                     = BinProps(rawValue: 1 << 16)
  static let isEmojiModifier             = BinProps(rawValue: 1 << 17)
  static let isEmojiModifierBase         = BinProps(rawValue: 1 << 18)
  static let isEmojiPresentation         = BinProps(rawValue: 1 << 19)
  static let isExtender                  = BinProps(rawValue: 1 << 20)
  static let isFullCompositionExclusion  = BinProps(rawValue: 1 << 21)
  static let isGraphemeBase              = BinProps(rawValue: 1 << 22)
  static let isGraphemeExtend            = BinProps(rawValue: 1 << 23)
  static let isHexDigit                  = BinProps(rawValue: 1 << 24)
  static let isIDContinue                = BinProps(rawValue: 1 << 25)
  static let isIDSBinaryOperator         = BinProps(rawValue: 1 << 26)
  static let isIDSTrinaryOperator        = BinProps(rawValue: 1 << 27)
  static let isIDStart                   = BinProps(rawValue: 1 << 28)
  static let isIdeographic               = BinProps(rawValue: 1 << 29)
  static let isJoinControl               = BinProps(rawValue: 1 << 30)
  static let isLogicalOrderException     = BinProps(rawValue: 1 << 31)
  static let isLowercase                 = BinProps(rawValue: 1 << 32)
  static let isMath                      = BinProps(rawValue: 1 << 33)
  static let isNoncharacterCodePoint     = BinProps(rawValue: 1 << 34)
  static let isPatternSyntax             = BinProps(rawValue: 1 << 35)
  static let isPatternWhitespace         = BinProps(rawValue: 1 << 36)
  static let isQuotationMark             = BinProps(rawValue: 1 << 37)
  static let isRadical                   = BinProps(rawValue: 1 << 38)
  static let isSentenceTerminal          = BinProps(rawValue: 1 << 39)
  static let isSoftDotted                = BinProps(rawValue: 1 << 40)
  static let isTerminalPunctuation       = BinProps(rawValue: 1 << 41)
  static let isUnifiedIdeograph          = BinProps(rawValue: 1 << 42)
  static let isUppercase                 = BinProps(rawValue: 1 << 43)
  static let isVariationSelector         = BinProps(rawValue: 1 << 44)
  static let isWhitespace                = BinProps(rawValue: 1 << 45)
  static let isXIDContinue               = BinProps(rawValue: 1 << 46)
  static let isXIDStart                  = BinProps(rawValue: 1 << 47)
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
  "XID_Start": .isXIDStart
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
  let combinations = Array(Set(data.map { $0.1 })).map { $0.rawValue }
  
  // Data combinations array
  
  emitCollection(
    combinations,
    name: "_swift_stdlib_scalar_binProp_data",
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

func emitBinaryPropAccessor(_ dataCount: Int, into result: inout String) {
  result += """
  SWIFT_RUNTIME_STDLIB_INTERNAL
  __swift_bool _swift_stdlib_hasBinaryProperty(__swift_uint32_t scalar,
                                               __swift_intptr_t propertyMask) {
  
    __swift_uint64_t binaryProperties = 0;
  
    auto low = 0;
    auto high = \(dataCount) - 1;

    while (high >= low) {
      auto idx = low + (high - low) / 2;

      auto entry = _swift_stdlib_scalar_binProps[idx];

      // Shift the ccc value out of the scalar.
      auto lower = (entry << 11) >> 11;

      __swift_uint32_t upper = 0;
      
      // If we're not at the end of the array, the range count is simply the
      // distance to the next element.
      if (idx != \(dataCount) - 1) {
        auto nextEntry = _swift_stdlib_scalar_binProps[idx + 1];
  
        auto nextLower = (nextEntry << 11) >> 11;
        
        upper = nextLower - 1;
      } else {
        // Otherwise, the range count is the distance to 0x10FFFF
        upper = 0x10FFFF;
      }

      // Shift everything out.
      auto dataIdx = entry >> 21;

      if (scalar >= lower && scalar <= upper) {
        binaryProperties =  _swift_stdlib_scalar_binProps_data[dataIdx];
        break;
      }

      if (scalar > upper) {
        low = idx + 1;
        continue;
      }

      if (scalar < lower) {
        high = idx - 1;
        continue;
      }
    }

    return binaryProperties & propertyMask;
  }
  
  
  """
}

func generateBinaryProps(into result: inout String) {
  let derivedCoreProps = readFile("Data/DerivedCoreProperties.txt")
  let bidiMirrored = readFile("Data/DerivedBinaryProperties.txt")
  let normalization = readFile("Data/DerivedNormalizationProps.txt")
  let emoji = readFile("Data/emoji-data.txt")
  let propList = readFile("Data/PropList.txt")
  
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
  emitBinaryPropAccessor(data.count, into: &result)
}
