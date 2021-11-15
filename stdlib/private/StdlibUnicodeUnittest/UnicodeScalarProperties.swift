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

import Foundation

// Cache of opened files 
var cachedFiles: [String: String] = [:]

func readInputFile(_ index: Int) -> String {
  let file = CommandLine.arguments[index]

  do {
    guard let cache = cachedFiles[file] else {
      return try String(contentsOfFile: file, encoding: .utf8)
    }

    return cache
  } catch {
    fatalError(error.localizedDescription)
  }
}

//===----------------------------------------------------------------------===//
// Binary Properties
//===----------------------------------------------------------------------===//

// Note: If one ever updates this list, be it adding new properties, removing,
// etc., please update the same list found in:
// 'stdlib/public/core/UnicodeScalarProperties.swift'.
let availableBinaryProperties: Set<String> = [
  "Alphabetic",
  "ASCII_Hex_Digit",
  "Bidi_Control",
  "Bidi_Mirrored",
  "Cased",
  "Case_Ignorable",
  "Changes_When_Casefolded",
  "Changes_When_Casemapped",
  "Changes_When_Lowercased",
  "Changes_When_NFKC_Casefolded",
  "Changes_When_Titlecased",
  "Changes_When_Uppercased",
  "Dash",
  "Default_Ignorable_Code_Point",
  "Deprecated",
  "Diacritic",
  "Emoji",
  "Emoji_Modifier",
  "Emoji_Modifier_Base",
  "Emoji_Presentation",
  "Extender",
  "Full_Composition_Exclusion",
  "Grapheme_Base",
  "Grapheme_Extend",
  "Hex_Digit",
  "ID_Continue",
  "ID_Start",
  "Ideographic",
  "IDS_Binary_Operator",
  "IDS_Trinary_Operator",
  "Join_Control",
  "Logical_Order_Exception",
  "Lowercase",
  "Math",
  "Noncharacter_Code_Point",
  "Other_Alphabetic",
  "Other_Default_Ignorable_Code_Point",
  "Other_Grapheme_Extend",
  "Other_ID_Continue",
  "Other_ID_Start",
  "Other_Lowercase",
  "Other_Math",
  "Other_Uppercase",
  "Pattern_Syntax",
  "Pattern_White_Space",
  "Quotation_Mark",
  "Radical",
  "Sentence_Terminal",
  "Soft_Dotted",
  "Terminal_Punctuation",
  "Unified_Ideograph",
  "Uppercase",
  "Variation_Selector",
  "White_Space",
  "XID_Continue",
  "XID_Start"
]

func parseBinaryProperties(
  _ data: String,
  into result: inout [String: Set<Unicode.Scalar>]
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
    
    guard availableBinaryProperties.contains(filteredProperty) else {
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
      result[filteredProperty, default: []].insert(Unicode.Scalar(scalar)!)
    }
  }
}

// A dictionary of all currently exposed Unicode Scalar Properties. Keyed by
// the literal property name and values being the set of scalars who all conform
// to said property.
public let binaryProperties: [String: Set<Unicode.Scalar>] = {
  var result: [String: Set<Unicode.Scalar>] = [:]

  // DerivedCoreProperties.txt
  let derivedCoreProps = readInputFile(2)
  parseBinaryProperties(derivedCoreProps, into: &result)

  // DerivedNormalizationProps.txt
  let derivedNormalizationProps = readInputFile(3)
  parseBinaryProperties(derivedNormalizationProps, into: &result)

  // DerivedBinaryProperties.txt
  let derivedBinaryProperties = readInputFile(4)
  parseBinaryProperties(derivedBinaryProperties, into: &result)

  // PropList.txt
  let propList = readInputFile(5)
  parseBinaryProperties(propList, into: &result)

  // emoji-data.txt
  let emojiData = readInputFile(6)
  parseBinaryProperties(emojiData, into: &result)

  return result
}()
