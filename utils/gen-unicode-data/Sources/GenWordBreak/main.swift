//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

extension Unicode {
  enum WordBreakProperty: UInt8 {
    // We don't store the other properties, so we really don't care about them
    // here.
    
    case extend = 0
    case format = 1
    case katakana = 2
    case hebrewLetter = 3
    case aLetter = 4
    case midNumLet = 5
    case midLetter = 6
    case midNum = 7
    case numeric = 8
    case extendNumLet = 9
    case wSegSpace = 10
    case extendedPictographic = 11
    
    init?(_ str: String) {
      switch str {
      case "Extend":
        self = .extend
      case "Format":
        self = .format
      case "Katakana":
        self = .katakana
      case "Hebrew_Letter":
        self = .hebrewLetter
      case "ALetter":
        self = .aLetter
      case "MidNumLet":
        self = .midNumLet
      case "MidLetter":
        self = .midLetter
      case "MidNum":
        self = .midNum
      case "Numeric":
        self = .numeric
      case "ExtendNumLet":
        self = .extendNumLet
      case "WSegSpace":
        self = .wSegSpace
      case "Extended_Pictographic":
        self = .extendedPictographic
      default:
        return nil
      }
    }
  }
}

func getWordBreakPropertyData(
  for path: String
) -> [(ClosedRange<UInt32>, Unicode.WordBreakProperty)] {
  let data = readFile(path)
  
  var unflattened: [(ClosedRange<UInt32>, Unicode.WordBreakProperty)] = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    // Each line in this file is broken up into two sections:
    // 1: Either the singular scalar or a range of scalars who conform to said
    //    grapheme break property.
    // 2: The grapheme break property that said scalar(s) conform to (with
    //    additional comments noting the character category, name and amount of
    //    scalars the range represents).
    let components = line.split(separator: ";")
    
    // Get the property first because it may be one we don't care about.
    let splitProperty = components[1].split(separator: "#")
    let filteredProperty = splitProperty[0].filter { !$0.isWhitespace }
    
    guard let gbp = Unicode.WordBreakProperty(filteredProperty) else {
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
    
    unflattened.append((scalars, gbp))
  }
  
  return flatten(unflattened)
}

func emit(
  _ data: [(ClosedRange<UInt32>, Unicode.WordBreakProperty)],
  into result: inout String
) {
  emitCollection(
    data,
    name: "_swift_stdlib_words",
    type: "__swift_uint32_t",
    into: &result
  ) {
    var value = $0.0.lowerBound
    value |= UInt32($0.0.count) << 21
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  emitCollection(
    data,
    name: "_swift_stdlib_words_data",
    type: "__swift_uint8_t",
    into: &result
  ) {
    let value = $0.1.rawValue
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

// Main entry point into the grapheme break property generator.
func generateGraphemeBreakProperty() {
  var result = readFile("Input/WordData.h")
  
  let baseData = getWordBreakPropertyData(for: "Data/15/WordBreakProperty.txt")
  let emojiData = getWordBreakPropertyData(for: "Data/15/emoji-data.txt")
  
  let data = flatten(baseData + emojiData)
  
  emit(data, into: &result)
  
  result += """
  #endif // #ifndef WORD_DATA_H
  
  """
  
  write(result, to: "Output/Common/WordData.h")
}

generateGraphemeBreakProperty()
