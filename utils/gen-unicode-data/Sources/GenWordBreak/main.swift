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

struct WordBreakEntry : Comparable {
  static func < (lhs: WordBreakEntry, rhs: WordBreakEntry) -> Bool {
    return lhs.index < rhs.index
  }
  
  let index: Int
  let range: ClosedRange<UInt32>
  let property: Unicode.WordBreakProperty
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
  _ data: [WordBreakEntry],
  into result: inout String
) {
  
  result += """
  #define WORD_BREAK_DATA_COUNT \(data.count)
  
  """
  
  emitCollection(
    data,
    name: "_swift_stdlib_words",
    type: "__swift_uint32_t",
    into: &result
  ) {
    var value = $0.range.lowerBound
    value |= UInt32($0.range.count) << 21
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  emitCollection(
    data,
    name: "_swift_stdlib_words_data",
    type: "__swift_uint8_t",
    into: &result
  ) {
    let value = $0.property.rawValue
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

// Main entry point into the word break generator.
func generateWordBreak() {
  var result = readFile("Input/WordData.h")
  
  let baseData = getWordBreakPropertyData(for: "Data/16/WordBreakProperty.txt")
  let emojiData = getWordBreakPropertyData(for: "Data/16/emoji-data.txt")
  
  var idx = 0
  let data = flatten(baseData + emojiData).map { (values) -> WordBreakEntry in
    idx += 1
    return WordBreakEntry(
      index: idx,
      range: values.0,
      property: values.1
    )
  }
  
  let reorderedData = eytzingerize(data, dummy: WordBreakEntry(
    index: 0,
    range: 0...0,
    property: .extend
  ))
  
  emit(reorderedData, into: &result)
  
  result += """
  #endif // #ifndef WORD_DATA_H
  
  """
  
  write(result, to: "Output/Common/WordData.h")
}

generateWordBreak()
