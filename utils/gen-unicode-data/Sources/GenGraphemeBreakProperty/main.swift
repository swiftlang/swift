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

extension Unicode {
  enum GraphemeBreakProperty: UInt32 {
    // We don't store the other properties, so we really don't care about them
    // here.

    case control = 0
    case extend = 1
    case prepend = 2
    case spacingMark = 3
    case extendedPictographic = 4

    init?(_ str: String) {
      switch str {
      case "Extend":
        self = .extend

      // Although CR and LF are distinct properties, we have fast paths in place
      // for those cases, so combine them here to allow for more contiguous
      // ranges.
      case "Control",
           "CR",
           "LF":
        self = .control
      case "Prepend":
        self = .prepend
      case "SpacingMark":
        self = .spacingMark
      case "Extended_Pictographic":
        self = .extendedPictographic
      default:
        return nil
      }
    }
  }
}

// Given a path to one of the Unicode data files, reads it and returns the
// unflattened list of scalar & grapheme break property.
//
// Each line in one of these data files is formatted like the following:
//
//     007F..009F    ; Control # Cc  [33] <control-007F>..<control-009F>
//     00AD          ; Control # Cf       SOFT HYPHEN
//
// Where each section is split by a ';'. The first section informs us of either
// the range of scalars who conform to this property or the singular scalar
// who does. The second section tells us what grapheme break property these
// scalars conform to. There are extra comments telling us what general
// category these scalars are a part of, how many scalars are in a range, and
// the name of the scalars.
func getGraphemeBreakPropertyData(
  for path: String
) -> [(ClosedRange<UInt32>, Unicode.GraphemeBreakProperty)] {
  let data = readFile(path)

  var unflattened: [(ClosedRange<UInt32>, Unicode.GraphemeBreakProperty)] = []

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

    guard let gbp = Unicode.GraphemeBreakProperty(filteredProperty) else {
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

  return unflattened
}

// Takes the flattened data and writes it as a static C array.
func emit(
  _ data: [(ClosedRange<UInt32>, Unicode.GraphemeBreakProperty)],
  into result: inout String
) {
  result += """
  #define GRAPHEME_BREAK_DATA_COUNT \(data.count)
  
  static const __swift_uint32_t _swift_stdlib_graphemeBreakProperties[\(data.count)] = {

  """

  formatCollection(data, into: &result) { (range, gbp) -> String in
    // Our value uses the 21 bits to represent the scalar, 8 bits to represent
    // the range's count, and finally the last three bits to represent the
    // grapheme break property enum.

    // E.g. for the top 3 bits:
    // 000 = control
    // 001 = extend
    // 010 = prepend
    // 011 = spacingMark
    // 100 = extendedPictographic (not using the extra range bit)
    // 101 = extendedPictographic (using the extra range bit)
    // 110 = not used (will never occur)
    // 111 = not used (will never occur)

    var value = range.lowerBound
    value |= UInt32(range.count - 1) << 21
    value |= gbp.rawValue << 29

    // However, for extendedPictographic because we don't use second to top bit,
    // we can use an extra bit for the range value to store more.
    if gbp == .extendedPictographic {
      value = range.lowerBound
      value |= UInt32(range.count - 1) << 21
      value |= 1 << 31
    }

    return "0x\(String(value, radix: 16, uppercase: true))"
  }

  result += """
  
  };
  
  
  """
}

// Main entry point into the grapheme break property generator.
func generateGraphemeBreakProperty() {
  var result = readFile("Input/GraphemeData.h")

  let baseData = getGraphemeBreakPropertyData(
    for: "Data/GraphemeBreakProperty.txt"
  )
  let emojiData = getGraphemeBreakPropertyData(for: "Data/emoji-data.txt")

  let flattened = flatten(baseData + emojiData)

  var data: [(ClosedRange<UInt32>, Unicode.GraphemeBreakProperty)] = []

  for (range, gbp) in flattened {
    guard range.count < 0x200 else {
      let lower = String(range.lowerBound, radix: 16)
      let upper = String(range.upperBound, radix: 16)

      print("Manual range: 0x\(lower) ... 0x\(upper) for \(gbp)")
      continue
    }

    data.append((range, gbp))
  }

  emit(data, into: &result)
  
  // Handle the CLDR grapheme breaking rules:
  
  let indicSyllabicCategory = readFile("Data/IndicSyllabicCategory.txt")
  
  let consonants = getLinkingConsonant(from: indicSyllabicCategory)
  
  emitLinkingConsonant(consonants, into: &result)
  
  result += """
  #endif // #ifndef GRAPHEME_DATA_H
  
  """
  
  write(result, to: "Output/Common/GraphemeData.h")
}

generateGraphemeBreakProperty()
