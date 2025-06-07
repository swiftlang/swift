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

  enum IndicConjunctBreakProperty: String {
    case consonant = "Consonant"
    case extend = "Extend"
    // we just manually check for linker in StringGraphemeBreaking.swift
  }
}

struct GraphemeBreakEntry : Comparable {
  static func < (lhs: GraphemeBreakEntry, rhs: GraphemeBreakEntry) -> Bool {
    return lhs.index < rhs.index
  }
  
  let index: Int
  let range: ClosedRange<UInt32>
  let property: Unicode.GraphemeBreakProperty
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

func getInCB(
  _ incb: Unicode.IndicConjunctBreakProperty,
  from data: String
) -> [ClosedRange<UInt32>] {
  var unflattened: [(ClosedRange<UInt32>, Unicode.IndicConjunctBreakProperty)] = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let components = line.split(separator: ";")
    
    // Get the property first because it may be one we don't care about.
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    // We only care about 'InCB' properties.
    guard filteredProperty == "InCB" else {
      continue
    }

    let splitInCBProperty = components[2].split(separator: "#")
    let filteredInCBProperty = splitInCBProperty[0].filter { !$0.isWhitespace }

    guard filteredInCBProperty == incb.rawValue else {
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
    
    unflattened.append((scalars, incb))
  }
  
  return flatten(unflattened).map { $0.0 }
}

// Takes the flattened data and writes it as a static C array.
func emit(
  _ data: [GraphemeBreakEntry],
  into result: inout String
) {
  result += """
  #define GRAPHEME_BREAK_DATA_COUNT \(data.count)
  
  static const __swift_uint32_t _swift_stdlib_graphemeBreakProperties[\(data.count)] = {

  """

  formatCollection(data, into: &result) { (entry) -> String in
    let range = entry.range
    let gbp = entry.property
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

func emitInCB(
  _ incb: Unicode.IndicConjunctBreakProperty,
  _ data: [ClosedRange<UInt32>],
  into result: inout String
) {
  // 64 bit arrays * 8 bytes = .512 KB
  var bitArrays: [BitArray] = .init(repeating: .init(size: 64), count: 64)
  
  let chunkSize = 0x110000 / 64 / 64
  
  var chunks: [Int] = []
  
  for i in 0 ..< 64 * 64 {
    let lower = i * chunkSize
    let upper = lower + chunkSize - 1
    
    let idx = i / 64
    let bit = i % 64
    
    for scalar in lower ... upper {
      if data.contains(where: { $0.contains(UInt32(scalar)) }) {
        chunks.append(i)
        
        bitArrays[idx][bit] = true
        break
      }
    }
  }
  
  // Remove the trailing 0s. Currently this reduces quick look size down to
  // 96 bytes from 512 bytes.
  var reducedBA = Array(bitArrays.reversed())
  reducedBA = Array(reducedBA.drop {
    $0.words == [0x0]
  })
  
  bitArrays = reducedBA.reversed()
  
  // Keep a record of every rank for all the bitarrays.
  var ranks: [UInt16] = []
  
  // Record our quick look ranks.
  var lastRank: UInt16 = 0
  for (i, _) in bitArrays.enumerated() {
    guard i != 0 else {
      ranks.append(0)
      continue
    }
    
    var rank = UInt16(bitArrays[i - 1].words[0].nonzeroBitCount)
    rank += lastRank
    
    ranks.append(rank)
    
    lastRank = rank
  }
  
  // Insert our quick look size at the beginning.
  var size = BitArray(size: 64)
  size.words = [UInt64(bitArrays.count)]
  bitArrays.insert(size, at: 0)
  
  for chunk in chunks {
    var chunkBA = BitArray(size: chunkSize)
    
    let lower = chunk * chunkSize
    let upper = lower + chunkSize
    
    for scalar in lower ..< upper {
      if data.contains(where: { $0.contains(UInt32(scalar)) }) {
        chunkBA[scalar % chunkSize] = true
      }
    }
    
    // Append our chunk bit array's rank.
    var lastRank: UInt16 = 0
    for (i, _) in chunkBA.words.enumerated() {
      guard i != 0 else {
        ranks.append(0)
        continue
      }
      
      var rank = UInt16(chunkBA.words[i - 1].nonzeroBitCount)
      rank += lastRank
      
      ranks.append(rank)
      lastRank = rank
    }
    
    bitArrays += chunkBA.words.map {
      var ba = BitArray(size: 64)
      ba.words = [$0]
      return ba
    }
  }
  
  emitCollection(
    ranks,
    name: "_swift_stdlib_InCB_\(incb.rawValue)_ranks",
    into: &result
  )
  
  emitCollection(
    bitArrays,
    name: "_swift_stdlib_InCB_\(incb.rawValue)",
    type: "__swift_uint64_t",
    into: &result
  ) {
    assert($0.words.count == 1)
    return "0x\(String($0.words[0], radix: 16, uppercase: true))"
  }
}

func emitInCB(into result: inout String, _ platform: String) {
  let derviedCoreProperties: String
  
  switch platform {
  case "Apple":
    derviedCoreProperties = readFile("Data/16/Apple/DerivedCoreProperties.txt")
  default:
    derviedCoreProperties = readFile("Data/16/DerivedCoreProperties.txt")
  }

  let consonants = getInCB(.consonant, from: derviedCoreProperties)

  emitInCB(.consonant, consonants, into: &result)
}

// Main entry point into the grapheme break property generator.
func generateGraphemeBreakProperty(for platform: String) {
  var result = readFile("Input/GraphemeData.h")

  let baseData = getGraphemeBreakPropertyData(
    for: "Data/16/GraphemeBreakProperty.txt"
  )
  let emojiData = getGraphemeBreakPropertyData(for: "Data/16/emoji-data.txt")

  let flattened = flatten(baseData + emojiData)

  var data: [GraphemeBreakEntry] = []

  for (range, gbp) in flattened {
    guard range.count < 0x200 else {
      let lower = String(range.lowerBound, radix: 16)
      let upper = String(range.upperBound, radix: 16)

      print("Manual range: 0x\(lower) ... 0x\(upper) for \(gbp)")
      continue
    }

    data.append(GraphemeBreakEntry(
      index: data.count,
      range: range,
      property: gbp
    ))
  }

  data = eytzingerize(data, dummy: GraphemeBreakEntry(
    index: 0,
    range: 0...0,
    property: .control
  ))

  emit(data, into: &result)

  // Handle the InCB properties:
  emitInCB(into: &result, platform)

  result += """
  #endif // #ifndef GRAPHEME_DATA_H

  """

  write(result, to: "Output/\(platform)/GraphemeData.h")
}

for platform in ["Common", "Apple"] {
  generateGraphemeBreakProperty(for: platform)
}
