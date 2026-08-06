//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

// The Bidi_Class property values. The raw value is the abbreviation used in the
// data lines of DerivedBidiClass.txt (UAX #44). The '@missing' lines in that
// file instead spell out the long name, so 'init(longName:)' handles those.
//
// WARNING: 'binaryRepresentation' must be kept in-sync with the stdlib code
// that decodes these values, found at:
//   'stdlib/public/core/UnicodeScalarProperties.swift' (Unicode.BidiClass) and
//   'stdlib/public/stubs/Unicode/UnicodeScalarProps.cpp'
//     (_swift_stdlib_getBidiClass).
enum BidiClass: String {
  case leftToRight = "L"
  case rightToLeft = "R"
  case arabicLetter = "AL"
  case europeanNumber = "EN"
  case europeanSeparator = "ES"
  case europeanTerminator = "ET"
  case arabicNumber = "AN"
  case commonSeparator = "CS"
  case nonspacingMark = "NSM"
  case boundaryNeutral = "BN"
  case paragraphSeparator = "B"
  case segmentSeparator = "S"
  case whitespace = "WS"
  case otherNeutral = "ON"
  case leftToRightEmbedding = "LRE"
  case leftToRightOverride = "LRO"
  case rightToLeftEmbedding = "RLE"
  case rightToLeftOverride = "RLO"
  case popDirectionalFormat = "PDF"
  case leftToRightIsolate = "LRI"
  case rightToLeftIsolate = "RLI"
  case firstStrongIsolate = "FSI"
  case popDirectionalIsolate = "PDI"

  // The long names only appear on the '@missing' default lines, so we only
  // need to recognize the handful of classes that show up there.
  init?(longName: String) {
    switch longName {
    case "Left_To_Right":
      self = .leftToRight
    case "Right_To_Left":
      self = .rightToLeft
    case "Arabic_Letter":
      self = .arabicLetter
    case "European_Terminator":
      self = .europeanTerminator
    default:
      return nil
    }
  }

  var binaryRepresentation: UInt8 {
    switch self {
    case .leftToRight:
      return 0
    case .rightToLeft:
      return 1
    case .arabicLetter:
      return 2
    case .europeanNumber:
      return 3
    case .europeanSeparator:
      return 4
    case .europeanTerminator:
      return 5
    case .arabicNumber:
      return 6
    case .commonSeparator:
      return 7
    case .nonspacingMark:
      return 8
    case .boundaryNeutral:
      return 9
    case .paragraphSeparator:
      return 10
    case .segmentSeparator:
      return 11
    case .whitespace:
      return 12
    case .otherNeutral:
      return 13
    case .leftToRightEmbedding:
      return 14
    case .leftToRightOverride:
      return 15
    case .rightToLeftEmbedding:
      return 16
    case .rightToLeftOverride:
      return 17
    case .popDirectionalFormat:
      return 18
    case .leftToRightIsolate:
      return 19
    case .rightToLeftIsolate:
      return 20
    case .firstStrongIsolate:
      return 21
    case .popDirectionalIsolate:
      return 22
    }
  }
}

func parseBidiScalars(_ string: Substring) -> ClosedRange<UInt32> {
  let filtered = string.filter { !$0.isWhitespace }

  // If we have '.' appear, it means we have a legitimate range. Otherwise,
  // it's a singular scalar.
  if filtered.contains(".") {
    let range = filtered.split(separator: ".")

    return UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
  } else {
    let scalar = UInt32(filtered, radix: 16)!

    return scalar ... scalar
  }
}

// Unlike DerivedGeneralCategory.txt, DerivedBidiClass.txt does not list every
// scalar. Unassigned scalars instead take their value from the '@missing'
// default lines in the header comment, e.g:
//
//   # @missing: 0000..10FFFF; Left_To_Right
//   # @missing: 0590..05FF; Right_To_Left
//
// The first line establishes the overall default, and the later, more specific
// ranges override it for the scalars they cover.
func getBidiClassDefaults(
  from data: String
) -> [(ClosedRange<UInt32>, BidiClass)] {
  var result: [(ClosedRange<UInt32>, BidiClass)] = []

  for line in data.split(separator: "\n") {
    guard line.hasPrefix("# @missing:") else {
      continue
    }

    // Drop the '# @missing:' prefix and split the "range; Long_Name" that
    // follows.
    let info = line.dropFirst("# @missing:".count)
    let components = info.split(separator: ";")

    let longName = components[1].filter { !$0.isWhitespace }

    guard let bidi = BidiClass(longName: longName) else {
      fatalError("Unhandled Bidi_Class default: \(longName)")
    }

    result.append((parseBidiScalars(components[0]), bidi))
  }

  return result
}

func getBidiClass(
  from data: String,
  into result: inout [(ClosedRange<UInt32>, BidiClass)]
) {
  for line in data.split(separator: "\n") {
    // Skip comments (including the '@missing' lines handled separately).
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    let filteredClass = components[1].filter { !$0.isWhitespace }

    guard let bidi = BidiClass(rawValue: filteredClass) else {
      fatalError("Unhandled Bidi_Class value: \(filteredClass)")
    }

    result.append((parseBidiScalars(components[0]), bidi))
  }
}

func emitBidiClass(
  defaults: [(ClosedRange<UInt32>, BidiClass)],
  data: [(ClosedRange<UInt32>, BidiClass)],
  into result: inout String
) {
  var bidiData: [UInt32: BidiClass] = [:]

  // Apply the '@missing' defaults first. They are listed most-general first, so
  // applying them in order lets the more specific ranges override the overall
  // default.
  for (range, bidi) in defaults {
    for scalar in range {
      bidiData[scalar] = bidi
    }
  }

  // Now override the defaults with the explicitly-listed scalars.
  for (range, bidi) in data {
    for scalar in range {
      bidiData[scalar] = bidi
    }
  }

  // Surrogate code points can never be queried (Unicode.Scalar cannot represent
  // them), so drop them. Everything else in 0x0 ... 0x10FFFF is covered by the
  // overall '@missing' default, giving us a complete inversion list.
  bidiData = bidiData.filter { Unicode.Scalar($0.key) != nil }

  // A proper inversion list: each entry records the start of a range and the
  // value that holds until the next entry's start. Merging adjacent ranges that
  // share a value keeps the list minimal.
  let data = flatten(Array(bidiData))

  result += """
  #define BIDI_CLASS_COUNT \(data.count)


  """

  emitCollection(
    data,
    name: "_swift_stdlib_bidiClass",
    type: "__swift_uint32_t",
    into: &result
  ) {
    var value = $0.0.lowerBound
    value |= UInt32($0.1.binaryRepresentation) << 21

    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func generateBidiClass(for platform: String, into result: inout String) {
  let derivedBidiClass: String

  switch platform {
  case "Apple":
    derivedBidiClass = readFile("Data/17/Apple/DerivedBidiClass.txt")
  default:
    derivedBidiClass = readFile("Data/17/DerivedBidiClass.txt")
  }

  let defaults = getBidiClassDefaults(from: derivedBidiClass)

  var data: [(ClosedRange<UInt32>, BidiClass)] = []
  getBidiClass(from: derivedBidiClass, into: &data)

  emitBidiClass(defaults: defaults, data: data, into: &result)
}
