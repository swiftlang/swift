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

enum GeneralCategory: String {
  case uppercaseLetter = "Lu"
  case lowercaseLetter = "Ll"
  case titlecaseLetter = "Lt"
  case modifierLetter = "Lm"
  case otherLetter = "Lo"
  case nonspacingMark = "Mn"
  case spacingMark = "Mc"
  case enclosingMark = "Me"
  case decimalNumber = "Nd"
  case letterNumber = "Nl"
  case otherNumber = "No"
  case connectorPunctuation = "Pc"
  case dashPunctuation = "Pd"
  case openPunctuation = "Ps"
  case closePunctuation = "Pe"
  case initialPunctuation = "Pi"
  case finalPunctuation = "Pf"
  case otherPunctuation = "Po"
  case mathSymbol = "Sm"
  case currencySymbol = "Sc"
  case modifierSymbol = "Sk"
  case otherSymbol = "So"
  case spaceSeparator = "Zs"
  case lineSeparator = "Zl"
  case paragraphSeparator = "Zp"
  case control = "Cc"
  case format = "Cf"
  case surrogate = "Cs"
  case privateUse = "Co"
  case unassigned = "Cn"

  var binaryRepresentation: UInt8 {
    switch self {
    case .uppercaseLetter:
      return 0
    case .lowercaseLetter:
      return 1
    case .titlecaseLetter:
      return 2
    case .modifierLetter:
      return 3
    case .otherLetter:
      return 4
    case .nonspacingMark:
      return 5
    case .spacingMark:
      return 6
    case .enclosingMark:
      return 7
    case .decimalNumber:
      return 8
    case .letterNumber:
      return 9
    case .otherNumber:
      return 10
    case .connectorPunctuation:
      return 11
    case .dashPunctuation:
      return 12
    case .openPunctuation:
      return 13
    case .closePunctuation:
      return 14
    case .initialPunctuation:
      return 15
    case .finalPunctuation:
      return 16
    case .otherPunctuation:
      return 17
    case .mathSymbol:
      return 18
    case .currencySymbol:
      return 19
    case .modifierSymbol:
      return 20
    case .otherSymbol:
      return 21
    case .spaceSeparator:
      return 22
    case .lineSeparator:
      return 23
    case .paragraphSeparator:
      return 24
    case .control:
      return 25
    case .format:
      return 26
    case .surrogate:
      return 27
    case .privateUse:
      return 28
    case .unassigned:
      return 29
    }
  }
}

func getGeneralCategory(
  from data: String,
  into result: inout [(ClosedRange<UInt32>, GeneralCategory)]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    let filteredCategory = components[1].filter { !$0.isWhitespace }

    guard let gc = GeneralCategory(rawValue: filteredCategory) else {
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

    result.append((scalars, gc))
  }
}

func emitGeneralCategory(
  _ data: [(ClosedRange<UInt32>, GeneralCategory)],
  into result: inout String
) {
  result += """
    #define GENERAL_CATEGORY_COUNT \(data.count)


    """

  emitCollection(
    data,
    name: "_swift_stdlib_generalCategory",
    type: "__swift_uint64_t",
    into: &result
  ) {
    var value: UInt64 = UInt64($0.0.lowerBound)

    let generalCategory = $0.1.binaryRepresentation

    value |= UInt64(generalCategory) << 21

    value |= UInt64($0.0.count - 1) << 32

    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func generateGeneralCategory(into result: inout String) {
  let derivedGeneralCategory = readFile("Data/16/DerivedGeneralCategory.txt")

  var data: [(ClosedRange<UInt32>, GeneralCategory)] = []

  getGeneralCategory(from: derivedGeneralCategory, into: &data)

  emitGeneralCategory(flatten(data), into: &result)
}
