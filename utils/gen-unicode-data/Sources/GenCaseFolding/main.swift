//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

func getCaseData(for path: String) -> [UInt32: [UInt32]] {
  let data = readFile(path)

  var mappings: [UInt32: [UInt32]] = [:]

  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let components = line.split(separator: ";")

    let status = components[1].filter { !$0.isWhitespace }

    // We only care about Common and Full case mappings.
    guard status == "C" || status == "F" else {
      continue
    }

    let scalar = UInt32(components[0], radix: 16)!

    let mapping = components[2].split(separator: " ").map { UInt32($0, radix: 16)! }

    mappings[scalar] = mapping
  }

  return mappings
}

func emitCaseData(_ data: [UInt32: [UInt32]], into result: inout String) {
  let caseMph = mph(for: data.keys.map { UInt64($0) })

  emitMph(
    caseMph,
    name: "_swift_stdlib_case",
    defineLabel: "CASE_FOLD",
    into: &result
  )

  var caseData: [(UInt32, [UInt32])] = .init(
    repeating: (0, []),
    count: data.keys.count
  )

  for (key, mapping) in data {
    let idx = caseMph.index(for: UInt64(key))

    caseData[idx] = (key, mapping)
  }

  emitCollection(
    caseData,
    name: "_swift_stdlib_case",
    type: "__swift_uint64_t",
    into: &result
  ) {
    // Our original scalar goes at the bottom 21 bits.
    var value = UInt64($0)

    // If our scalar is a Full mapping (maps to more than 1 scalar), then
    // set the top bit to indicate another look through and move on.
    guard $1.count == 1 else {
      value |= 0x1 << 63

      return "0x\(String(value, radix: 16, uppercase: true))"
    }

    let distance = Int32($0) - Int32($1[0])

    value |= UInt64(UInt32(bitPattern: distance)) << 21

    return "0x\(String(value, radix: 16, uppercase: true))"
  }

  let fullData = data.filter { $1.count > 1 }
  let fullMph = mph(for: fullData.keys.map { UInt64($0) })

  emitMph(
    fullMph,
    name: "_swift_stdlib_case_full",
    defineLabel: "CASE_FULL_FOLD",
    into: &result
  )

  var fullCaseData: [(UInt32, [UInt32])] = .init(
    repeating: (0, []),
    count: fullData.count
  )

  for (key, mapping) in fullData {
    let idx = fullMph.index(for: UInt64(key))

    fullCaseData[idx] = (key, mapping)
  }

  emitCollection(
    fullCaseData,
    name: "_swift_stdlib_case_full",
    type: "__swift_uint64_t",
    into: &result
  ) {
    var value: UInt64 = 0

    // Store the count in the top 2 bits.
    assert((2 ... 3).contains($1.count))
    value |= UInt64($1.count) << 62

    for (i, scalar) in $1.enumerated() {
      let distance = Int32($0) - Int32(scalar)
      assert(distance.magnitude <= UInt16.max)

      value |= UInt64(distance.magnitude) << (i * 17)

      if distance < 0 {
        value |= 0x10000 << (i * 17)
      }
    }

    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func generateScriptProperties() {
  var result = readFile("Input/CaseData.h")

  let data = getCaseData(for: "Data/16/CaseFolding.txt")
  emitCaseData(data, into: &result)

  result += """
    #endif // #ifndef CASE_DATA_H

    """

  write(result, to: "Output/Common/CaseData.h")
}

generateScriptProperties()
