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

func getAge(
  from data: String,
  into result: inout [(ClosedRange<UInt32>, [UInt8])]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

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

    let version = components[1].filter { !$0.isWhitespace }
    let parts = version.split(separator: ".")

    let major = UInt8(parts[0])!
    let minor = UInt8(parts[1])!

    result.append((scalars, [major, minor]))
  }
}

func emitAge(
  data: [(ClosedRange<UInt32>, [UInt8])],
  into result: inout String
) {
  var uniqueAges: Set<[UInt8]> = []

  for (_, age) in data {
    uniqueAges.insert(age)
  }

  let ages = uniqueAges.map {
    UInt16($0[0]) | (UInt16($0[1]) << 8)
  }

  result += """
    #define AGE_COUNT \(data.count)


    """

  emitCollection(ages, name: "_swift_stdlib_ages_data", into: &result)

  emitCollection(
    data,
    name: "_swift_stdlib_ages",
    type: "__swift_uint64_t",
    into: &result
  ) {
    var value: UInt64 = UInt64($0.0.lowerBound)

    let age = UInt16($0.1[0]) | (UInt16($0.1[1]) << 8)
    let ageIndex = ages.firstIndex(of: age)!

    value |= UInt64(ageIndex) << 21

    value |= UInt64($0.0.count - 1) << 32

    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func generateAgeProp(into result: inout String) {
  let derivedAge = readFile("Data/16/DerivedAge.txt")

  var ageData: [(ClosedRange<UInt32>, [UInt8])] = []

  getAge(from: derivedAge, into: &ageData)

  emitAge(data: flatten(ageData), into: &result)
}
