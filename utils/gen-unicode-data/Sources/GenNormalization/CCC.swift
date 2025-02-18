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

// Given a string to the UnicodeData file, return the flattened list of scalar
// to Canonical Combining Class.
//
// Each line in this data file is formatted like the following:
//
//     0000;<control>;Cc;0;BN;;;;;N;NULL;;;;
//
// Where each section is split by a ';'. The first section informs us of the
// scalar in the line with the various properties. For the purposes of CCC data,
// we only need the 0 in between the Cc and BN (index 3) which is the raw value
// for the CCC.
func getCCCData(from data: String, with dict: inout [UInt32: UInt16]) {
  for line in data.split(separator: "\n") {
    let components = line.split(separator: ";", omittingEmptySubsequences: false)

    let ccc = UInt16(components[3])!

    // For the most part, CCC 0 is the default case, so we can save much more
    // space by not keeping this information and making it the fallback case.
    if ccc == 0 {
      continue
    }

    let scalarStr = components[0]
    let scalar = UInt32(scalarStr, radix: 16)!

    var newValue = dict[scalar, default: 0]

    // Store our ccc past the 3rd bit.
    newValue |= ccc << 3

    dict[scalar] = newValue
  }
}
