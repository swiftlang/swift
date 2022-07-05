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

// Given a string to the DerivedNormalizationProps Unicode file, return the
// flattened list of scalar to NFC Quick Check.
//
// Each line in one of these data files is formatted like the following:
//
//     0343..0344    ; NFC_QC; N # Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
//     0374          ; NFC_QC; N # Lm       GREEK NUMERAL SIGN
//
// Where each section is split by a ';'. The first section informs us of either
// the range of scalars who conform to this property or the singular scalar
// who does. The second section tells us what normalization property these
// scalars conform to. There are extra comments telling us what general
// category these scalars are a part of, how many scalars are in a range, and
// the name of the scalars.
func getQCData(from data: String, with dict: inout [UInt32: UInt16]) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    // Get the property first because we only care about NFC_QC or NFD_QC.
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    guard filteredProperty == "NFD_QC" || filteredProperty == "NFC_QC" else {
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
    
    // Special case: Do not store hangul NFD_QC.
    if scalars == 0xAC00...0xD7A3, filteredProperty == "NFD_QC" {
      continue
    }
    
    let filteredNFCQC = components[2].filter { !$0.isWhitespace }
    
    for scalar in scalars {
      var newValue = dict[scalar, default: 0]
      
      switch filteredProperty {
      case "NFD_QC":
        // NFD_QC is the first bit in the data value and is set if a scalar is
        // NOT qc.
        newValue |= 1 << 0
      case "NFC_QC":
        // If our scalar is NOT NFC_QC, then set the 2nd bit in our data value.
        // Otherwise, this scalar is MAYBE NFC_QC, so set the 3rd bit. A scalar
        // who IS NFC_QC has a value of 0.
        if filteredNFCQC == "N" {
          newValue |= 1 << 1
        } else {
          newValue |= 1 << 2
        }
      default:
        fatalError("Unknown NFC_QC type?")
      }
      
      dict[scalar] = newValue
    }
  }
}
