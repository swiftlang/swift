//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Normalization tests are currently only available on Darwin, awaiting a sensible
// file API...
#if _runtime(_ObjC)
import Foundation

func parseGraphemeBreakTests(
  _ data: String,
  into result: inout [(String, Int)]
) {
  for line in data.split(separator: "\n") {
    // Only look at actual tests
    guard line.hasPrefix("÷") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: " ")

    var string = ""
    var count = 0

    for i in components.indices {
      guard i != 0 else {
        continue
      }

      let scalar: Unicode.Scalar

      // If we're an odd index, this is a scalar.
      if i & 0x1 == 1 {
        scalar = Unicode.Scalar(UInt32(components[i], radix: 16)!)!

        string.unicodeScalars.append(scalar)
      } else {
        // Otherwise, it is a grapheme breaking operator.

        // If this is a break, record the +1 count. Otherwise it is × which is
        // not a break.
        if components[i] == "÷" {
          count += 1
        }
      }
    }

    result.append((string, count))
  }
}

public let graphemeBreakTests: [(String, Int)] = {
  var result: [(String, Int)] = []

  let testFile = readInputFile("GraphemeBreakTest.txt")

  parseGraphemeBreakTests(testFile, into: &result)

  return result
}()
#endif
