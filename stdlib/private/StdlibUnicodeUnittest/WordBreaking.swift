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

func parseWordBreakTests(
  _ data: String,
  into result: inout [(String, [String])]
) {
  for line in data.split(separator: "\n") {
    // Only look at actual tests
    guard line.hasPrefix("÷") else {
      continue
    }

    let components = line.split(separator: "#").first!.split(separator: " ")

    var string = ""
    var words: [String] = [""]

    for i in components.indices.dropFirst() {
      // If we're an odd index, this is a scalar.
      if !i.isMultiple(of: 2) {
        let scalar = Unicode.Scalar(UInt32(components[i], radix: 16)!)!

        string.unicodeScalars.append(scalar)
        words[words.count - 1].unicodeScalars.append(scalar)
      } else {
        // Otherwise, it is a word breaking operator.

        // If this is a break, record the +1 count. Otherwise it is × which is
        // not a break.
        if components[i] == "÷" {
          words.append("")
        }
      }
    }

    words.removeLast()

    result.append((string, words))
  }
}

public let wordBreakTests: [(String, [String])] = {
  var result: [(String, [String])] = []

  let testFile = readInputFile("WordBreakTest.txt")

  parseWordBreakTests(testFile, into: &result)

  return result
}()
#endif
