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

public struct GraphemeBreakTest {
  public let string: String
  public let pieces: [[Unicode.Scalar]]

  init?(line: some StringProtocol) {
    // Only look at actual tests
    guard line.hasPrefix("÷") else { return nil }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: " ")

    var string = ""
    var pieces: [[Unicode.Scalar]] = []

    var piece: [Unicode.Scalar] = []
    for component in components {
      switch component {
      case "":
        break
      case "×": // no grapheme break opportunity
        break
      case "÷": // grapheme break opportunity
        guard !piece.isEmpty else { break }
        pieces.append(piece)
        piece = []
      case _: // hexadecimal scalar value
        guard let value = UInt32(component, radix: 16) else { return nil }
        guard let scalar = Unicode.Scalar(value) else { return nil }
        string.unicodeScalars.append(scalar)
        piece.append(scalar)
      }
    }
    if !piece.isEmpty { pieces.append(piece) }
    self.string = string
    self.pieces = pieces
  }
}

public let graphemeBreakTests: [GraphemeBreakTest] = {
  let testFile = readInputFile("GraphemeBreakTest.txt")
  return testFile.split(separator: "\n")
    .compactMap { GraphemeBreakTest(line: $0) }
}()
#endif
