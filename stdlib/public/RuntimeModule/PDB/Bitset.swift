//===--- Bitset.swift - PDB support for Swift -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a bit-set data structure.
//
//===----------------------------------------------------------------------===//

import Swift

struct Bitset: BidirectionalCollection {
  typealias Element = Bool
  typealias Index = Int

  private var words: [UInt32]

  var startIndex: Int { 0 }
  var endIndex: Int { words.count << 5 }

  static func load(
    from stream: inout MultiStreamFile.Stream
  ) throws -> Bitset {

    let count = try stream.read(as: UInt32.self)
    let words = try stream.read(as: UInt32.self, count: Int(count))

    return Bitset(words: words)
  }

  subscript(ndx: Int) -> Bool {
    precondition(ndx < (words.count << 5), "Access out of range")

    let wordNdx = ndx >> 5
    let bitNdx = ndx & 0x1f

    return ((words[wordNdx] >> bitNdx) & 1) != 0
  }

  func index(before ndx: Int) -> Int {
    precondition(ndx > 0, "Attempt to move off start of bitset")
    return ndx - 1
  }

  func index(after ndx: Int) -> Int {
    precondition(ndx < (words.count << 5), "Access out of range")
    return ndx + 1
  }
}
