//===--- NameTableNI.swift - PDB support for Swift ------------------------===//
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
// Implements the name table parser required by the PDB header (as distinct
// from the one in the NameMap stream).
//
//===----------------------------------------------------------------------===//

import Swift

internal import BacktracingImpl.ImageFormats.PDB

/// This is the name table from the PDB header, as opposed to the
/// one in the NameMap stream.
struct NameTableNI: Sequence {
  typealias Element = [Int:String].Element
  typealias Index = [Int:String].Index
  typealias Iterator = [Int:String].Iterator

  var startIndex: Index { niToString.startIndex }
  var endIndex: Index { niToString.endIndex }

  var stringToNi: [String:Int]
  var niToString: [Int:String]

  func makeIterator() -> Iterator {
    return niToString.makeIterator()
  }

  static func load(from stream: inout MultiStreamFile.Stream)
    throws -> NameTableNI? {
    // Read the string data itself
    let bytes = try stream.read(as: CB.self)
    let buffer = try stream.read(as: UInt8.self, count: Int(bytes))

    // Now read the map
    let _ = try stream.read(as: UInt32.self)
    let size = try stream.read(as: UInt32.self)

    // Read the present bitmap
    let present = try Bitset.load(from: &stream)

    // Read the deleted bitmap
    let _ = try Bitset.load(from: &stream)

    var stringToNi: [String:Int] = [:]
    var niToString: [Int:String] = [:]

    // Now for each item in the present set, read a pair of words
    // that define the mapping
    for ndx in 0..<Int(size) {
      if present[ndx] {
        let stringOffset = Int(try stream.read(as: UInt32.self))
        let ni = try stream.read(as: UInt32.self)

        // Get the string
        let string = buffer.withUnsafeBytes {
          (buf8: UnsafeRawBufferPointer) -> String in

          var end = stringOffset
          while end < buf8.count && buf8[end] != 0 {
            end += 1
          }

          return String(decoding: buf8[stringOffset..<end], as: Windows1252.self)
        }

        stringToNi[string] = Int(ni)
        niToString[Int(ni)] = string
      }
    }

    // There's also a name count
    let _ = try stream.read(as: UInt32.self)

    return NameTableNI(stringToNi: stringToNi, niToString: niToString)
  }

  subscript(s: String) -> Int? {
    return stringToNi[s]
  }

  subscript(ni: Int) -> String? {
    return niToString[ni]
  }
}
