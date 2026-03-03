//===--- Hashes.swift - PDB support for Swift ------------------------------===//
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
// Implements the two hash functions that are used by PDB files.
//
//===----------------------------------------------------------------------===//

import Swift

func lhash(_ buffer: UnsafeRawBufferPointer) -> UInt32 {
  var hash = buffer.withMemoryRebound(to: UInt32.self) { words in
    var hash = UInt32(0)
    for word in words {
      hash ^= word
    }
    return hash
  }

  var pos = buffer.count & ~0x3

  // Handle trailing 16-bit word
  if (buffer.count & 2) != 0 {
    let short = buffer.load(fromByteOffset: pos, as: UInt16.self)
    hash ^= UInt32(short)
    pos += 2
  }

  // Handle trailing byte
  if (buffer.count & 1) != 0 {
    let byte = buffer[pos]
    hash ^= UInt32(byte)
  }

  let toLowerMask = UInt32(0x20202020)
  hash |= toLowerMask
  hash ^= (hash >> 11)
  hash ^= (hash >> 16)

  return hash
}

func lhash2(_ buffer: UnsafeRawBufferPointer) -> UInt32 {
  // Do entire words first
  var hash = buffer.withMemoryRebound(to: UInt32.self) { words in
    var hash = UInt32(0xb170a1bf)
    for word in words {
      hash = hash &+ word
      hash = hash &+ (hash << 10)
      hash ^= (hash >> 6)
    }
    return hash
  }

  // Now do bytes
  let pos = buffer.count & ~0x3

  for byte in buffer[pos...] {
    hash = hash &+ UInt32(truncatingIfNeeded: byte)
    hash = hash &+ (hash << 10)
    hash ^= (hash >> 6)
  }

  // Finally, use an LCG (from p.284 of Numerical Recipes, 2nd edn)
  return hash &* 1664525 &+ 1013904223
}
