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

import SwiftShims

extension Unicode {
  internal struct NFD<S: StringProtocol> {
    let base: S
  }
}

extension Unicode.NFD {
  internal struct Iterator {
    var buffer: [(scalar: Unicode.Scalar, normData: UInt16)] = []
    
    var iterator: S.UnicodeScalarView.Iterator
    
    var starter: (scalar: Unicode.Scalar, normData: UInt16)? = nil
  }
}

extension Unicode.NFD.Iterator: IteratorProtocol {
  internal mutating func decompose(
    _ scalar: Unicode.Scalar,
    with normData: UInt16
  ) {
    // ASCII always decomposes to itself.
    if scalar.value < 0xC0 {
      // ASCII always has normData of 0.
      // CCC = 0, NFC_QC = Yes, NFD_QC = Yes
      buffer.append((scalar: scalar, normData: 0))
      return
    }

    // Handle Hangul decomposition algorithmically.
    if (0xAC00 ... 0xD7A3).contains(scalar.value) {
      let sIdx = scalar.value - 0xAC00

      let lIdx = sIdx / 588
      let l = Unicode.Scalar(_value: 0x1100 &+ lIdx)
      // Hangul leading consonants, L, always have normData of 0.
      // CCC = 0, NFC_QC = Yes, NFD_QC = Yes
      buffer.append((scalar: l, normData: 0))
    
      let vIdx = (sIdx % 588) / 28
      let v = Unicode.Scalar(_value: 0x1161 &+ vIdx)
      // Hangul vowels, V, always have normData of 4.
      // CCC = 0, NFC_QC = Maybe, NFD_QC = Yes
      buffer.append((scalar: v, normData: 4))
      
      let tIdx = sIdx % 28
      if tIdx != 0 {
        let t = Unicode.Scalar(_value: 0x11A7 &+ tIdx)
        // Hangul tail consonants, T, always have normData of 4.
        // CCC = 0, NFC_QC = Maybe, NFD_QC = Yes
        buffer.append((scalar: t, normData: 4))
      }
      
      return
    }

    // Otherwise, we need to lookup the decomposition (if there is one).
    decomposeSlow(scalar, with: normData)
  }

  internal mutating func decomposeSlow(
    _ scalar: Unicode.Scalar,
    with normData: UInt16
  ) {
    // Look into the decomposition perfect hash table.
    let decompEntry = _swift_stdlib_getDecompositionEntry(scalar.value)

    // Our original scalar is stored in the first 18 bits of this entry.
    let hashedScalar = (decompEntry << 14) >> 14

    // If this is not our original scalar, then we have no decomposition for this
    // scalar, so just emit itself.
    guard scalar.value == hashedScalar else {
      buffer.append((scalar: scalar, normData: normData))
      return
    }
    
    // Otherwise, our index is stored in the top 14 bits.
    let decompIdx = Int(decompEntry >> 18)
    
    let decompPtr = _swift_stdlib_nfd_decompositions._unsafelyUnwrappedUnchecked
    
    // This size is the utf8 length of the decomposition.
    var size = Int(decompPtr[decompIdx])
    
    let utf8 = UnsafeBufferPointer(
      start: decompPtr + decompIdx + 1,
      count: size
    )

    while size > 0 {
      let (scalar, len) = _decodeScalar(utf8, startingAt: utf8.count &- size)
      size &-= len

      let normData: UInt16

      // Fast path: Because this will be emitted into the completed NFD buffer,
      // we don't need to look at NFD_QC anymore which lets us do a larger
      // latiny check for NFC_QC and CCC (0xC0 vs. 0x300).
      if scalar.value < 0x300 {
        normData = 0
      } else {
        normData = _swift_stdlib_getNormData(scalar.value)
      }

      buffer.append((scalar: scalar, normData: normData))
    }
  }

  internal mutating func next() -> (scalar: Unicode.Scalar, normData: UInt16)? {
    // Empty out our buffer before attempting to decompose the next
    // normalization segment.
    if !buffer.isEmpty {
      return buffer.removeLast()
    }
    
    // If we have a starter stored, take care of that before asking for more
    // scalars.
    if let starter = starter.take() {
      let ccc = starter.normData >> 3
      var isNFDQC = starter.normData & 1 == 0
      
      // Because we don't store precomposed hangul in our NFD_QC data, these
      // will return true for NFD_QC when in fact they are not.
      if (0xAC00 ... 0xD7A3).contains(starter.scalar.value) {
        isNFDQC = false
      }
      
      // If our scalar IS NFD quick check, then it's as simple as appending to
      // our buffer and moving on the next scalar. Otherwise, we need to
      // decompose this and append each decomposed scalar.
      if isNFDQC {
        // Fast path: If our scalar is also ccc = 0, then this doesn't need to
        // be appended to the buffer at all.
        if ccc == 0 {
          return starter
        }

        buffer.append(starter)
      } else {
        decompose(starter.scalar, with: starter.normData)
      }
    }
    
    while let scalar = iterator.next() {
      let normData = _swift_stdlib_getNormData(scalar.value)
      let ccc = normData >> 3
      var isNFDQC = normData & 1 == 0
      
      // Because we don't store precomposed hangul in our NFD_QC data, these
      // will return true for NFD_QC when in fact they are not.
      if (0xAC00 ... 0xD7A3).contains(scalar.value) {
        isNFDQC = false
      }
      
      // If we've reached a starter, stop.
      if ccc == 0, !buffer.isEmpty {
        starter = (scalar, normData)
        break
      }
      
      // If our scalar IS NFD quick check, then it's as simple as appending to
      // our buffer and moving on the next scalar. Otherwise, we need to
      // decompose this and append each decomposed scalar.
      if isNFDQC {
        // Fast path: If our scalar is also ccc = 0, then this doesn't need to
        // be appended to the buffer at all.
        if ccc == 0 {
          return (scalar, normData)
        }

        buffer.append((scalar, normData))
      } else {
        decompose(scalar, with: normData)
      }
    }
    
    // If we made it this far with an empty buffer, we're done.
    if buffer.isEmpty {
      return nil
    }
    
    // Sort the entire buffer based on the canonical combining class.
    buffer._insertionSort(within: buffer.indices) {
      ($0.normData >> 3) < ($1.normData >> 3)
    }
    
    // Reverse the buffer after sorting to make removals faster from the back
    // rather than removing from the front.
    buffer.reverse()
    
    return buffer.removeLast()
  }
}

extension Unicode.NFD: Sequence {
  internal func makeIterator() -> Iterator {
    Iterator(iterator: base.unicodeScalars.makeIterator())
  }
}

extension StringProtocol {
  internal var nfd: Unicode.NFD<Self> {
    Unicode.NFD(base: self)
  }
}
