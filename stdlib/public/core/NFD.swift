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

extension Unicode {
  internal struct _InternalNFD<S: StringProtocol> {
    let base: S.UnicodeScalarView
  }
}

extension Unicode._InternalNFD {
  internal struct Iterator {
    var buffer = Unicode._NormDataBuffer()

    // This index always points at the next starter of a normalization segment.
    // Each iteration of 'next()' moves this index up to the next starter.
    var index: S.UnicodeScalarView.Index

    let unicodeScalars: S.UnicodeScalarView
  }
}

extension Unicode._InternalNFD.Iterator: IteratorProtocol {
  internal mutating func decompose(
    _ scalar: Unicode.Scalar,
    with normData: Unicode._NormData
  ) {
    // ASCII always decomposes to itself.
    if _fastPath(scalar.value < 0xC0) {
      // ASCII always has normData of 0.
      // CCC = 0, NFC_QC = Yes, NFD_QC = Yes
      buffer.append((scalar, normData))
      return
    }

    // Handle Hangul decomposition algorithmically.
    // S.base = 0xAC00
    // S.count = 11172
    // S.base + S.count - 1 = 0xD7A3
    if (0xAC00 ... 0xD7A3).contains(scalar.value) {
      decomposeHangul(scalar)
      return
    }

    // Otherwise, we need to lookup the decomposition (if there is one).
    decomposeSlow(scalar, with: normData)
  }

  @inline(never)
  internal mutating func decomposeHangul(_ scalar: Unicode.Scalar) {
    // L = Hangul leading consonants
    let L: (base: UInt32, count: UInt32) = (base: 0x1100, count: 19)
    // V = Hangul vowels
    let V: (base: UInt32, count: UInt32) = (base: 0x1161, count: 21)
    // T = Hangul tail consonants
    let T: (base: UInt32, count: UInt32) = (base: 0x11A7, count: 28)
    // N = Number of precomposed Hangul syllables that start with the same
    //     leading consonant. (There is no base for N).
    let N: (base: UInt32, count: UInt32) = (base: 0x0, count: 588)
    // S = Hangul precomposed syllables
    let S: (base: UInt32, count: UInt32) = (base: 0xAC00, count: 11172)

    let sIdx = scalar.value &- S.base

    let lIdx = sIdx / N.count
    let l = Unicode.Scalar(_value: L.base &+ lIdx)
    // Hangul leading consonants, L, always have normData of 0.
    // CCC = 0, NFC_QC = Yes, NFD_QC = Yes
    buffer.append((scalar: l, normData: .init(rawValue: 0)))

    let vIdx = (sIdx % N.count) / T.count
    let v = Unicode.Scalar(_value: V.base &+ vIdx)
    // Hangul vowels, V, always have normData of 4.
    // CCC = 0, NFC_QC = Maybe, NFD_QC = Yes
    buffer.append((scalar: v, normData: .init(rawValue: 4)))

    let tIdx = sIdx % T.count
    if tIdx != 0 {
      let t = Unicode.Scalar(_value: T.base &+ tIdx)
      // Hangul tail consonants, T, always have normData of 4.
      // CCC = 0, NFC_QC = Maybe, NFD_QC = Yes
      buffer.append((scalar: t, normData: .init(rawValue: 4)))
    }
  }

  @inline(never)
  internal mutating func decomposeSlow(
    _ scalar: Unicode.Scalar,
    with normData: Unicode._NormData
  ) {
    // Look into the decomposition perfect hash table.
    let decompEntry = Unicode._DecompositionEntry(scalar)

    // If this is not our original scalar, then we have no decomposition for this
    // scalar, so just emit itself. This is required because perfect hashing
    // does not know the original set of keys that it used to create itself, so
    // we store the original scalar in our decomposition entry to ensure that
    // scalars that hash to the same index don't succeed.
    guard scalar == decompEntry.hashedScalar else {
      buffer.append((scalar, normData))
      return
    }

    var utf8 = decompEntry.utf8

    while utf8.count > 0 {
      let (scalar, len) = _decodeScalar(utf8, startingAt: 0)
      utf8 = UnsafeBufferPointer(rebasing: utf8[len...])

      // Fast path: Because this will be emitted into the completed NFD buffer,
      // we don't need to look at NFD_QC anymore which lets us do a larger
      // latiny check for NFC_QC and CCC (0xC0 vs. 0x300).
      let normData = Unicode._NormData(scalar, fastUpperbound: 0x300)

      buffer.append((scalar, normData))
    }
  }

  internal mutating func next() -> ScalarAndNormData? {
    // Empty out our buffer before attempting to decompose the next
    // normalization segment.
    if let nextBuffered = buffer.next() {
      return nextBuffered
    }

    while index < unicodeScalars.endIndex {
      let scalar = unicodeScalars[index]
      let normData = Unicode._NormData(scalar)

      // If we've reached a starter, stop.
      if normData.ccc == 0, !buffer.isEmpty {
        break
      }

      unicodeScalars.formIndex(after: &index)

      // If our scalar IS NFD quick check, then it's as simple as appending to
      // our buffer and moving on the next scalar. Otherwise, we need to
      // decompose this and append each decomposed scalar.
      if normData.isNFDQC {
        // Fast path: If our scalar is also ccc = 0, then this doesn't need to
        // be appended to the buffer at all.
        if normData.ccc == 0 {
          return (scalar, normData)
        }

        buffer.append((scalar, normData))
      } else {
        decompose(scalar, with: normData)
      }
    }

    // Sort the entire buffer based on the canonical combining class.
    buffer.sort()

    return buffer.next()
  }
}

extension Unicode._InternalNFD: Sequence {
  internal func makeIterator() -> Iterator {
    Iterator(
      index: base.startIndex,
      unicodeScalars: base
    )
  }
}

extension StringProtocol {
  internal var _internalNFD: Unicode._InternalNFD<Self> {
    Unicode._InternalNFD(base: unicodeScalars)
  }
}
