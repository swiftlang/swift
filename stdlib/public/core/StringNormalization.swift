//===--- StringNormalization.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Unicode.Scalar {
  // Normalization boundary - a place in a string where everything left of the
  // boundary can be normalized independently from everything right of the
  // boundary. The concatenation of each result is the same as if the entire
  // string had been normalized as a whole.
  //
  // Normalization segment - a sequence of code units between two normalization
  // boundaries (without any boundaries in the middle). Note that normalization
  // segments can, as a process of normalization, expand, contract, and even
  // produce new sub-segments.

  // Quick check if a scalar is an NFC segment starter.
  internal var _isNFCStarter: Bool {
    // Fast path: All scalars up to U+300 are NFC_QC and have boundaries
    // before them.
    let normData = Unicode._NormData(self, fastUpperbound: 0x300)
    return normData.ccc == 0 && normData.isNFCQC
  }
}

extension UnsafeBufferPointer where Element == UInt8 {
  internal func hasNormalizationBoundary(before offset: Int) -> Bool {
    if offset == 0 || offset == count {
      return true
    }
    _internalInvariant(!UTF8.isContinuation(self[_unchecked: offset]))

    // Sub-300 latiny fast-path
    if self[_unchecked: offset] < 0xCC { return true }

    let cu = _decodeScalar(self, startingAt: offset).0
    return cu._isNFCStarter
  }

  internal func isOnUnicodeScalarBoundary(_ offset: Int) -> Bool {
    guard offset < count else {
      _internalInvariant(offset == count)
      return true
    }
    return !UTF8.isContinuation(self[offset])
  }
}
