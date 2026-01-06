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
    unsafe _internalInvariant(!UTF8.isContinuation(self[_unchecked: offset]))

    // Sub-300 latiny fast-path
    if unsafe self[_unchecked: offset] < 0xCC { return true }

    let cu = unsafe _decodeScalar(self, startingAt: offset).0
    return cu._isNFCStarter
  }

  internal func isOnUnicodeScalarBoundary(_ offset: Int) -> Bool {
    guard offset < count else {
      _internalInvariant(offset == count)
      return true
    }
    return unsafe !UTF8.isContinuation(self[offset])
  }
}

internal func _isScalarNFCQC(
  _ scalar: Unicode.Scalar,
  _ prevCCC: inout UInt8
) -> Bool {
  let normData = Unicode._NormData(scalar, fastUpperbound: 0x300)

  if prevCCC > normData.ccc, normData.ccc != 0 {
    return false
  }

  if !normData.isNFCQC {
    return false
  }

  prevCCC = normData.ccc
  return true
}

extension _StringGutsSlice {
  internal func _withNFCCodeUnits(_ f: (UInt8) throws -> Void) rethrows {
    let substring = String(_guts)[range]
    // Fast path: If we're already NFC (or ASCII), then we don't need to do
    // anything at all.
    if _fastPath(_guts.isNFC) {
      try substring.utf8.forEach(f)
      return
    }

    var isNFCQC = true
    var prevCCC: UInt8 = 0

    if _guts.isFastUTF8 {
      _fastNFCCheck(&isNFCQC, &prevCCC)

      // Because we have access to the fastUTF8, we can go through that instead
      // of accessing the UTF8 view on String.
      if isNFCQC {
        try unsafe withFastUTF8 {
          for unsafe byte in unsafe $0 {
            try f(byte)
          }
        }

        return
      }
    } else {
      for scalar in substring.unicodeScalars {
        if !_isScalarNFCQC(scalar, &prevCCC) {
          isNFCQC = false
          break
        }
      }

      if isNFCQC {
        for byte in substring.utf8 {
          try f(byte)
        }

        return
      }
    }

    for scalar in substring.unicodeScalars._internalNFC {
      try scalar.withUTF8CodeUnits {
        for unsafe byte in unsafe $0 {
          try f(byte)
        }
      }
    }
  }

  internal func _fastNFCCheck(_ isNFCQC: inout Bool, _ prevCCC: inout UInt8) {
    unsafe withFastUTF8 { utf8 in
      isNFCQC = unsafe _nfcQuickCheck(utf8, prevCCC: &prevCCC)
    }
  }
}

/// Run the Unicode NFC quick check algorithm, returns
internal func _nfcQuickCheck(
  _ utf8: UnsafeBufferPointer<UInt8>,
  prevCCC: inout UInt8
) -> Bool {
  var position = 0

  while position < utf8.count {
    // If our first byte is less than 0xCC, then it means we're under the
    // 0x300 scalar value and everything up to 0x300 is NFC already.
    if unsafe utf8[position] < 0xCC {
      // If our first byte is less than 0xC0, then it means it is ASCII
      // and only takes up a single byte.
      if unsafe utf8[position] < 0xC0 {
        position &+= 1
      } else {
        // Otherwise, this is a 2 byte < 0x300 sequence.
        position &+= 2
      }
      // ASCII always has ccc of 0.
      prevCCC = 0

      continue
    }

    let (scalar, len) = unsafe _decodeScalar(utf8, startingAt: position)

    guard _isScalarNFCQC(scalar, &prevCCC) else {
      return false
    }

    position &+= len
  }

  return true
}

