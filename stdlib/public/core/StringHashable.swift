//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

extension _UnmanagedString where CodeUnit == UInt8 {
  // FIXME: cannot be marked @_versioned. See <rdar://problem/34438258>
  // @_inlineable // FIXME(sil-serialize-all)
  // @_versioned // FIXME(sil-serialize-all)
  internal func hashASCII() -> Int {
    return Int(Int64(bitPattern:
      _SipHash13Context.hash(
        data: start, dataByteCount: count, key: _Hashing.secretKey
      )
    ))
  }
}

internal func _castASCIIBuffer(
  _ ptr: UnsafePointer<_FixedArray16<HashType>>
) -> UnsafeRawPointer {
  return UnsafeRawPointer(ptr)
}

typealias HashType = UInt
extension _UnmanagedString where CodeUnit == UInt16 {
  // FIXME: cannot be marked @_versioned. See <rdar://problem/34438258>
  // @_inlineable // FIXME(sil-serialize-all)
  // @_versioned // FIXME(sil-serialize-all)
  internal static func hashUTF16(
    _ string: UnsafeBufferPointer<UInt8>,

    var buf = _FixedArray16<HashType>(allZeros: ())
    var idx = 0

    for (i, cu) in self.enumerated() {
      let cuIsASCII = cu <= 0x7F
  ) {
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)
      guard cuIsASCII && isSingleSegmentScalar else {
        if idx != 0 {
          let ptr = _castASCIIBuffer(&buf)
          hasher.append(ptr, byteCount: idx)
        }

        let codeUnitSequence = IteratorSequence(
          _NormalizedCodeUnitIterator(self[i...])
        )
        for element in codeUnitSequence {
          hasher.append(HashType(element))
        }
        break
      }

      buf[idx] = HashType(cu)
      idx += 1

      if idx >= buf.capacity {
        let ptr = _castASCIIBuffer(&buf)
        hasher.append(ptr, byteCount: buf.capacity)
        idx = 0
      }

      hasher.append(HashType(cu))
    }
  }
}

extension _UnmanagedOpaqueString {
  // FIXME: cannot be marked @_versioned. See <rdar://problem/34438258>
  // @_inlineable // FIXME(sil-serialize-all)
  // @_versioned // FIXME(sil-serialize-all)
  internal static func hashUTF16(
    _ string: UnsafeBufferPointer<UInt16>,

    var buf = _FixedArray16<HashType>(allZeros: ())
    var idx = 0

    into hasher: inout _Hasher
  ) {
    for (i, cu) in self.enumerated() {

      let cuIsASCII = cu <= 0x7F
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)

      guard cuIsASCII && isSingleSegmentScalar else {
        if idx != 0 {
          let ptr = _castASCIIBuffer(&buf)
          hasher.append(ptr, byteCount: idx)
        }

        let codeUnitSequence = IteratorSequence(
          _NormalizedCodeUnitIterator(self[i...])
        )
        for element in codeUnitSequence {
          hasher.append(HashType(element))
        }
        break
      }

      buf[idx] = HashType(cu)
      idx += 1

      if idx >= buf.capacity {
        let ptr = _castASCIIBuffer(&buf)
        hasher.append(ptr, byteCount: buf.capacity)
        idx = 0
      }

      hasher.append(Int(truncatingIfNeeded: cu))
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  @_versioned
  internal func computeHashValue() -> Int {
    return self.hashASCII()
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  @_versioned
  internal func computeHashValue() -> Int {
    return self.hashUTF16()
  }
}

extension _UnmanagedOpaqueString {
  @_versioned
  internal func computeHashValue() -> Int {
    return self.hashUTF16()
  }
}

extension _StringGuts {
  //
  // FIXME(TODO: JIRA): HACK HACK HACK: Work around for ARC :-(
  //
  @_versioned
  @effects(readonly)
  @inline(never) // Hide the CF dependency
  internal static func _computeHashValue(
    _unsafeBitPattern: _RawBitPattern
  ) -> Int {
    return _StringGuts(rawBits: _unsafeBitPattern)._computeHashValue()
  }

  @_versioned
  // TODO: After removing above hack: @inline(never) // Hide the CF dependency
  internal func _computeHashValue() -> Int {
    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      return _asOpaque().computeHashValue()
    }
    if isASCII {
      return _unmanagedASCIIView.computeHashValue()
    }
    return _unmanagedUTF16View.computeHashValue()
  }

  @_versioned
  // TODO: After removing above hack: @inline(never) // Hide the CF dependency
  internal func _computeHashValue(_ range: Range<Int>) -> Int {
    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      return _asOpaque()[range].computeHashValue()
    }
    if isASCII {
      return _unmanagedASCIIView[range].computeHashValue()
    }
    return _unmanagedUTF16View[range].computeHashValue()
  }
}

extension String : Hashable {
  /// The string's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  @_inlineable // FIXME(sil-serialize-all)
  public var hashValue: Int {
    defer { _fixLifetime(self) }
    let gutsBits = _guts.rawBits
    return _StringGuts._computeHashValue(_unsafeBitPattern: gutsBits)
  }

  @_inlineable
  public func _hash(into hasher: inout _Hasher) {
    hasher.append(self.hashValue)
  }
}

extension StringProtocol {
  @_inlineable // FIXME(sil-serialize-all)
  public var hashValue : Int {
    return _wholeString._guts._computeHashValue(_encodedOffsetRange)
  }

  @_inlineable
  public func _hash(into hasher: inout _Hasher) {
    hasher.append(self.hashValue)
  }
}
