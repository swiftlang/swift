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


extension _FixedArray8 where T == UInt8 {
  func combinedUInt64() -> UInt64 {
    var buffer = self
    while buffer.count < buffer.capacity {
      buffer.append(0xFF)
    }
    var combined: UInt64 = 0
    for value in buffer {
      combined = (combined << 8) | UInt64(value)
    }
    return combined
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  // FIXME: cannot be marked @_versioned. See <rdar://problem/34438258>
  // @_inlineable // FIXME(sil-serialize-all)
  // @_versioned // FIXME(sil-serialize-all)
  @effects(releasenone)
  internal func hashASCII(into hasher: inout _Hasher) {
    var buffer = _FixedArray8<UInt8>()
    for c in self {
      if buffer.count < buffer.capacity {
        buffer.append(UInt8(truncatingIfNeeded: c))
      }

      if buffer.count == buffer.capacity {
        hasher.append(buffer.combinedUInt64())
        buffer.count = 0
      }
    }

    if buffer.count > 0 {
      hasher.append(buffer.combinedUInt64())
    }
  }
}

extension BidirectionalCollection where Element == UInt16, SubSequence == Self {
  // FIXME: cannot be marked @_versioned. See <rdar://problem/34438258>
  // @_inlineable // FIXME(sil-serialize-all)
  // @_versioned // FIXME(sil-serialize-all)
  internal func hashUTF16(into hasher: inout _Hasher) {
    var buffer = _FixedArray8<UInt8>()

    var i = startIndex
    for cu in self {
      defer { i = index(after: i) }
      let cuIsASCII = cu <= 0x7F
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)

      guard cuIsASCII && isSingleSegmentScalar else {
        if buffer.count != 0 {
          let combined = buffer.combinedUInt64()
          hasher.append(combined)
          buffer.count = 0
        }

        let codeUnitSequence = IteratorSequence(
          _NormalizedCodeUnitIterator(self[i..<endIndex])
        )
        for element in codeUnitSequence {
          hasher.append(UInt(element))
        }
        return
      }

      buffer.append(UInt8(truncatingIfNeeded: cu))

      if buffer.count >= buffer.capacity {
        let combined = buffer.combinedUInt64()
        hasher.append(combined)
        buffer.count = 0
      }
    }

    if buffer.count > 0 {
      let combined = buffer.combinedUInt64()
      hasher.append(combined)
      buffer.count = 0
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  @_versioned
  internal func computeHashValue() -> Int {
    var hasher = _Hasher()
    self.hashASCII(into: &hasher)
    return hasher.finalize()
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  @_versioned
  internal func computeHashValue() -> Int {
    var hasher = _Hasher()
    self.hashUTF16(into: &hasher)
    return hasher.finalize()
  }
}

extension _UnmanagedOpaqueString {
  @_versioned
  internal func computeHashValue() -> Int {
    var hasher = _Hasher()
    self.hashUTF16(into: &hasher)
    return hasher.finalize())
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
