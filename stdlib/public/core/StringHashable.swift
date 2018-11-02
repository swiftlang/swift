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
  internal func hashASCII(into core: inout Hasher.Core) {
    core.combine(bytes: rawBuffer)
  }
}

extension BidirectionalCollection where Element == UInt16, SubSequence == Self {
  internal func hashUTF16(into core: inout Hasher.Core) {
    for i in self.indices {
      let cu = self[i]
      let cuIsASCII = cu <= 0x7F
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)

      if cuIsASCII && isSingleSegmentScalar {
        core.combine(UInt8(truncatingIfNeeded: cu))
      } else {
        for encodedScalar in Unicode._ParsingIterator(
          codeUnits: _NormalizedCodeUnitIterator(self[i..<endIndex]),
          parser: Unicode.UTF16.ForwardParser()
        ) {
          let transcoded = Unicode.UTF8.transcode(
            encodedScalar, from: Unicode.UTF16.self
          ).unsafelyUnwrapped // never fails
          let (bytes, count) = transcoded._bytes
          core.combine(bytes: bytes, count: count)
        }
        return
      }
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  internal func hash(into hasher: inout Hasher) {
    self.hashASCII(into: &hasher._core)
    hasher._core.combine(0xFF as UInt8) // terminator
  }

  internal func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    return Hasher._hash(seed: seed, bytes: rawBuffer)
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  internal func hash(into hasher: inout Hasher) {
    self.hashUTF16(into: &hasher._core)
    hasher._core.combine(0xFF as UInt8) // terminator
  }

  internal func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    var core = Hasher.Core(seed: seed)
    self.hashUTF16(into: &core)
    return Int(truncatingIfNeeded: core.finalize())
  }
}

extension _UnmanagedOpaqueString {
  internal func hash(into hasher: inout Hasher) {
    self.hashUTF16(into: &hasher._core)
    hasher._core.combine(0xFF as UInt8) // terminator
  }

  internal func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    var core = Hasher.Core(seed: seed)
    self.hashUTF16(into: &core)
    return Int(truncatingIfNeeded: core.finalize())
  }
}

extension _SmallUTF8String {
  internal func hash(into hasher: inout Hasher) {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    if isASCII {
      self.withUnmanagedASCII { $0.hash(into: &hasher) }
      return
    }
    self.withUnmanagedUTF16 { $0.hash(into: &hasher) }
#endif // 64-bit
  }

  internal func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    if isASCII {
      return self.withUnmanagedASCII { $0._rawHashValue(seed: seed) }
    }
    return self.withUnmanagedUTF16 { $0._rawHashValue(seed: seed) }
#endif // 64-bit
  }
}

extension _StringGuts {
  @_effects(releasenone) // FIXME: Is this valid in the opaque case?
  @usableFromInline
  internal func hash(into hasher: inout Hasher) {
    if _isSmall {
      _smallUTF8String.hash(into: &hasher)
      return
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      _asOpaque().hash(into: &hasher)
      return
    }
    if isASCII {
      _unmanagedASCIIView.hash(into: &hasher)
      return
    }
    _unmanagedUTF16View.hash(into: &hasher)
  }

  @_effects(releasenone) // FIXME: Is this valid in the opaque case?
  @usableFromInline
  internal func hash(_ range: Range<Int>, into hasher: inout Hasher) {
    if _isSmall {
      _smallUTF8String[range].hash(into: &hasher)
      return
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      _asOpaque()[range].hash(into: &hasher)
      return
    }
    if isASCII {
      _unmanagedASCIIView[range].hash(into: &hasher)
      return
    }
    _unmanagedUTF16View[range].hash(into: &hasher)
  }

  @_effects(releasenone) // FIXME: Is this valid in the opaque case?
  @usableFromInline
  internal func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    if _isSmall {
      return _smallUTF8String._rawHashValue(seed: seed)
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      return _asOpaque()._rawHashValue(seed: seed)
    }
    if isASCII {
      return _unmanagedASCIIView._rawHashValue(seed: seed)
    }
    return _unmanagedUTF16View._rawHashValue(seed: seed)
  }

  @_effects(releasenone) // FIXME: Is this valid in the opaque case?
  @usableFromInline
  internal func _rawHashValue(
    _ range: Range<Int>,
    seed: (UInt64, UInt64)
  ) -> Int {
    if _isSmall {
      return _smallUTF8String[range]._rawHashValue(seed: seed)
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      return _asOpaque()[range]._rawHashValue(seed: seed)
    }
    if isASCII {
      return _unmanagedASCIIView[range]._rawHashValue(seed: seed)
    }
    return _unmanagedUTF16View[range]._rawHashValue(seed: seed)
  }
}

extension String : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    _guts.hash(into: &hasher)
  }

  @inlinable
  public func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    return _guts._rawHashValue(seed: seed)
  }
}

extension StringProtocol {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    _wholeString._guts.hash(_encodedOffsetRange, into: &hasher)
  }

  @inlinable
  public func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    return _wholeString._guts._rawHashValue(_encodedOffsetRange, seed: seed)
  }
}
