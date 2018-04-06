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

func _emptyASCIIHashBuffer() -> _UIntBuffer<UInt64, UInt8> {
  var buffer = _UIntBuffer<UInt64, UInt8>()
  // We don't want the unused bits of a partially filled buffer to collide
  // with trailing nuls when hashing
  buffer._storage = UInt64.max
  return buffer
}

internal struct ASCIIHasher {
  private var buffer = _emptyASCIIHashBuffer()

  internal mutating func consume() -> UInt64? {
    if !buffer.isEmpty {
      defer { resetBuffer() }
      return buffer._storage
    }
    return nil
  }

  private mutating func resetBuffer() {
    buffer = _emptyASCIIHashBuffer()
  }

  internal mutating func append(_ c: UInt8) -> UInt64? {
    if buffer.count < buffer.capacity {
      buffer.append(c)
    }

    if buffer.count == buffer.capacity {
      defer { resetBuffer() }
      return buffer._storage
    }
    return nil
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  // NOT @usableFromInline
  @effects(releasenone)
  internal func hashASCII(into hasher: inout _Hasher) {
    var asciiHasher = ASCIIHasher()
    for c in self {
      if let combined = asciiHasher.append(UInt8(truncatingIfNeeded: c)) {
        hasher.append(combined)
      }
    }

    if let combined = asciiHasher.consume() {
      hasher.append(combined)
    }
  }
}

extension BidirectionalCollection where Element == UInt16, SubSequence == Self {
  // NOT @usableFromInline
  internal func hashUTF16(into hasher: inout _Hasher) {
    var asciiHasher = ASCIIHasher()

    for i in self.indices {
      let cu = self[i]
      let cuIsASCII = cu <= 0x7F
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)

      guard cuIsASCII && isSingleSegmentScalar else {
        if let combined = asciiHasher.consume() {
          hasher.append(combined)
        }

        let codeUnitSequence = IteratorSequence(
          _NormalizedCodeUnitIterator(self[i..<endIndex])
        )
        for element in codeUnitSequence {
          hasher.append(UInt(element))
        }
        return
      }

      if let combined = asciiHasher.append(UInt8(truncatingIfNeeded: cu)) {
        hasher.append(combined)
      }
    }

    if let combined = asciiHasher.consume() {
      hasher.append(combined)
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  @effects(releasenone)
  @usableFromInline
  internal func computeHashValue(into hasher: inout _Hasher) {
    self.hashASCII(into: &hasher)
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  @effects(releasenone)
  @usableFromInline
  internal func computeHashValue(into hasher: inout _Hasher) {
    self.hashUTF16(into: &hasher)
  }
}

extension _UnmanagedOpaqueString {
  @usableFromInline
  internal func computeHashValue(into hasher: inout _Hasher) {
    self.hashUTF16(into: &hasher)
  }
}

extension _SmallUTF8String {
  @inlinable
  internal func computeHashValue(into hasher: inout _Hasher) {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    if isASCII {
      return self.withUnmanagedASCII { $0.computeHashValue(into: &hasher) }
    }
    return self.withUnmanagedUTF16 { $0.computeHashValue(into: &hasher) }
#endif // 64-bit
  }
}

extension _StringGuts {
  @usableFromInline
  @effects(releasenone) // FIXME: Is this guaranteed in the opaque case?
  internal func _hash(into hasher: inout _Hasher) {
    if _isSmall {
      return _smallUTF8String.computeHashValue(into: &hasher)
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      _asOpaque().computeHashValue(into: &hasher)
      return
    }
    if isASCII {
      _unmanagedASCIIView.computeHashValue(into: &hasher)
      return
    }
    _unmanagedUTF16View.computeHashValue(into: &hasher)
  }

  @usableFromInline
  @effects(releasenone) // FIXME: Is this guaranteed in the opaque case?
  internal func _hash(_ range: Range<Int>, into hasher: inout _Hasher) {
    if _isSmall {
      return _smallUTF8String[range].computeHashValue(into: &hasher)
    }

    defer { _fixLifetime(self) }
    if _slowPath(_isOpaque) {
      _asOpaque()[range].computeHashValue(into: &hasher)
      return
    }
    if isASCII {
      _unmanagedASCIIView[range].computeHashValue(into: &hasher)
      return
    }
    _unmanagedUTF16View[range].computeHashValue(into: &hasher)
  }
}

extension String : Hashable {
  /// The string's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  @inlinable
  public var hashValue: Int {
    return _hashValue(for: self)
  }

  @inlinable
  public func _hash(into hasher: inout _Hasher) {
    _guts._hash(into: &hasher)
  }
}

extension StringProtocol {
  @inlinable
  public var hashValue : Int {
    return _hashValue(for: self)
  }

  @inlinable
  public func _hash(into hasher: inout _Hasher) {
    _wholeString._guts._hash(_encodedOffsetRange, into: &hasher)
  }
}
