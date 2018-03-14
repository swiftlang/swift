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
  // null terminated strings need to hash differently than non-null terminated
  // ones. A partially filled ascii buffer should have 1s in the leftover space
  buffer._storage = UInt64.max
  return buffer
}

extension _UnmanagedString where CodeUnit == UInt8 {
  // NOT @_versioned
  @effects(releasenone)
  internal func hashASCII(into hasher: inout _Hasher) {
    var buffer = _emptyASCIIHashBuffer()
    for c in self {
      if buffer.count < buffer.capacity {
        buffer.append(UInt8(truncatingIfNeeded: c))
      }

      if buffer.count == buffer.capacity {
        hasher.append(buffer._storage)
        buffer = _emptyASCIIHashBuffer()
      }
    }

    if buffer.count > 0 {
      hasher.append(buffer._storage)
    }
  }
}

extension BidirectionalCollection where Element == UInt16, SubSequence == Self {
  // NOT @_versioned
  internal func hashUTF16(into hasher: inout _Hasher) {
    var buffer = _emptyASCIIHashBuffer()

    var i = startIndex
    for cu in self {
      defer { i = index(after: i) }
      let cuIsASCII = cu <= 0x7F
      let isSingleSegmentScalar = self.hasNormalizationBoundary(after: i)

      guard cuIsASCII && isSingleSegmentScalar else {
        if buffer.count != 0 {
          hasher.append(buffer._storage)
          buffer = _emptyASCIIHashBuffer()
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
        hasher.append(buffer._storage)
        buffer = _emptyASCIIHashBuffer()
      }
    }

    if buffer.count > 0 {
      hasher.append(buffer._storage)
      buffer = _emptyASCIIHashBuffer()
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  @effects(releasenone)
  @_versioned
  internal func hash(into hasher: inout _Hasher) {
    self.hashASCII(into: &hasher)
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  @effects(releasenone)
  @_versioned
  internal func hash(into hasher: inout _Hasher) {
    self.hashUTF16(into: &hasher)
  }
}

extension _UnmanagedOpaqueString {
  @_versioned
  internal func hash(into hasher: inout _Hasher) {
    self.hashUTF16(into: &hasher)
  }
}

extension _StringGuts {
  @_versioned
  @effects(releasenone) // FIXME: Is this guaranteed in the opaque case?
  internal func _hash(into hasher: inout _Hasher) {
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

  @_versioned
  @effects(releasenone) // FIXME: Is this guaranteed in the opaque case?
  internal func _hash(_ range: Range<Int>, into hasher: inout _Hasher) {
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
}

extension String : Hashable {
  /// The string's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  @_inlineable
  public var hashValue: Int {
    return _hashValue(for: self)
  }

  @_inlineable
  public func _hash(into hasher: inout _Hasher) {
    _guts._hash(into: &hasher)
  }
}

extension StringProtocol {
  @_inlineable
  public var hashValue : Int {
    return _hashValue(for: self)
  }

  @_inlineable
  public func _hash(into hasher: inout _Hasher) {
    _wholeString._guts._hash(_encodedOffsetRange, into: &hasher)
  }
}
