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
  // NOT @_versioned
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
  // NOT @_versioned
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
