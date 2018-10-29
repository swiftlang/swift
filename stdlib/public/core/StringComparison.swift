//===----------------------------------------------------------------------===//
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

import SwiftShims

@_frozen
@usableFromInline
internal enum _StringComparisonResult: Int {
  case less = -1
  case equal = 0
  case greater = 1

  @inlinable
  internal var flipped: _StringComparisonResult {
    @inline(__always) get {
      return _StringComparisonResult(
        rawValue: -self.rawValue)._unsafelyUnwrappedUnchecked
    }
  }

  @inlinable @inline(__always)
  internal init(signedNotation int: Int) {
    self = int < 0 ? .less : int == 0 ? .equal : .greater
  }
}

extension _SlicedStringGuts {
  @inline(__always)
  @_effects(readonly)
  internal func withNFCCodeUnits<R>(
    _ f: (_NormalizedUTF8CodeUnitIterator) throws -> R
  ) rethrows -> R {
    if self.isNFCFastUTF8 {
      // TODO(UTF8 perf): Faster iterator if we're already normal
      return try self.withFastUTF8 {
        return try f(_NormalizedUTF8CodeUnitIterator($0, range: 0..<$0.count))
      }
    }
    if self.isFastUTF8 {
      return try self.withFastUTF8 {
        return try f(_NormalizedUTF8CodeUnitIterator($0, range: 0..<$0.count))
      }
    }
    return try f(_NormalizedUTF8CodeUnitIterator(
      foreign: self._guts, range: self.range))
  }
}

// Perform a binary comparison of bytes in memory. Return value is negative if
// less, 0 if equal, positive if greater.
@inlinable @inline(__always) // Memcmp wrapper
internal func _binaryCompare<UInt8>(
  _ lhs: UnsafeBufferPointer<UInt8>, _ rhs: UnsafeBufferPointer<UInt8>
) -> Int {
  var cmp = Int(truncatingIfNeeded:
    _stdlib_memcmp(
      lhs.baseAddress._unsafelyUnwrappedUnchecked,
      rhs.baseAddress._unsafelyUnwrappedUnchecked,
      Swift.min(lhs.count, rhs.count)))
  if cmp == 0 {
    cmp = lhs.count &- rhs.count
  }
  return cmp
}

// Double dispatch functions
extension _SlicedStringGuts {
  @usableFromInline
  @_effects(readonly)
  internal func compare(
    with other: _SlicedStringGuts
  ) -> _StringComparisonResult {
    if _fastPath(self.isNFCFastUTF8 && other.isNFCFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      return self.withFastUTF8 { nfcSelf in
        return other.withFastUTF8 { nfcOther in
          return _StringComparisonResult(
            signedNotation: _binaryCompare(nfcSelf, nfcOther))
        }
      }
    }

    return _slowCompare(with: other)
  }

  @inline(never) // opaque slow-path
  @_effects(readonly)
  internal func _slowCompare(
    with other: _SlicedStringGuts
  ) -> _StringComparisonResult {
    return self.withNFCCodeUnits {
      var selfIter = $0
      return other.withNFCCodeUnits {
        var otherIter = $0
        return selfIter.compare(with: otherIter)
      }
    }
  }
}

internal func _lexicographicalCompare(
  _ lhs: UInt8, _ rhs: UInt8
) -> _StringComparisonResult {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

internal func _lexicographicalCompare(
  _ lhs: UInt16, _ rhs: UInt16
) -> _StringComparisonResult {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

internal func _lexicographicalCompare(
  _ lhs: Int, _ rhs: Int
) -> _StringComparisonResult {
  // TODO: inspect code quality
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

@_effects(readonly)
internal func _lexicographicalCompare(
  _ lhs: Array<UInt8>, _ rhs: Array<UInt8>
) -> _StringComparisonResult {
  // Check for a difference in overlapping contents
  let count = Swift.min(lhs.count, rhs.count)
  for idx in 0..<count {
    let lhsValue = lhs[idx]
    let rhsValue = rhs[idx]
    guard lhsValue == rhsValue else {
      return lhsValue < rhsValue ? .less : .greater
    }
  }

  // Otherwise, the longer string is greater
  if lhs.count == rhs.count { return .equal }
  return lhs.count < rhs.count ? .less : .greater
}

