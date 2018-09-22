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

internal enum _StringComparison: Int {
  case less = -1
  case equal = 0
  case greater = 1
  
  @usableFromInline internal
  var flipped: _StringComparison {
    switch self {
      case .less: return .greater
      case .equal: return .equal
      case .greater: return .less
    }
  }
  
  @inline(__always)
  @usableFromInline internal
  init(signedNotation int: Int) {
    self = int < 0 ? .less : int == 0 ? .equal : .greater
  }
}

@inlinable // For pre-normal fast path
@_effects(readonly)
internal func _compareStringsEqual(_ lhs: String, _ rhs: String) -> Bool {
  // TODO(UTF8): Known-prenormal check / perf flag, memcmp call

  return _compareStringsCanonicalEquivalent(lhs, rhs)
}

@inlinable // For pre-normal fast path
@_effects(readonly)
internal func _compareStringsLess(_ lhs: String, _ rhs: String) -> Bool {
  // TODO(UTF8): Known-prenormal check / perf flag, memcmp call

  return _compareStringsCanonicalLess(lhs, rhs)
}

extension UnsafeBufferPointer where Element == UInt8 {
  @inlinable // FIXME(sil-serialize-all)
  internal func compareASCII(to other: UnsafeBufferPointer<UInt8>) -> Int {
    if self.baseAddress == other.baseAddress {
      return (self.count &- other.count).signum()
    }
    var cmp = Int(truncatingIfNeeded:
      _stdlib_memcmp(
        self.baseAddress!, other.baseAddress!,
        Swift.min(self.count, other.count)))
    if cmp == 0 {
      cmp = self.count &- other.count
    }
    return cmp.signum()
  }
}

extension _StringGuts {
  @inline(__always)
  @inlinable
  public func _bitwiseEqualTo(_ other: _StringGuts) -> Bool {
    return self.rawBits == other.rawBits
  }
}

@_effects(readonly)
internal func _compareStringsCanonical(
  _ lhs: String, _ rhs: String
) -> _StringComparison {
  // TODO(UTF8): fast paths, incremental comparison and normalization, etc.
  
  let left = lhs._guts
  let right = rhs._guts
  
  // Bitwise equality implies string equality
  if left._bitwiseEqualTo(right) {
    return .equal
  }
  
  if left.isKnownASCII && right.isKnownASCII {
    return left.withFastUTF8 { l in
      return right.withFastUTF8 { r in 
        return _StringComparison(signedNotation: l.compareASCII(to: r))
      }
    }
  }
  return _compareStringsSlow(lhs, rhs)
}

// internal func _findDiffIdx(

internal func _lexicographicalCompare(
  _ lhs: UInt8, _ rhs: UInt8
) -> _StringComparison {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

internal func _lexicographicalCompare(
  _ lhs: UInt16, _ rhs: UInt16
) -> _StringComparison {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}


@usableFromInline @inline(never)// @opaque slowish-path
@_effects(readonly)
internal func _compareStringsCanonicalEquivalent(
  _ lhs: String, _ rhs: String
) -> Bool {
  return _compareStringsCanonical(lhs, rhs) == .equal
}

@usableFromInline @inline(never)// @opaque slowish-path
@_effects(readonly)
internal func _compareStringsCanonicalLess(
  _ lhs: String, _ rhs: String
) -> Bool {
  return _compareStringsCanonical(lhs, rhs) == .less
}

internal func _lexicographicalCompare(
  _ lhs: Int, _ rhs: Int
) -> _StringComparison {
  // TODO: inspect code quality
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

@_effects(readonly)
internal func _lexicographicalCompare(
  _ lhs: Array<UInt8>, _ rhs: Array<UInt8>
) -> _StringComparison {
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

// @opaque
@_effects(readonly)
@inline(never) // slow-path
internal func _compareStringsSlow(
  _ lhs: String, _ rhs: String
) -> _StringComparison {
  // TODO(UTF8): fast paths, incremental comparison and normalization, etc.

  let left = lhs._guts
  let right = rhs._guts
  
  let lhsRange = lhs.startIndex..<lhs.endIndex
  let rhsRange = rhs.startIndex..<rhs.endIndex
  
  switch (left.isFastUTF8, right.isFastUTF8) {
  case (true, true):
    return left.withFastUTF8 { leftUTF8 in
      return right.withFastUTF8 { rightUTF8 in 
        var leftIterator = 
          _NormalizedUTF8CodeUnitIterator(leftUTF8, range: 0..<leftUTF8.count)
        let rightIterator =
          _NormalizedUTF8CodeUnitIterator(rightUTF8, range: 0..<rightUTF8.count)
        return leftIterator.compare(with: rightIterator)
      }
    }
  case (true, false):
    return left.withFastUTF8 { leftUTF8 in
      var leftIterator =
        _NormalizedUTF8CodeUnitIterator(leftUTF8, range: 0..<leftUTF8.count)
      let rightIterator =
        _NormalizedUTF8CodeUnitIterator(right, range: rhsRange)
      return leftIterator.compare(with: rightIterator)
    }
  case (false, true):
    return right.withFastUTF8 { rightUTF8 in
      var leftIterator =
        _NormalizedUTF8CodeUnitIterator(left, range: lhsRange)
      let rightIterator =
        _NormalizedUTF8CodeUnitIterator(rightUTF8, range: 0..<rightUTF8.count)
      return leftIterator.compare(with: rightIterator)
    }
  case (false, false):
    var leftIterator 
      = _NormalizedUTF8CodeUnitIterator(left, range: lhsRange)
    let rightIterator 
      = _NormalizedUTF8CodeUnitIterator(right, range: rhsRange)
    return leftIterator.compare(with: rightIterator)
  }
}
