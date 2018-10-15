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

@usableFromInline // @opaque
@_effects(readonly)
internal func _compareStringsCanonicalEquivalent(
  _ lhs: String, _ rhs: String
) -> Bool {
  // TODO(UTF8): fast paths, incremental comparison and normalization, etc.

  return _compareStringsSlow(lhs, rhs) == .equal
}

@usableFromInline @inline(never)// @opaque slowish-path
@_effects(readonly)
internal func _compareStringsCanonicalLess(
  _ lhs: String, _ rhs: String
) -> Bool {
  // TODO(UTF8): fast paths, incremental comparison and normalization, etc.

  return _compareStringsSlow(lhs, rhs) == .less
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

  // FIXME(UTF8 perf): This is all a horribly slow implementation...

  let normalizedLHS = lhs._normalize()
  let normalizedRHS = rhs._normalize()
  return _lexicographicalCompare(normalizedLHS, normalizedRHS)
}
