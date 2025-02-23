//===--- ArrayBody.swift - Data needed once per array ---------------------===//
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
//
//  Array storage begins with a Body and ends with a sequence of
//  contiguous Elements.  This struct describes the Body part.
//
//===----------------------------------------------------------------------===//

import SwiftShims

@frozen
@usableFromInline
internal struct _ArrayBody {
  @usableFromInline
  internal var _storage: _SwiftArrayBodyStorage

  @inlinable
  internal init(
    count: Int, capacity: Int, elementTypeIsBridgedVerbatim: Bool = false
  ) {
    _internalInvariant(count >= 0)
    _internalInvariant(capacity >= 0)

    _storage = _SwiftArrayBodyStorage(
      count: count,
      _capacityAndFlags:
        (UInt(truncatingIfNeeded: capacity) &<< 1) |
        (elementTypeIsBridgedVerbatim ? 1 : 0))
  }

  /// In principle ArrayBody shouldn't need to be default
  /// constructed, but since we want to claim all the allocated
  /// capacity after a new buffer is allocated, it's typical to want
  /// to update it immediately after construction.
  @inlinable
  internal init() {
    _storage = _SwiftArrayBodyStorage(count: 0, _capacityAndFlags: 0)
  }

  /// The number of elements stored in this Array.
  @inlinable
  internal var count: Int {
    get {
      return _assumeNonNegative(_storage.count)
    }
    set(newCount) {
      _storage.count = newCount
    }
  }

  /// The number of elements that can be stored in this Array without
  /// reallocation.
  @inlinable
  internal var capacity: Int {
    return Int(_capacityAndFlags &>> 1)
  }

  /// Is the Element type bitwise-compatible with some Objective-C
  /// class?  The answer is---in principle---statically-knowable, but
  /// I don't expect to be able to get this information to the
  /// optimizer before 1.0 ships, so we store it in a bit here to
  /// avoid the cost of calls into the runtime that compute the
  /// answer.
  @inlinable
  internal var elementTypeIsBridgedVerbatim: Bool {
    get {
      return (_capacityAndFlags & 0x1) != 0
    }
    set {
      _capacityAndFlags
        = newValue ? _capacityAndFlags | 1 : _capacityAndFlags & ~1
    }
  }

  /// Storage optimization: compresses capacity and
  /// elementTypeIsBridgedVerbatim together.
  @inlinable
  internal var _capacityAndFlags: UInt {
    get {
      return _storage._capacityAndFlags
    }
    set {
      _storage._capacityAndFlags = newValue
    }
  }
}
