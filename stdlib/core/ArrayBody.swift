//===--- ArrayBody.swift - Data needed once per array ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Array storage begins with a Body and ends with a sequence of
//  contiguous Elements.  This struct describes the Body part.
//
//===----------------------------------------------------------------------===//

struct _ArrayBody {
  init(count: Int, capacity: Int, elementTypeIsBridgedVerbatim: Bool = false) {
    assert(count >= 0)
    assert(capacity >= 0)
    self.count = count
    self._capacityAndFlags
      = (UInt(capacity) << 1) | (elementTypeIsBridgedVerbatim ? 1 : 0)
  }

  /// In principle CountAndCapacity shouldn't need to be default
  /// constructed, but since we want to claim all the allocated
  /// capacity after a new buffer is allocated, it's typical to want
  /// to update it immediately after construction.
  init() {
    self.count = 0
    self._capacityAndFlags = 0
  }
  
  /// The number of elements stored in this Array
  var count: Int

  /// The number of elements that can be stored in this Array without
  /// reallocation.
  var capacity: Int {
    return Int(_capacityAndFlags >> 1)
  }

  /// Is the Element type bitwise-compatible with some Objective-C
  /// class?  The answer is---in principle---statically-knowable, but
  /// I don't expect to be able to get this information to the
  /// optimizer before 1.0 ships, so we store it in a bit here to
  /// avoid the cost of calls into the runtime that compute the
  /// answer.
  var elementTypeIsBridgedVerbatim: Bool {
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
  var _capacityAndFlags: UInt
}

