//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

/// This file is copied from swift-collections and should not be modified here.
/// Rather all changes should be made to swift-collections and copied back.

import Swift

#if hasFeature(Embedded)
@usableFromInline
#endif
internal struct _DequeBufferHeader {
  var capacity: Int

  var count: Int

  var startSlot: _DequeSlot

#if hasFeature(Embedded)
  @usableFromInline
#endif
  init(capacity: Int, count: Int, startSlot: _DequeSlot) {
    self.capacity = capacity
    self.count = count
    self.startSlot = startSlot
    _checkInvariants()
  }

  #if COLLECTIONS_INTERNAL_CHECKS
#if hasFeature(Embedded)
  @usableFromInline
#endif
  internal func _checkInvariants() {
    precondition(capacity >= 0)
    precondition(count >= 0 && count <= capacity)
    precondition(startSlot.position >= 0 && startSlot.position <= capacity)
  }
  #else
#if hasFeature(Embedded)
  @usableFromInline
#endif
  internal func _checkInvariants() {}
  #endif // COLLECTIONS_INTERNAL_CHECKS
}

extension _DequeBufferHeader: CustomStringConvertible {
#if hasFeature(Embedded)
  @usableFromInline
#endif
  internal var description: String {
    "(capacity: \(capacity), count: \(count), startSlot: \(startSlot))"
  }
}
