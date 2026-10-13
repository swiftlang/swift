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
internal struct _DequeSlot {
#if hasFeature(Embedded)
  @usableFromInline
#endif
  internal var position: Int

#if hasFeature(Embedded)
  @inlinable
#endif
  init(at position: Int) {
    assert(position >= 0)
    self.position = position
  }
}

extension _DequeSlot {
#if hasFeature(Embedded)
  @inlinable
#endif
  internal static var zero: Self { Self(at: 0) }

#if hasFeature(Embedded)
  @inlinable
#endif
  internal func advanced(by delta: Int) -> Self {
    Self(at: position &+ delta)
  }

#if hasFeature(Embedded)
  @inlinable
#endif
  internal func orIfZero(_ value: Int) -> Self {
    guard position > 0 else { return Self(at: value) }
    return self
  }
}

extension _DequeSlot: CustomStringConvertible {
#if hasFeature(Embedded)
  @usableFromInline
#endif
  internal var description: String {
    "@\(position)"
  }
}

extension _DequeSlot: Equatable {
#if hasFeature(Embedded)
  @inlinable
#endif
  static func ==(left: Self, right: Self) -> Bool {
    left.position == right.position
  }
}

extension _DequeSlot: Comparable {
#if hasFeature(Embedded)
  @inlinable
#endif
  static func <(left: Self, right: Self) -> Bool {
    left.position < right.position
  }
}

extension Range where Bound == _DequeSlot {
#if hasFeature(Embedded)
  @inlinable
#endif
  internal var _count: Int { upperBound.position - lowerBound.position }
}
