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

/// Initializes a `Set` from unique members.
///
/// Using a builder can be faster than inserting members into an empty
/// `Set`.
@_fixed_layout
public // SPI(Foundation)
struct _SetBuilder<Element: Hashable> {
  @usableFromInline
  internal var _target: _NativeSet<Element>
  @usableFromInline
  internal let _requestedCount: Int

  @inlinable
  public init(count: Int) {
    _target = _NativeSet(capacity: count)
    _requestedCount = count
  }

  @inlinable
  @inline(__always)
  public mutating func add(member: Element) {
    _precondition(_target.count < _requestedCount,
      "Can't add more members than promised")
    _target._unsafeInsertNew(member)
  }

  @inlinable
  public __consuming func take() -> Set<Element> {
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")
    return Set(_native: _target)
  }
}
