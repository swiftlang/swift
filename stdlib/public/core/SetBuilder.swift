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
  public mutating func add(member: Element) {
    _precondition(_target.count < _requestedCount,
      "Can't add more members than promised")
    _target.insertNew(member, isUnique: true)
  }

  @inlinable
  public mutating func take() -> Set<Element> {
    _precondition(_target.capacity > 0 || _requestedCount == 0,
      "Cannot take the result twice")
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")

    // Prevent taking the result twice.
    var result = _NativeSet<Element>()
    swap(&result, &_target)
    return Set(_native: result)
  }
}
