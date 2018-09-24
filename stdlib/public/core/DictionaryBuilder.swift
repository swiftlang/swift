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

/// Initializes a `Dictionary` from unique members.
///
/// Using a builder can be faster than inserting members into an empty
/// `Dictionary`.
@_fixed_layout
public // SPI(Foundation)
struct _DictionaryBuilder<Key: Hashable, Value> {
  @usableFromInline
  internal var _target: _NativeDictionary<Key, Value>
  @usableFromInline
  internal let _requestedCount: Int

  @inlinable
  public init(count: Int) {
    _target = _NativeDictionary(capacity: count)
    _requestedCount = count
  }

  @inlinable
  public mutating func add(key newKey: Key, value: Value) {
    _target.insertNew(key: newKey, value: value)
  }

  @inlinable
  public mutating func take() -> Dictionary<Key, Value> {
    _precondition(_target.capacity > 0 || _requestedCount == 0,
      "Cannot take the result twice")
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")

    // Prevent taking the result twice.
    var result = _NativeDictionary<Key, Value>()
    swap(&result, &_target)
    return Dictionary(_native: result)
  }
}
