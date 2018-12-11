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
    _precondition(_target.count < _requestedCount,
      "Can't add more members than promised")
    _target._unsafeInsertNew(key: newKey, value: value)
  }

  @inlinable
  public __consuming func take() -> Dictionary<Key, Value> {
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")
    return Dictionary(_native: _target)
  }
}

extension Dictionary {
  /// Creates a new dictionary with the specified capacity, then calls the given
  /// closure to initialize its contents.
  ///
  /// Foundation uses this initializer to bridge the contents of an NSDictionary
  /// instance without allocating a pair of intermediary buffers.  Pass the
  /// required capacity and a closure that can intialize the dictionary's
  /// elements. The closure must return `c`, the number of initialized elements
  /// in both buffers, such that the elements in the range `0..<c` are
  /// initialized and the elements in the range `c..<capacity` are
  /// uninitialized. The resulting dictionary has a `count` equal to `c`.
  ///
  /// The closure must not add any duplicate keys to the keys buffer.  The
  /// buffers passed to the closure are only valid for the duration of the call.
  /// After the closure returns, this initializer moves all initialized elements
  /// into their correct buckets.
  ///
  /// - Parameters:
  ///   - capacity: The capacity of the new dictionary.
  ///   - body: A closure that can initialize the dictionary's elements. This
  ///     closure must return the count of the initialized elements, starting at
  ///     the beginning of the buffer.
  @inlinable
  public // SPI(Foundation)
  init(
    _unsafeUninitializedCapacity capacity: Int,
    initializingWith initializer: (
      _ keys: UnsafeMutableBufferPointer<Key>,
      _ values: UnsafeMutableBufferPointer<Value>,
      _ initializedCount: inout Int
    ) -> Void
  ) {
    let native = _NativeDictionary<Key, Value>(capacity: capacity)
    let keys = UnsafeMutableBufferPointer<Key>(
      start: native._keys,
      count: capacity)
    let values = UnsafeMutableBufferPointer<Value>(
      start: native._values,
      count: capacity)
    var count = 0
    initializer(keys, values, &count)
    _precondition(count >= 0 && count <= capacity)

    // Hash elements into the correct buckets.
    var bucket = _HashTable.Bucket(offset: 0)
    while bucket.offset < count {
      if native.hashTable._isOccupied(bucket) {
        // We've moved an element here in a previous iteration.
        bucket.offset += 1
        continue
      }
      let hashValue = native.hashValue(for: native._keys[bucket.offset])
      let target: _HashTable.Bucket
      if _isDebugAssertConfiguration() {
        let (b, found) = native.find(
          native._keys[bucket.offset],
          hashValue: hashValue)
        guard !found else {
          _preconditionFailure("Duplicate keys found")
        }
        native.hashTable.insert(b)
        target = b
      } else {
        target = native.hashTable.insertNew(hashValue: hashValue)
      }
      if target < bucket || target.offset >= count {
        // Target bucket is uninitialized
        native.moveEntry(from: bucket, to: target)
        bucket.offset += 1
      } else if target == bucket {
        // Already in place
        bucket.offset += 1
      } else {
        // Swap into place, then try again with the swapped-in value.
        native.swapEntry(target, with: bucket)
      }
    }
    native._storage._count = count
    self = Dictionary(_native: native)
  }
}
