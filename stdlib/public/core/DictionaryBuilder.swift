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
    allowingDuplicates: Bool,
    initializingWith initializer: (
      _ keys: UnsafeMutableBufferPointer<Key>,
      _ values: UnsafeMutableBufferPointer<Value>,
      _ initializedCount: inout Int
    ) -> Void
  ) {
    self.init(_native: _NativeDictionary(
        _unsafeUninitializedCapacity: capacity,
        allowingDuplicates: allowingDuplicates,
        initializingWith: initializer))
  }
}

extension _NativeDictionary {
  @inlinable
  internal init(
    _unsafeUninitializedCapacity capacity: Int,
    allowingDuplicates: Bool,
    initializingWith initializer: (
      _ keys: UnsafeMutableBufferPointer<Key>,
      _ values: UnsafeMutableBufferPointer<Value>,
      _ initializedCount: inout Int
    ) -> Void
  ) {
    self.init(capacity: capacity)
    var count = 0
    initializer(
      UnsafeMutableBufferPointer(start: _keys, count: capacity),
      UnsafeMutableBufferPointer(start: _values, count: capacity),
      &count)
    _precondition(count >= 0 && count <= capacity)
    _storage._count = count

    // Hash initialized elements and move each of them into their correct
    // buckets.
    //
    // - The storage is separated into three regions: "before bucket", "bucket
    //   up to count", and "count and after".
    //
    // - Everything in the middle region, "bucket up to count" is either
    //   unprocessed or in use. There are no uninitialized entries in it.
    //
    // - Everything in "before bucket" and "count and after" is either
    //   uninitialized or in use. These regions work like regular dictionary
    //   storage.
    //
    // - "in use" is tracked by the bitmap in `hashTable`, the same way it would
    //   be for a working Dictionary.
    //
    // Each iteration of the loop below processes an unprocessed element, and/or
    // reduces the size of the "bucket up to count" region, while ensuring the
    // above invariants.
    var bucket = _HashTable.Bucket(offset: 0)
    while bucket.offset < count {
      if hashTable._isOccupied(bucket) {
        // We've moved an element here in a previous iteration.
        bucket.offset += 1
        continue
      }
      // Find the target bucket for this entry and mark it as in use.
      let target = _preloadEntry(
        in: bucket,
        allowingDuplicates: allowingDuplicates)

      if target < bucket || target.offset >= count {
        // The target is in either the "before bucket" or the "count and after"
        // region.  We can simply move the entry, leaving behind an
        // uninitialized bucket.
        moveEntry(from: bucket, to: target)
        // Restore invariants by moving the region boundary such that the bucket
        // becomes part of the "before bucket" region.
        bucket.offset += 1
      } else if target == bucket {
        // Already in place.
        bucket.offset += 1
      } else {
        // The target bucket is also in the middle region. Swap the current
        // item into place, then try again with the swapped-in value, so that we
        // don't lose it.
        swapEntry(target, with: bucket)
      }
    }
    // When the middle region disappears, we're left with a valid Dictionary.
  }

  /// Find and return the correct bucket for the entry stored in `bucket`, while
  /// also preparing to place it there. This is for use in the bulk loading
  /// initializer, `init(_unsafeUninitializedCapacity:, allowingDuplicates:,
  /// initializingWith:)`.
  ///
  /// When this function returns, it is guaranteed that
  /// - the returned bucket is marked occupied in the hash table's bitmap,
  /// - any duplicate that was already in the returned bucket is destroyed, and
  /// - the original item is still in its original bucket.
  @inlinable
  @inline(__always)
  internal func _preloadEntry(
    in bucket: Bucket,
    allowingDuplicates: Bool
  ) -> Bucket {
    if _isDebugAssertConfiguration() || allowingDuplicates {
      let (b, found) = find(_keys[bucket.offset])
      if found {
        _precondition(allowingDuplicates, "Duplicate keys found")
        // Discard existing key & value in preparation of overwriting it.
        uncheckedDestroy(at: b)
        _storage._count -= 1
      } else {
        hashTable.insert(b)
      }
      return b
    }

    let hashValue = self.hashValue(for: _keys[bucket.offset])
    return hashTable.insertNew(hashValue: hashValue)
  }
}
