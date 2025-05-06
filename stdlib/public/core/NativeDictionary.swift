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

/// A wrapper around __RawDictionaryStorage that provides most of the
/// implementation of Dictionary.
@usableFromInline
@frozen
@safe
internal struct _NativeDictionary<Key: Hashable, Value> {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  /// See this comments on __RawDictionaryStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline
  internal var _storage: __RawDictionaryStorage

  /// Constructs an instance from the empty singleton.
  @inlinable
  internal init() {
    unsafe self._storage = __RawDictionaryStorage.empty
  }

  /// Constructs a dictionary adopting the given storage.
  @inlinable
  internal init(_ storage: __owned __RawDictionaryStorage) {
    unsafe self._storage = storage
  }

  @inlinable
  internal init(capacity: Int) {
    if capacity == 0 {
      unsafe self._storage = __RawDictionaryStorage.empty
    } else {
      unsafe self._storage =
        _DictionaryStorage<Key, Value>.allocate(capacity: capacity)
    }
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_ cocoa: __owned __CocoaDictionary) {
    self.init(cocoa, capacity: cocoa.count)
  }

  @inlinable
  internal init(_ cocoa: __owned __CocoaDictionary, capacity: Int) {
    if capacity == 0 {
      unsafe self._storage = __RawDictionaryStorage.empty
    } else {
      _internalInvariant(cocoa.count <= capacity)
      unsafe self._storage =
        _DictionaryStorage<Key, Value>.convert(cocoa, capacity: capacity)
      for (key, value) in cocoa {
        insertNew(
          key: _forceBridgeFromObjectiveC(key, Key.self),
          value: _forceBridgeFromObjectiveC(value, Value.self))
      }
    }
  }
#endif
}

@available(*, unavailable)
extension _NativeDictionary: Sendable {}

extension _NativeDictionary { // Primitive fields
  @usableFromInline
  internal typealias Bucket = _HashTable.Bucket

  @inlinable
  internal var capacity: Int {
    @inline(__always)
    get {
      return unsafe _assumeNonNegative(_storage._capacity)
    }
  }

  @inlinable
  internal var hashTable: _HashTable {
    @inline(__always) get {
      return unsafe _storage._hashTable
    }
  }

  @inlinable
  internal var age: Int32 {
    @inline(__always) get {
      return unsafe _storage._age
    }
  }

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable
  internal var _keys: UnsafeMutablePointer<Key> {
    return unsafe _storage._rawKeys.assumingMemoryBound(to: Key.self)
  }

  @inlinable
  internal var _values: UnsafeMutablePointer<Value> {
    return unsafe _storage._rawValues.assumingMemoryBound(to: Value.self)
  }

  @inlinable
  @inline(__always)
  internal func invalidateIndices() {
    unsafe _storage._age &+= 1
  }
}

extension _NativeDictionary { // Low-level unchecked operations
  @inlinable
  @inline(__always)
  @unsafe
  internal func uncheckedKey(at bucket: Bucket) -> Key {
    defer { _fixLifetime(self) }
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    return unsafe _keys[bucket.offset]
  }

  @inlinable
  @inline(__always)
  @unsafe
  internal func uncheckedValue(at bucket: Bucket) -> Value {
    defer { _fixLifetime(self) }
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    return unsafe _values[bucket.offset]
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  @inline(__always)
  @unsafe
  internal func uncheckedInitialize(
    at bucket: Bucket,
    toKey key: __owned Key,
    value: __owned Value) {
    defer { _fixLifetime(self) }
    unsafe _internalInvariant(hashTable.isValid(bucket))
    unsafe (_keys + bucket.offset).initialize(to: key)
    unsafe (_values + bucket.offset).initialize(to: value)
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  @inline(__always)
  @unsafe
  internal func uncheckedDestroy(at bucket: Bucket) {
    defer { _fixLifetime(self) }
    unsafe _internalInvariant(hashTable.isValid(bucket))
    unsafe (_keys + bucket.offset).deinitialize(count: 1)
    unsafe (_values + bucket.offset).deinitialize(count: 1)
  }
}

extension _NativeDictionary { // Low-level lookup operations
  @inlinable
  @inline(__always)
  internal func hashValue(for key: Key) -> Int {
    return unsafe key._rawHashValue(seed: _storage._seed)
  }

  @safe
  @inlinable
  @inline(__always)
  internal func find(_ key: Key) -> (bucket: Bucket, found: Bool) {
    return unsafe _storage.find(key)
  }

  /// Search for a given element, assuming it has the specified hash value.
  ///
  /// If the element is not present in this set, return the position where it
  /// could be inserted.
  @inlinable
  @inline(__always)
  internal func find(
    _ key: Key,
    hashValue: Int
  ) -> (bucket: Bucket, found: Bool) {
    return unsafe _storage.find(key, hashValue: hashValue)
  }
}

extension _NativeDictionary { // ensureUnique
  @_alwaysEmitIntoClient
  @inline(never)
  internal mutating func _copyOrMoveAndResize(
    capacity: Int,
    moveElements: Bool
  ) {
    let capacity = Swift.max(capacity, self.capacity)
    let newStorage = unsafe _DictionaryStorage<Key, Value>.resize(
      original: _storage,
      capacity: capacity,
      move: moveElements)
    let result = unsafe _NativeDictionary(newStorage)
    if count > 0 {
      for unsafe bucket in unsafe hashTable {
        let key: Key
        let value: Value
        if moveElements {
          key = unsafe (_keys + bucket.offset).move()
          value = unsafe (_values + bucket.offset).move()
        } else {
          key = unsafe self.uncheckedKey(at: bucket)
          value = unsafe self.uncheckedValue(at: bucket)
        }
        result._unsafeInsertNew(key: key, value: value)
      }
      if moveElements {
        // Clear out old storage, ensuring that its deinit won't overrelease the
        // elements we've just moved out.
        unsafe _storage._hashTable.clear()
        unsafe _storage._count = 0
      }
    }
    unsafe _storage = result._storage
  }

  @inlinable
  internal mutating func resize(capacity: Int) {
    _copyOrMoveAndResize(capacity: capacity, moveElements: true)
  }

  @inlinable
  internal mutating func copyAndResize(capacity: Int) {
    _copyOrMoveAndResize(capacity: capacity, moveElements: false)
  }

  @inlinable
  @_semantics("optimize.sil.specialize.generic.size.never")
  internal mutating func copy() {
    let newStorage = unsafe _DictionaryStorage<Key, Value>.copy(original: _storage)
    unsafe _internalInvariant(newStorage._scale == _storage._scale)
    unsafe _internalInvariant(newStorage._age == _storage._age)
    unsafe _internalInvariant(newStorage._seed == _storage._seed)
    let result = unsafe _NativeDictionary(newStorage)
    if count > 0 {
      unsafe result.hashTable.copyContents(of: hashTable)
      unsafe result._storage._count = self.count
      for unsafe bucket in unsafe hashTable {
        let key = unsafe uncheckedKey(at: bucket)
        let value = unsafe uncheckedValue(at: bucket)
        unsafe result.uncheckedInitialize(at: bucket, toKey: key, value: value)
      }
    }
    unsafe _storage = result._storage
  }

  /// Ensure storage of self is uniquely held and can hold at least `capacity`
  /// elements.
  ///
  /// -Returns: `true` if contents were rehashed; otherwise, `false`.
  @inlinable
  @_semantics("optimize.sil.specialize.generic.size.never")
  internal mutating func ensureUnique(isUnique: Bool, capacity: Int) -> Bool {
    if _fastPath(capacity <= self.capacity && isUnique) {
      return false
    }
    if isUnique {
      resize(capacity: capacity)
      return true
    }
    if capacity <= self.capacity {
      copy()
      return false
    }
    copyAndResize(capacity: capacity)
    return true
  }

  internal mutating func reserveCapacity(_ capacity: Int, isUnique: Bool) {
    _ = ensureUnique(isUnique: isUnique, capacity: capacity)
  }
}

extension _NativeDictionary {
  @inlinable
  @inline(__always)
  func validatedBucket(for index: _HashTable.Index) -> Bucket {
    unsafe _precondition(hashTable.isOccupied(index.bucket) && index.age == age,
      "Attempting to access Dictionary elements using an invalid index")
    return unsafe index.bucket
  }

  @inlinable
  @inline(__always)
  func validatedBucket(for index: Dictionary<Key, Value>.Index) -> Bucket {
#if _runtime(_ObjC)
    guard index._isNative else {
      index._cocoaPath()
      // Accept Cocoa indices as long as they contain a key that exists in this
      // dictionary, and the address of their Cocoa object generates the same
      // age.
      let cocoa = index._asCocoa
      if cocoa.age == self.age {
        let key = _forceBridgeFromObjectiveC(cocoa.key, Key.self)
        let (bucket, found) = find(key)
        if found {
          return bucket
        }
      }
      _preconditionFailure(
        "Attempting to access Dictionary elements using an invalid index")
    }
#endif
    return unsafe validatedBucket(for: index._asNative)
  }
}

extension _NativeDictionary: _DictionaryBuffer {
  @usableFromInline
  internal typealias Index = Dictionary<Key, Value>.Index

  @inlinable
  internal var startIndex: Index {
    let bucket = unsafe hashTable.startBucket
    return unsafe Index(_native: _HashTable.Index(bucket: bucket, age: age))
  }

  @inlinable
  internal var endIndex: Index {
    let bucket = unsafe hashTable.endBucket
    return unsafe Index(_native: _HashTable.Index(bucket: bucket, age: age))
  }

  @inlinable
  internal func index(after index: Index) -> Index {
#if _runtime(_ObjC)
    guard _fastPath(index._isNative) else {
      let _ = validatedBucket(for: index)
      let i = index._asCocoa
      return Index(_cocoa: i.dictionary.index(after: i))
    }
#endif
    let bucket = unsafe validatedBucket(for: index._asNative)
    let next = unsafe hashTable.occupiedBucket(after: bucket)
    return unsafe Index(_native: _HashTable.Index(bucket: next, age: age))
  }

  @inlinable
  internal func index(forKey key: Key) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (bucket, found) = find(key)
    guard found else { return nil }
    return unsafe Index(_native: _HashTable.Index(bucket: bucket, age: age))
  }

  @inlinable
  internal var count: Int {
    @inline(__always) get {
      return unsafe _assumeNonNegative(_storage._count)
    }
  }

  @inlinable
  @inline(__always)
  func contains(_ key: Key) -> Bool {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return false
    }
    return find(key).found
  }

  @inlinable
  @inline(__always)
  func lookup(_ key: Key) -> Value? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (bucket, found) = self.find(key)
    guard found else { return nil }
    return unsafe self.uncheckedValue(at: bucket)
  }

  @inlinable
  @inline(__always)
  func lookup(_ index: Index) -> (key: Key, value: Value) {
    let bucket = validatedBucket(for: index)
    let key = unsafe self.uncheckedKey(at: bucket)
    let value = unsafe self.uncheckedValue(at: bucket)
    return (key, value)
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
    let bucket = validatedBucket(for: index)
    return unsafe self.uncheckedKey(at: bucket)
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
    let bucket = validatedBucket(for: index)
    return unsafe self.uncheckedValue(at: bucket)
  }
}

extension _NativeDictionary {
  @inlinable
  subscript(key: Key, isUnique isUnique: Bool) -> Value? {
    @inline(__always)
    get {
      // Dummy definition; don't use.
      return lookup(key)
    }
    @inline(__always)
    _modify {
      let (bucket, found) = mutatingFind(key, isUnique: isUnique)
      // If found, move the old value out of storage, wrapping it into an
      // optional before yielding it.
      var value: Value? = unsafe (found ? (_values + bucket.offset).move() : nil)
      defer {
        // This is in a defer block because yield might throw, and we need to
        // preserve Dictionary invariants when that happens.
        if let value = value {
          if found {
            // **Mutation.** Initialize storage to new value.
            unsafe (_values + bucket.offset).initialize(to: value)
          } else {
            // **Insertion.** Insert the new entry at the correct place.  Note
            // that `mutatingFind` already ensured that we have enough capacity.
            _insert(at: bucket, key: key, value: value)
          }
        } else {
          if found {
            // **Removal.** We've already deinitialized the value; deinitialize
            // the key too and register the removal.
            unsafe (_keys + bucket.offset).deinitialize(count: 1)
            _delete(at: bucket)
          } else {
            // Noop
          }
        }
      }
      yield &value
    }
  }
}

// This function has a highly visible name to make it stand out in stack traces.
@usableFromInline
@inline(never)
@_unavailableInEmbedded
internal func KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(
  _ keyType: Any.Type
) -> Never {
  _assertionFailure(
    "Fatal error",
    """
    Duplicate keys of type '\(keyType)' were found in a Dictionary.
    This usually means either that the type violates Hashable's requirements, or
    that members of such a dictionary were mutated after insertion.
    """,
    flags: _fatalErrorFlags())
}

extension _NativeDictionary { // Insertions
  /// Insert a new element into uniquely held storage.
  /// Storage must be uniquely referenced with adequate capacity.
  /// The `key` must not be already present in the Dictionary.
  @inlinable
  internal func _unsafeInsertNew(key: __owned Key, value: __owned Value) {
    _internalInvariant(count + 1 <= capacity)
    let hashValue = self.hashValue(for: key)
    if _isDebugAssertConfiguration() {
      // In debug builds, perform a full lookup and trap if we detect duplicate
      // elements -- these imply that the Element type violates Hashable
      // requirements. This is generally more costly than a direct insertion,
      // because we'll need to compare elements in case of hash collisions.
      let (bucket, found) = find(key, hashValue: hashValue)
      guard !found else {
        #if !$Embedded
        KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(Key.self)
        #else
        fatalError("duplicate keys in a Dictionary")
        #endif
      }
      unsafe hashTable.insert(bucket)
      unsafe uncheckedInitialize(at: bucket, toKey: key, value: value)
    } else {
      let bucket = unsafe hashTable.insertNew(hashValue: hashValue)
      unsafe uncheckedInitialize(at: bucket, toKey: key, value: value)
    }
    unsafe _storage._count &+= 1
  }

  /// Insert a new element into uniquely held storage, replacing an existing
  /// value (if any).  Storage must be uniquely referenced with adequate
  /// capacity.
  @_alwaysEmitIntoClient @inlinable // Introduced in 5.1
  internal mutating func _unsafeUpdate(
    key: __owned Key,
    value: __owned Value
  ) {
    let (bucket, found) = find(key)
    if found {
      // Note that we also update the key here. This method is used to handle
      // collisions arising from equality transitions during bridging, and in
      // that case it is desirable to keep values paired with their original
      // keys. This is not how `updateValue(_:, forKey:)` works.
      unsafe (_keys + bucket.offset).pointee = key
      unsafe (_values + bucket.offset).pointee = value
    } else {
      _precondition(count < capacity)
      _insert(at: bucket, key: key, value: value)
    }
  }

  /// Insert a new entry into uniquely held storage.
  /// Storage must be uniquely referenced.
  /// The `key` must not be already present in the Dictionary.
  @inlinable
  internal mutating func insertNew(key: __owned Key, value: __owned Value) {
    _ = ensureUnique(isUnique: true, capacity: count + 1)
    _unsafeInsertNew(key: key, value: value)
  }

  /// Same as find(_:), except assume a corresponding key/value pair will be
  /// inserted if it doesn't already exist, and mutated if it does exist. When
  /// this function returns, the storage is guaranteed to be native, uniquely
  /// held, and with enough capacity for a single insertion (if the key isn't
  /// already in the dictionary.)
  @safe
  @inlinable
  internal mutating func mutatingFind(
    _ key: Key,
    isUnique: Bool
  ) -> (bucket: Bucket, found: Bool) {
    let (bucket, found) = find(key)

    // Prepare storage.
    // If `key` isn't in the dictionary yet, assume that this access will end
    // up inserting it. (If we guess wrong, we might needlessly expand
    // storage; that's fine.) Otherwise this can only be a removal or an
    // in-place mutation.
    let rehashed = ensureUnique(
      isUnique: isUnique,
      capacity: count + (found ? 0 : 1))
    guard rehashed else { return (bucket, found) }
    let (b, f) = find(key)
    if f != found {
      #if !$Embedded
      KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(Key.self)
      #else
      fatalError("duplicate keys in a Dictionary")
      #endif
    }
    return (b, found)
  }

  @inlinable
  internal func _insert(
    at bucket: Bucket,
    key: __owned Key,
    value: __owned Value) {
    _internalInvariant(count < capacity)
    unsafe hashTable.insert(bucket)
    unsafe uncheckedInitialize(at: bucket, toKey: key, value: value)
    unsafe _storage._count += 1
  }

  @inlinable
  internal mutating func updateValue(
    _ value: __owned Value,
    forKey key: Key,
    isUnique: Bool
  ) -> Value? {
    let (bucket, found) = mutatingFind(key, isUnique: isUnique)
    if found {
      let oldValue = unsafe (_values + bucket.offset).move()
      unsafe (_values + bucket.offset).initialize(to: value)
      return oldValue
    }
    _insert(at: bucket, key: key, value: value)
    return nil
  }

  @inlinable
  internal mutating func setValue(
    _ value: __owned Value,
    forKey key: Key,
    isUnique: Bool
  ) {
    let (bucket, found) = mutatingFind(key, isUnique: isUnique)
    if found {
      unsafe (_values + bucket.offset).pointee = value
    } else {
      _insert(at: bucket, key: key, value: value)
    }
  }
}

extension _NativeDictionary {
  @inlinable
  @inline(__always)
  internal mutating func swapValuesAt(
    _ a: Bucket,
    _ b: Bucket,
    isUnique: Bool
  ) {
    let rehashed = ensureUnique(isUnique: isUnique, capacity: capacity)
    _internalInvariant(!rehashed)
    unsafe _internalInvariant(hashTable.isOccupied(a) && hashTable.isOccupied(b))
    let value = unsafe (_values + a.offset).move()
    unsafe (_values + a.offset).moveInitialize(from: _values + b.offset, count: 1)
    unsafe (_values + b.offset).initialize(to: value)
  }
  
  @_alwaysEmitIntoClient
  internal func extractDictionary(
    using bitset: _UnsafeBitset, 
    count: Int
  ) -> _NativeDictionary<Key, Value> {
    var count = count
    if count == 0 { return _NativeDictionary<Key, Value>() }
    if count == self.count { return self }
    let result = _NativeDictionary<Key, Value>(capacity: count)
    for unsafe offset in unsafe bitset {
      let key = unsafe self.uncheckedKey(at: Bucket(offset: offset))
      let value = unsafe self.uncheckedValue(at: Bucket(offset: offset))
      result._unsafeInsertNew(key: key, value: value)
      // The hash table can have set bits after the end of the bitmap.
      // Ignore them.
      count -= 1
      if count == 0 { break }
    }
    return result
  }
}

extension _NativeDictionary where Value: Equatable {
  @inlinable
  @inline(__always)
  func isEqual(to other: _NativeDictionary) -> Bool {
    if unsafe (self._storage === other._storage) { return true }
    if self.count != other.count { return false }

    for (key, value) in self {
      let (bucket, found) = other.find(key)
      guard found, unsafe other.uncheckedValue(at: bucket) == value else {
        return false
      }
    }
    return true
  }

#if _runtime(_ObjC)
  @inlinable
  func isEqual(to other: __CocoaDictionary) -> Bool {
    if self.count != other.count { return false }

    defer { _fixLifetime(self) }
    for unsafe bucket in unsafe self.hashTable {
      let key = unsafe self.uncheckedKey(at: bucket)
      let value = unsafe self.uncheckedValue(at: bucket)
      guard
        let cocoaValue = other.lookup(_bridgeAnythingToObjectiveC(key)),
        value == _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      else {
        return false
      }
    }
    return true
  }
#endif
}

extension _NativeDictionary: _HashTableDelegate {
  @inlinable
  @inline(__always)
  internal func hashValue(at bucket: Bucket) -> Int {
    return unsafe hashValue(for: uncheckedKey(at: bucket))
  }

  @inlinable
  @inline(__always)
  internal func moveEntry(from source: Bucket, to target: Bucket) {
    unsafe _internalInvariant(hashTable.isValid(source))
    unsafe _internalInvariant(hashTable.isValid(target))
    unsafe (_keys + target.offset)
      .moveInitialize(from: _keys + source.offset, count: 1)
    unsafe (_values + target.offset)
      .moveInitialize(from: _values + source.offset, count: 1)
  }

  @inlinable
  @inline(__always)
  internal func swapEntry(_ left: Bucket, with right: Bucket) {
    unsafe _internalInvariant(hashTable.isValid(left))
    unsafe _internalInvariant(hashTable.isValid(right))
    unsafe swap(&_keys[left.offset], &_keys[right.offset])
    unsafe swap(&_values[left.offset], &_values[right.offset])
  }
}

extension _NativeDictionary { // Deletion
  @inlinable
  @_effects(releasenone)
  @_semantics("optimize.sil.specialize.generic.size.never")
  internal func _delete(at bucket: Bucket) {
    unsafe hashTable.delete(at: bucket, with: self)
    unsafe _storage._count -= 1
    unsafe _internalInvariant(_storage._count >= 0)
    invalidateIndices()
  }

  @inlinable
  @_semantics("optimize.sil.specialize.generic.size.never")
  @unsafe
  internal mutating func uncheckedRemove(
    at bucket: Bucket,
    isUnique: Bool
  ) -> Element {
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    let rehashed = ensureUnique(isUnique: isUnique, capacity: capacity)
    _internalInvariant(!rehashed)
    let oldKey = unsafe (_keys + bucket.offset).move()
    let oldValue = unsafe (_values + bucket.offset).move()
    _delete(at: bucket)
    return (oldKey, oldValue)
  }

  @usableFromInline
  internal mutating func removeAll(isUnique: Bool) {
    guard isUnique else {
      let scale = unsafe self._storage._scale
      unsafe _storage = _DictionaryStorage<Key, Value>.allocate(
        scale: scale,
        age: nil,
        seed: nil)
      return
    }
    for unsafe bucket in unsafe hashTable {
      unsafe (_keys + bucket.offset).deinitialize(count: 1)
      unsafe (_values + bucket.offset).deinitialize(count: 1)
    }
    unsafe hashTable.clear()
    unsafe _storage._count = 0
    invalidateIndices()
  }
}

extension _NativeDictionary { // High-level operations
  @inlinable
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    let resultStorage = unsafe _DictionaryStorage<Key, T>.copy(original: _storage)
    unsafe _internalInvariant(resultStorage._seed == _storage._seed)
    let result = unsafe _NativeDictionary<Key, T>(resultStorage)
    // Because the current and new buffer have the same scale and seed, we can
    // initialize to the same locations in the new buffer, skipping hash value
    // recalculations.
    for unsafe bucket in unsafe hashTable {
      let key = unsafe self.uncheckedKey(at: bucket)
      let value = unsafe self.uncheckedValue(at: bucket)
      try result._insert(at: bucket, key: key, value: transform(value))
    }
    return result
  }

  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: __owned S,
    isUnique: Bool,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    var isUnique = isUnique
    for (key, value) in keysAndValues {
      let (bucket, found) = mutatingFind(key, isUnique: isUnique)
      isUnique = true
      if found {
        do {
          let newValue = try combine(unsafe uncheckedValue(at: bucket), value)
          unsafe _values[bucket.offset] = newValue
        } catch _MergeError.keyCollision {
          #if !$Embedded
          fatalError("Duplicate values for key: '\(key)'")
          #else
          fatalError("Duplicate values for a key in a Dictionary")
          #endif
        }
      } else {
        _insert(at: bucket, key: key, value: value)
      }
    }
  }

  #if $Embedded
  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: __owned S,
    isUnique: Bool,
    uniquingKeysWith combine: (Value, Value) throws(_MergeError) -> Value
  ) where S.Element == (Key, Value) {
    var isUnique = isUnique
    for (key, value) in keysAndValues {
      let (bucket, found) = mutatingFind(key, isUnique: isUnique)
      isUnique = true
      if found {
        do throws(_MergeError) {
          let newValue = try combine(unsafe uncheckedValue(at: bucket), value)
          unsafe _values[bucket.offset] = newValue
        } catch {
          #if !$Embedded
          fatalError("Duplicate values for key: '\(key)'")
          #else
          fatalError("Duplicate values for a key in a Dictionary")
          #endif
        }
      } else {
        _insert(at: bucket, key: key, value: value)
      }
    }
  }
  #endif

  @inlinable
  @inline(__always)
  internal init<S: Sequence>(
    grouping values: __owned S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value == [S.Element] {
    self.init()
    for value in values {
      let key = try keyForValue(value)
      let (bucket, found) = mutatingFind(key, isUnique: true)
      if found {
        unsafe _values[bucket.offset].append(value)
      } else {
        _insert(at: bucket, key: key, value: [value])
      }
    }
  }

  @_alwaysEmitIntoClient
  internal func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> _NativeDictionary<Key, Value> {
    try unsafe _UnsafeBitset.withTemporaryBitset(
      capacity: _storage._bucketCount
    ) { bitset in
      var count = 0
      for unsafe bucket in unsafe hashTable {
        if try unsafe isIncluded(
          (uncheckedKey(at: bucket), uncheckedValue(at: bucket))
        ) {
          unsafe bitset.uncheckedInsert(bucket.offset)
          count += 1
        }
      }
      return unsafe extractDictionary(using: bitset, count: count)
    }
  }
}

extension _NativeDictionary: Sequence {
  @usableFromInline
  @frozen
  @safe
  internal struct Iterator {
    // The iterator is iterating over a frozen view of the collection state, so
    // it keeps its own reference to the dictionary.
    @usableFromInline
    internal let base: _NativeDictionary
    @usableFromInline
    internal var iterator: _HashTable.Iterator

    @inlinable
    @inline(__always)
    init(_ base: __owned _NativeDictionary) {
      self.base = base
      unsafe self.iterator = base.hashTable.makeIterator()
    }
  }

  @inlinable
  internal __consuming func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

@available(*, unavailable)
extension _NativeDictionary.Iterator: Sendable {}

extension _NativeDictionary.Iterator: IteratorProtocol {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  @inlinable
  @inline(__always)
  internal mutating func nextKey() -> Key? {
    guard let index = unsafe iterator.next() else { return nil }
    return unsafe base.uncheckedKey(at: index)
  }

  @inlinable
  @inline(__always)
  internal mutating func nextValue() -> Value? {
    guard let index = unsafe iterator.next() else { return nil }
    return unsafe base.uncheckedValue(at: index)
  }

  @inlinable
  @inline(__always)
  internal mutating func next() -> Element? {
    guard let index = unsafe iterator.next() else { return nil }
    let key = unsafe base.uncheckedKey(at: index)
    let value = unsafe base.uncheckedValue(at: index)
    return (key, value)
  }
}
