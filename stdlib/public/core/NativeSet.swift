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

/// A wrapper around __RawSetStorage that provides most of the
/// implementation of Set.
@usableFromInline
@frozen
@safe
internal struct _NativeSet<Element: Hashable> {
  /// See the comments on __RawSetStorage and its subclasses to understand why we
  /// store an untyped storage here.
  @usableFromInline
  internal var _storage: __RawSetStorage

  /// Constructs an instance from the empty singleton.
  @inlinable
  @inline(__always)
  internal init() {
    unsafe self._storage = __RawSetStorage.empty
  }

  /// Constructs a native set adopting the given storage.
  @inlinable
  @inline(__always)
  internal init(_ storage: __owned __RawSetStorage) {
    unsafe self._storage = storage
  }

  @inlinable
  internal init(capacity: Int) {
    if capacity == 0 {
      unsafe self._storage = __RawSetStorage.empty
    } else {
      unsafe self._storage = _SetStorage<Element>.allocate(capacity: capacity)
    }
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_ cocoa: __owned __CocoaSet) {
    self.init(cocoa, capacity: cocoa.count)
  }

  @inlinable
  internal init(_ cocoa: __owned __CocoaSet, capacity: Int) {
    if capacity == 0 {
      unsafe self._storage = __RawSetStorage.empty
    } else {
      _internalInvariant(cocoa.count <= capacity)
      unsafe self._storage = _SetStorage<Element>.convert(cocoa, capacity: capacity)
      for element in cocoa {
        let nativeElement = _forceBridgeFromObjectiveC(element, Element.self)
        insertNew(nativeElement, isUnique: true)
      }
    }
  }
#endif
}

@available(*, unavailable)
extension _NativeSet: Sendable {}

extension _NativeSet { // Primitive fields
  @usableFromInline
  internal typealias Bucket = _HashTable.Bucket

  @inlinable
  internal var capacity: Int {
    @inline(__always)
    get {
      return unsafe _assumeNonNegative(_storage._capacity)
    }
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  internal var bucketCount: Int {
    unsafe _assumeNonNegative(_storage._bucketCount)
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
  internal var _elements: UnsafeMutablePointer<Element> {
    return unsafe _storage._rawElements.assumingMemoryBound(to: Element.self)
  }

  @inlinable
  @inline(__always)
  internal func invalidateIndices() {
    unsafe _storage._age &+= 1
  }
}

extension _NativeSet { // Low-level unchecked operations
  @inlinable
  @inline(__always)
  @unsafe
  internal func uncheckedElement(at bucket: Bucket) -> Element {
    defer { _fixLifetime(self) }
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    return unsafe _elements[bucket.offset]
  }

  @inlinable
  @inline(__always)
  @unsafe
  internal func uncheckedInitialize(
    at bucket: Bucket,
    to element: __owned Element
  ) {
    unsafe _internalInvariant(hashTable.isValid(bucket))
    unsafe (_elements + bucket.offset).initialize(to: element)
  }

  @_alwaysEmitIntoClient @inlinable // Introduced in 5.1
  @inline(__always)
  @unsafe
  internal func uncheckedAssign(
    at bucket: Bucket,
    to element: __owned Element
  ) {
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    unsafe (_elements + bucket.offset).pointee = element
  }
}

extension _NativeSet { // Low-level lookup operations
  @inlinable
  @inline(__always)
  internal func hashValue(for element: Element) -> Int {
    return unsafe element._rawHashValue(seed: _storage._seed)
  }

  @safe
  @inlinable
  @inline(__always)
  internal func find(_ element: Element) -> (bucket: Bucket, found: Bool) {
    return find(element, hashValue: self.hashValue(for: element))
  }

  /// Search for a given element, assuming it has the specified hash value.
  ///
  /// If the element is not present in this set, return the position where it
  /// could be inserted.
  @inlinable
  @inline(__always)
  internal func find(
    _ element: Element,
    hashValue: Int
  ) -> (bucket: Bucket, found: Bool) {
    let hashTable = unsafe self.hashTable
    var bucket = unsafe hashTable.idealBucket(forHashValue: hashValue)
    while unsafe hashTable._isOccupied(bucket) {
      if unsafe uncheckedElement(at: bucket) == element {
        return (bucket, true)
      }
      bucket = unsafe hashTable.bucket(wrappedAfter: bucket)
    }
    return (bucket, false)
  }
}

extension _NativeSet { // ensureUnique
  @inlinable
  internal mutating func resize(capacity: Int) {
    let capacity = Swift.max(capacity, self.capacity)
    let result = unsafe _NativeSet(_SetStorage<Element>.resize(
        original: _storage,
        capacity: capacity,
        move: true))
    if count > 0 {
      for unsafe bucket in unsafe hashTable {
        let element = unsafe (self._elements + bucket.offset).move()
        unsafe result._unsafeInsertNew(element)
      }
      // Clear out old storage, ensuring that its deinit won't overrelease the
      // elements we've just moved out.
      unsafe _storage._hashTable.clear()
      unsafe _storage._count = 0
    }
    unsafe _storage = result._storage
  }

  @inlinable
  internal mutating func copyAndResize(capacity: Int) {
    let capacity = Swift.max(capacity, self.capacity)
    let result = unsafe _NativeSet(_SetStorage<Element>.resize(
        original: _storage,
        capacity: capacity,
        move: false))
    if count > 0 {
      for unsafe bucket in unsafe hashTable {
        unsafe result._unsafeInsertNew(self.uncheckedElement(at: bucket))
      }
    }
    unsafe _storage = result._storage
  }

  @inlinable
  internal mutating func copy() {
    let newStorage = unsafe _SetStorage<Element>.copy(original: _storage)
    unsafe _internalInvariant(newStorage._scale == _storage._scale)
    unsafe _internalInvariant(newStorage._age == _storage._age)
    unsafe _internalInvariant(newStorage._seed == _storage._seed)
    let result = unsafe _NativeSet(newStorage)
    if count > 0 {
      unsafe result.hashTable.copyContents(of: hashTable)
      unsafe result._storage._count = self.count
      for unsafe bucket in unsafe hashTable {
        let element = unsafe uncheckedElement(at: bucket)
        unsafe result.uncheckedInitialize(at: bucket, to: element)
      }
    }
    unsafe _storage = result._storage
  }

  /// Ensure storage of self is uniquely held and can hold at least `capacity`
  /// elements.
  ///
  /// -Returns: `true` if contents were rehashed; otherwise, `false`.
  @inlinable
  @inline(__always)
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

extension _NativeSet {
  @inlinable
  @inline(__always)
  func validatedBucket(for index: _HashTable.Index) -> Bucket {
    unsafe _precondition(hashTable.isOccupied(index.bucket) && index.age == age,
      "Attempting to access Set elements using an invalid index")
    return unsafe index.bucket
  }

  @inlinable
  @inline(__always)
  func validatedBucket(for index: Set<Element>.Index) -> Bucket {
#if _runtime(_ObjC)
    guard index._isNative else {
      index._cocoaPath()
      let cocoa = index._asCocoa
      // Accept Cocoa indices as long as they contain an element that exists in
      // this set, and the address of their Cocoa object generates the same age.
      if cocoa.age == self.age {
        let element = _forceBridgeFromObjectiveC(cocoa.element, Element.self)
        let (bucket, found) = find(element)
        if found {
          return bucket
        }
      }
      _preconditionFailure(
        "Attempting to access Set elements using an invalid index")
    }
#endif
    return unsafe validatedBucket(for: index._asNative)
  }
}

extension _NativeSet: _SetBuffer {
  @usableFromInline
  internal typealias Index = Set<Element>.Index

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
    // Note that _asNative forces this not to work on Cocoa indices.
    let bucket = unsafe validatedBucket(for: index._asNative)
    let next = unsafe hashTable.occupiedBucket(after: bucket)
    return unsafe Index(_native: _HashTable.Index(bucket: next, age: age))
  }

  @inlinable
  @inline(__always)
  internal func index(for element: Element) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (bucket, found) = find(element)
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
  internal func contains(_ member: Element) -> Bool {
    // Fast path: Don't calculate the hash if the set has no elements.
    if count == 0 { return false }
    return find(member).found
  }

  @inlinable
  @inline(__always)
  internal func element(at index: Index) -> Element {
    let bucket = validatedBucket(for: index)
    return unsafe uncheckedElement(at: bucket)
  }
}

// This function has a highly visible name to make it stand out in stack traces.
@usableFromInline
@inline(never)
@_unavailableInEmbedded
internal func ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(
  _ elementType: Any.Type
) -> Never {
  _assertionFailure(
    "Fatal error",
    """
    Duplicate elements of type '\(elementType)' were found in a Set.
    This usually means either that the type violates Hashable's requirements, or
    that members of such a set were mutated after insertion.
    """,
    flags: _fatalErrorFlags())
}

extension _NativeSet { // Insertions
  /// Insert a new element into uniquely held storage.
  /// Storage must be uniquely referenced with adequate capacity.
  /// The `element` must not be already present in the Set.
  @inlinable
  @unsafe
  internal func _unsafeInsertNew(_ element: __owned Element) {
    _internalInvariant(count + 1 <= capacity)
    let hashValue = self.hashValue(for: element)
    if _isDebugAssertConfiguration() {
      // In debug builds, perform a full lookup and trap if we detect duplicate
      // elements -- these imply that the Element type violates Hashable
      // requirements. This is generally more costly than a direct insertion,
      // because we'll need to compare elements in case of hash collisions.
      let (bucket, found) = find(element, hashValue: hashValue)
      guard !found else {
        #if !$Embedded
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
        #else
        fatalError("duplicate elements in a Set")
        #endif
      }
      unsafe hashTable.insert(bucket)
      unsafe uncheckedInitialize(at: bucket, to: element)
    } else {
      let bucket = unsafe hashTable.insertNew(hashValue: hashValue)
      unsafe uncheckedInitialize(at: bucket, to: element)
    }
    unsafe _storage._count &+= 1
  }

  /// Insert a new element into uniquely held storage.
  /// Storage must be uniquely referenced.
  /// The `element` must not be already present in the Set.
  @inlinable
  internal mutating func insertNew(_ element: __owned Element, isUnique: Bool) {
    _ = ensureUnique(isUnique: isUnique, capacity: count + 1)
    unsafe _unsafeInsertNew(element)
  }

  @inlinable
  @unsafe
  internal func _unsafeInsertNew(_ element: __owned Element, at bucket: Bucket) {
    unsafe hashTable.insert(bucket)
    unsafe uncheckedInitialize(at: bucket, to: element)
    unsafe _storage._count += 1
  }

  @inlinable
  internal mutating func insertNew(
    _ element: __owned Element,
    at bucket: Bucket,
    isUnique: Bool
  ) {
    unsafe _internalInvariant(!hashTable.isOccupied(bucket))
    var bucket = bucket
    let rehashed = ensureUnique(isUnique: isUnique, capacity: count + 1)
    if rehashed {
      let (b, f) = find(element)
      if f {
        #if !$Embedded
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
        #else
        fatalError("duplicate elements in a Set")
        #endif
      }
      bucket = b
    }
    unsafe _unsafeInsertNew(element, at: bucket)
  }

  @inlinable
  internal mutating func update(
    with element: __owned Element,
    isUnique: Bool
  ) -> Element? {
    var (bucket, found) = find(element)
    let rehashed = ensureUnique(
      isUnique: isUnique,
      capacity: count + (found ? 0 : 1))
    if rehashed {
      let (b, f) = find(element)
      if f != found {
        #if !$Embedded
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
        #else
        fatalError("duplicate elements in a Set")
        #endif
      }
      bucket = b
    }
    if found {
      let old = unsafe (_elements + bucket.offset).move()
      unsafe uncheckedInitialize(at: bucket, to: element)
      return old
    }
    unsafe _unsafeInsertNew(element, at: bucket)
    return nil
  }

  /// Insert an element into uniquely held storage, replacing an existing value
  /// (if any).  Storage must be uniquely referenced with adequate capacity.
  @_alwaysEmitIntoClient @inlinable // Introduced in 5.1
  internal mutating func _unsafeUpdate(
    with element: __owned Element
  ) {
    let (bucket, found) = find(element)
    if found {
      unsafe uncheckedAssign(at: bucket, to: element)
    } else {
      _precondition(count < capacity)
      unsafe _unsafeInsertNew(element, at: bucket)
    }
  }
}

extension _NativeSet {
  @inlinable
  @inline(__always)
  func isEqual(to other: _NativeSet) -> Bool {
    if unsafe (self._storage === other._storage) { return true }
    if self.count != other.count { return false }

    for member in self {
      guard other.find(member).found else { return false }
    }
    return true
  }

#if _runtime(_ObjC)
  @inlinable
  func isEqual(to other: __CocoaSet) -> Bool {
    if self.count != other.count { return false }

    defer { _fixLifetime(self) }
    for unsafe bucket in unsafe self.hashTable {
      let key = unsafe self.uncheckedElement(at: bucket)
      let bridgedKey = _bridgeAnythingToObjectiveC(key)
      guard other.contains(bridgedKey) else { return false }
    }
    return true
  }
#endif
}

extension _NativeSet: _HashTableDelegate {
  @inlinable
  @inline(__always)
  internal func hashValue(at bucket: Bucket) -> Int {
    return hashValue(for: unsafe uncheckedElement(at: bucket))
  }

  @inlinable
  @inline(__always)
  internal func moveEntry(from source: Bucket, to target: Bucket) {
    unsafe (_elements + target.offset)
      .moveInitialize(from: _elements + source.offset, count: 1)
  }
}

extension _NativeSet { // Deletion
  @inlinable
  @_effects(releasenone)
  internal mutating func _delete(at bucket: Bucket) {
    unsafe hashTable.delete(at: bucket, with: self)
    unsafe _storage._count -= 1
    unsafe _internalInvariant(_storage._count >= 0)
    invalidateIndices()
  }

  @inlinable
  @inline(__always)
  @unsafe
  internal mutating func uncheckedRemove(
    at bucket: Bucket,
    isUnique: Bool) -> Element {
    unsafe _internalInvariant(hashTable.isOccupied(bucket))
    let rehashed = ensureUnique(isUnique: isUnique, capacity: capacity)
    _internalInvariant(!rehashed)
    let old = unsafe (_elements + bucket.offset).move()
    _delete(at: bucket)
    return old
  }

  @usableFromInline
  internal mutating func removeAll(isUnique: Bool) {
    guard isUnique else {
      let scale = unsafe self._storage._scale
      unsafe _storage = _SetStorage<Element>.allocate(
        scale: scale,
        age: nil,
        seed: nil)
      return
    }
    for unsafe bucket in unsafe hashTable {
      unsafe (_elements + bucket.offset).deinitialize(count: 1)
    }
    unsafe hashTable.clear()
    unsafe _storage._count = 0
    invalidateIndices()
  }
}

extension _NativeSet: Sequence {
  @usableFromInline
  @frozen
  @safe
  internal struct Iterator {
    // The iterator is iterating over a frozen view of the collection state, so
    // it keeps its own reference to the set.
    @usableFromInline
    internal let base: _NativeSet
    @usableFromInline
    internal var iterator: _HashTable.Iterator

    @inlinable
    @inline(__always)
    init(_ base: __owned _NativeSet) {
      self.base = base
      unsafe self.iterator = base.hashTable.makeIterator()
    }
  }

  @inlinable
  @inline(__always)
  internal __consuming func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

@available(*, unavailable)
extension _NativeSet.Iterator: Sendable {}

extension _NativeSet.Iterator: IteratorProtocol {
  @inlinable
  @inline(__always)
  internal mutating func next() -> Element? {
    guard let index = unsafe iterator.next() else { return nil }
    return unsafe base.uncheckedElement(at: index)
  }
}

extension _NativeSet {
  @_alwaysEmitIntoClient
  internal func isSubset<S: Sequence>(of possibleSuperset: S) -> Bool
  where S.Element == Element {
    unsafe _UnsafeBitset.withTemporaryBitset(capacity: self.bucketCount) { seen in
      // Mark elements in self that we've seen in `possibleSuperset`.
      var seenCount = 0
      for element in possibleSuperset {
        let (bucket, found) = find(element)
        guard found else { continue }
        let inserted = unsafe seen.uncheckedInsert(bucket.offset)
        if inserted {
          seenCount += 1
          if seenCount == self.count {
            return true
          }
        }
      }
      return false
    }
  }

  @_alwaysEmitIntoClient
  internal func isStrictSubset<S: Sequence>(of possibleSuperset: S) -> Bool
  where S.Element == Element {
    unsafe _UnsafeBitset.withTemporaryBitset(capacity: self.bucketCount) { seen in
      // Mark elements in self that we've seen in `possibleSuperset`.
      var seenCount = 0
      var isStrict = false
      for element in possibleSuperset {
        let (bucket, found) = find(element)
        guard found else {
          if !isStrict {
            isStrict = true
            if seenCount == self.count { return true }
          }
          continue
        }
        let inserted = unsafe seen.uncheckedInsert(bucket.offset)
        if inserted {
          seenCount += 1
          if seenCount == self.count, isStrict {
            return true
          }
        }
      }
      return false
    }
  }

  @_alwaysEmitIntoClient
  internal func isStrictSuperset<S: Sequence>(of possibleSubset: S) -> Bool
  where S.Element == Element {
    unsafe _UnsafeBitset.withTemporaryBitset(capacity: self.bucketCount) { seen in
      // Mark elements in self that we've seen in `possibleStrictSubset`.
      var seenCount = 0
      for element in possibleSubset {
        let (bucket, found) = find(element)
        guard found else { return false }
        let inserted = unsafe seen.uncheckedInsert(bucket.offset)
        if inserted {
          seenCount += 1
          if seenCount == self.count {
            return false
          }
        }
      }
      return true
    }
  }

  @_alwaysEmitIntoClient
  internal __consuming func extractSubset(
    using bitset: _UnsafeBitset,
    count: Int
  ) -> _NativeSet {
    var count = count
    if count == 0 { return _NativeSet() }
    if count == self.count { return self }
    let result = _NativeSet(capacity: count)
    for unsafe offset in unsafe bitset {
      unsafe result._unsafeInsertNew(self.uncheckedElement(at: Bucket(offset: offset)))
      // The hash table can have set bits after the end of the bitmap.
      // Ignore them.
      count -= 1
      if count == 0 { break }
    }
    return result
  }

  @_alwaysEmitIntoClient
  internal __consuming func subtracting<S: Sequence>(_ other: S) -> _NativeSet
  where S.Element == Element {
    guard count > 0 else { return _NativeSet() }

    // Find one item that we need to remove before creating a result set.
    var it = other.makeIterator()
    var bucket: Bucket? = nil
    while let next = it.next() {
      let (b, found) = find(next)
      if found {
        bucket = b
        break
      }
    }
    guard let bucket = bucket else { return self }

    // Rather than directly creating a new set, calculate the difference in a
    // bitset first. This ensures we hash each element (in both sets) only once,
    // and that we'll have an exact count for the result set, preventing
    // rehashings during insertions.
    return unsafe _UnsafeBitset.withTemporaryCopy(of: hashTable.bitset) { difference in
      var remainingCount = self.count

      let removed = unsafe difference.uncheckedRemove(bucket.offset)
      _internalInvariant(removed)
      remainingCount -= 1

      while let element = it.next() {
        let (bucket, found) = find(element)
        if found {
          if unsafe difference.uncheckedRemove(bucket.offset) {
            remainingCount -= 1
            if remainingCount == 0 { return _NativeSet() }
          }
        }
      }
      unsafe _internalInvariant(difference.count > 0)
      return unsafe extractSubset(using: difference, count: remainingCount)
    }
  }

  @_alwaysEmitIntoClient
  internal __consuming func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> _NativeSet<Element> {
    try unsafe _UnsafeBitset.withTemporaryBitset(capacity: bucketCount) { bitset in
      var count = 0
      for unsafe bucket in unsafe hashTable {
        if try isIncluded(unsafe uncheckedElement(at: bucket)) {
          unsafe bitset.uncheckedInsert(bucket.offset)
          count += 1
        }
      }
      return unsafe extractSubset(using: bitset, count: count)
    }
  }

  @_alwaysEmitIntoClient
  internal __consuming func intersection(
    _ other: _NativeSet<Element>
  ) -> _NativeSet<Element> {
    // Rather than directly creating a new set, mark common elements in a
    // bitset first. This minimizes hashing, and ensures that we'll have an
    // exact count for the result set, preventing rehashings during
    // insertions.
    unsafe _UnsafeBitset.withTemporaryBitset(capacity: bucketCount) { bitset in
      var count = 0
      // Prefer to iterate over the smaller set. However, we must be careful to
      // only include elements from `self`, not `other`.
      if self.count > other.count {
        for element in other {
          let (bucket, found) = find(element)
          if found {
            // `other` is a `Set`, so we can assume it doesn't have duplicates.
            unsafe bitset.uncheckedInsert(bucket.offset)
            count += 1
          }
        }
      } else {
        for unsafe bucket in unsafe hashTable {
          if other.find(unsafe uncheckedElement(at: bucket)).found {
            unsafe bitset.uncheckedInsert(bucket.offset)
            count += 1
          }
        }
      }
      return unsafe extractSubset(using: bitset, count: count)
    }
  }

  @_alwaysEmitIntoClient
  internal __consuming func genericIntersection<S: Sequence>(
    _ other: S
  ) -> _NativeSet<Element>
  where S.Element == Element {
    // Rather than directly creating a new set, mark common elements in a bitset
    // first. This minimizes hashing, and ensures that we'll have an exact count
    // for the result set, preventing rehashings during insertions.
    unsafe _UnsafeBitset.withTemporaryBitset(capacity: bucketCount) { bitset in
      var count = 0
      for element in other {
        let (bucket, found) = find(element)
        // Note: we need to be careful not to increment `count` here if the
        // element is a duplicate item.
        if found, unsafe bitset.uncheckedInsert(bucket.offset) {
          count += 1
        }
      }
      return unsafe extractSubset(using: bitset, count: count)
    }
  }
}
