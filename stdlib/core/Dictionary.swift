//===----------------------------------------------------------------------===//
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

// General Mutable, Value-Type Collections
// =================================================
//
// Basic copy-on-write (COW) requires a container's data to be copied
// into new storage before it is modified, to avoid changing the data
// of other containers that may share the data.  There is one
// exception: when we know the container has the only reference to the
// data, we can elide the copy.  This COW optimization is crucial for
// the performance of mutating algorithms.
//
// Some container elements (Characters in a String, key/value pairs in
// an open-addressing hash table) are not traversable with a fixed
// size offset, so incrementing/decrementing indices requires looking
// at the contents of the container.  The current interface for
// incrementing/decrementing indices of an Collection is the usual ++i,
// --i. Therefore, for memory safety, the indices need to keep a
// reference to the container's underlying data so that it can be
// inspected.  But having multiple outstanding references to the
// underlying data defeats the COW optimization.
//
// The way out is to count containers referencing the data separately
// from indices that reference the data.  When deciding to elide the
// copy and modify the data directly---as long as we don't violate
// memory safety of any outstanding indices---we only need to be
// sure that no other containers are referencing the data.
//
// Implementation notes
// ====================
//
// `Dictionary` uses two storage schemes: native storage and Cocoa storage.
//
// Native storage is a hash table with open addressing and linear probing.  The
// bucket array forms a logical ring (e.g., a chain can wrap around the end of
// buckets array to the beginning of it).
//
// The buckets are typed as `Optional<(KeyType, ValueType)>`.  A `.None` value
// marks the end of a chain.  There is always at least one `.None` among the
// buckets.  `Dictionary` does not use tombstones.
//
// In addition to the native storage `Dictionary` can also wrap an
// `NSDictionary` in order to allow brdidging `NSDictionary` to `Dictionary` in
// `O(1)`.
//
// Currently native storage uses a data structure like this::
//
//   Dictionary<K,V> (a struct)
//   +----------------------------------------------+
//   | [ _VariantDictionaryStorage<K,V> (an enum) ] |
//   +---|------------------------------------------+
//      /
//     |
//     V  _NativeDictionaryStorageOwner<K,V> (a class)
//   +-----------------------------------------------------------+
//   | [refcount#1] [ _NativeDictionaryStorage<K,V> (a struct) ] |
//   +----------------|------------------------------------------+
//                    |
//     +--------------+
//     |
//     V  _NativeDictionaryStorageImpl<K,V> (a class)
//   +-----------------------------------------+
//   | [refcount#2]    [...element storage...] |
//   +-----------------------------------------+
//     ^
//     +---+
//         |              Dictionary<K,V>.Index (an enum)
//   +-----|--------------------------------------------+
//   |     |     _NativeDictionaryIndex<K,V> (a struct) |
//   | +---|------------------------------------------+ |
//   | | [ _NativeDictionaryStorage<K,V> (a struct) ] | |
//   | +----------------------------------------------+ |
//   +--------------------------------------------------+
//
// We would like to optimize by allocating the `_NativeDictionaryStorageOwner`
// /inside/ the `_NativeDictionaryStorageImpl`, and override the `dealloc`
// method of `_NativeDictionaryStorageOwner` to do nothing but release its
// reference.
//
//     Dictionary<K,V> (a struct)
//     +----------------------------------------------+
//     | [ _VariantDictionaryStorage<K,V> (an enum) ] |
//     +---|------------------------------------------+
//        /
//       |          +---+
//       |          V   |  _NativeDictionaryStorageImpl<K,V> (a class)
//   +---|--------------|----------------------------------------------+
//   |   |              |                                              |
//   |   | [refcount#2] |                                              |
//   |   |              |                                              |
//   |   V              | _NativeDictionaryStorageOwner<K,V> (a class) |
//   | +----------------|------------------------------------------+   |
//   | | [refcount#1] [ _NativeDictionaryStorage<K,V> (a struct) ] |   |
//   | +-----------------------------------------------------------+   |
//   |                                                                 |
//   | [...element storage...]                                         |
//   +-----------------------------------------------------------------+
//
//
// Cocoa storage uses a data structure like this::
//
//   Dictionary<K,V> (a struct)
//   +----------------------------------------------+
//   | _VariantDictionaryStorage<K,V> (an enum)     |
//   | +----------------------------------------+   |
//   | | [ _CocoaDictionaryStorage (a struct) ] |   |
//   | +---|------------------------------------+   |
//   +-----|----------------------------------------+
//         |
//     +---+
//     |
//     V  NSDictionary (a class)
//   +--------------+
//   | [refcount#1] |
//   +--------------+
//     ^
//     +-+
//       |     Dictionary<K,V>.Index (an enum)
//   +---|-----------------------------------+
//   |   |  _CocoaDictionaryIndex (a struct) |
//   | +-|-----------------------------+     |
//   | | * [ all keys ] [ next index ] |     |
//   | +-------------------------------+     |
//   +---------------------------------------+
//
// `_NativeDictionaryStorageOwnerBase` is an `NSDictionary` subclass.  It can
// be returned to Objective-C during bridging if both `KeyType` and `ValueType`
// bridge verbatim.
//
// Index Invalidation
// ------------------
//
// Indexing a container, `c[i]`, uses the integral offset stored in the index
// to access the elements referenced by the container.  The buffer referenced
// by the index is only used to increment and decrement the index.  Most of the
// time, these two buffers will be identical, but they need not always be.  For
// example, if one ensures that a `Dictionary` has sufficient capacity to avoid
// reallocation on the next element insertion, the following works ::
//
//   var (i, found) = d.find(k) // i is associated with d's buffer
//   if found {
//      var e = d            // now d is sharing its data with e
//      e[newKey] = newValue // e now has a unique copy of the data
//      return e[i]          // use i to access e
//   }
//
// The result should be a set of iterator invalidation rules familiar to anyone
// familiar with the C++ standard library.  Note that because all accesses to a
// dictionary buffer are bounds-checked, this scheme never compromises memory
// safety.
//
// Bridging
// ========
//
// Bridging `NSDictionary` to `Dictionary`
// ---------------------------------------
//
// `NSDictionary` bridges to `Dictionary<NSObject, AnyObject>` in `O(1)`,
// without memory allocation.
//
// Bridging `Dictionary` to `NSDictionary`
// ---------------------------------------
//
// `Dictionary<K, V>` bridges to `NSDictionary` iff both `K` and `V` are
// bridged.  Otherwise, a runtime error is raised.
//
// * if both `K` and `V` are bridged verbatim, then `Dictionary<K, V>` bridges
//   to `NSDictionary` in `O(1)`, without memory allocation.
//
// * otherwise, `K` and/or `V` are unconditionally or conditionally bridged.
//   In this case, `Dictionary<K, V>` is bridged to `NSDictionary` in `O(N)`
//   by allocating a new `NSDictionary` that contains the bridged key-value
//   pairs from the original `Dictionary`.
//
// Syntax for bridging
// -------------------
//
// There are two implict conversions:
//
// * `NSDictionary` converts to `Dictionary<NSObject, AnyObject>`,
// * `Dictionary<K, V>` converts to `NSDictionary`.
//

/// This protocol is only used for compile-time checks that
/// every storage type implements all required operations.
protocol _DictionaryStorage {
  typealias KeyType
  typealias ValueType
  typealias Index
  var startIndex: Index { get }
  var endIndex: Index { get }
  func indexForKey(key: KeyType) -> Index?
  func assertingGet(i: Index) -> (KeyType, ValueType)
  func assertingGet(key: KeyType) -> ValueType
  func maybeGet(key: KeyType) -> ValueType?
  mutating func updateValue(value: ValueType, forKey: KeyType) -> ValueType?
  mutating func removeAtIndex(index: Index)
  mutating func removeValueForKey(key: KeyType) -> ValueType?
  var count: Int { get }

  class func fromArray(elements: Array<(KeyType, ValueType)>) -> Self
}

/// The inverse of the default hash table load factor.  Factored out so that it
/// can be used in multiple places in the implementation and stay consistent.
/// Should not be used outside `Dictionary` implementation.
@transparent
var _dictionaryDefaultMaxLoadFactorInverse: Double {
  return 1.0 / 0.75
}

/// Header part of the native storage for `Dictionary`.
struct _DictionaryBody {
  init(capacity: Int) {
    self.capacity = capacity
  }

  var capacity: Int
  var count: Int = 0
  var maxLoadFactorInverse: Double = _dictionaryDefaultMaxLoadFactorInverse
}

/// An element of the variable-length array part of the native storage for
/// `Dictionary`.
struct _DictionaryElement<KeyType : Hashable, ValueType> {
  let key: KeyType
  var value: ValueType
  @conversion func __conversion() -> (KeyType, ValueType) {
    return (key, value)
  }
}

/// An instance of this class has all dictionary data tail-allocated.  It is
/// used as a `HeapBuffer` storage.
@final class _NativeDictionaryStorageImpl<KeyType : Hashable, ValueType> :
    HeapBufferStorageBase {

  typealias Element = _DictionaryElement<KeyType, ValueType>
  typealias DictionaryHeapBuffer = HeapBuffer<_DictionaryBody, Element?>

  deinit {
    let buffer = DictionaryHeapBuffer(reinterpretCast(self) as DictionaryHeapBuffer.Storage)
    let body = buffer.value
    buffer._value.destroy()
    buffer.elementStorage.destroy(body.capacity)
  }
}

struct _NativeDictionaryStorage<KeyType : Hashable, ValueType> :
    _DictionaryStorage {

  typealias Owner = _NativeDictionaryStorageOwner<KeyType, ValueType>
  typealias StorageImpl = _NativeDictionaryStorageImpl<KeyType, ValueType>
  typealias Element = _DictionaryElement<KeyType, ValueType>

  let buffer: StorageImpl.DictionaryHeapBuffer

  @transparent
  var body: _DictionaryBody {
    get {
      return buffer.value
    }
    nonmutating set(newValue) {
      buffer.value = newValue
    }
  }

  @transparent
  var elements: UnsafePointer<Element?> {
    return buffer.elementStorage
  }

  init(capacity: Int) {
    let body = _DictionaryBody(capacity: capacity)
    buffer = StorageImpl.DictionaryHeapBuffer(StorageImpl.self, body, capacity)
    for var i = 0; i < capacity; ++i {
      (elements + i).initialize(.None)
    }
  }

  init(minimumCapacity: Int = 2) {
    // Make sure there's a representable power of 2 >= minimumCapacity
    _sanityCheck(minimumCapacity <= (Int.max >> 1) + 1)

    var capacity = 2
    while capacity < minimumCapacity {
      capacity <<= 1
    }

    self = _NativeDictionaryStorage<KeyType, ValueType>(capacity: capacity)
  }

  @transparent
  var capacity: Int {
    get {
      return body.capacity
    }
    nonmutating set(newValue) {
      body.capacity = newValue
    }
  }

  @transparent
  var count: Int {
    get {
      return body.count
    }
    nonmutating set(newValue) {
      body.count = newValue
    }
  }

  @transparent
  var maxLoadFactorInverse: Double {
    get {
      return body.maxLoadFactorInverse
    }
    set(newValue) {
      body.maxLoadFactorInverse = newValue
    }
  }

  @transparent
  var maxLoadFactor: Double {
    get {
      return 1.0 / maxLoadFactorInverse
    }
    set(newValue) {
      // 1.0 might be useful for testing purposes; anything more is
      // crazy
      _sanityCheck(newValue <= 1.0)
      maxLoadFactorInverse = 1.0 / newValue
    }
  }

  subscript(i: Int) -> Element? {
    @transparent
    get {
      _precondition(i >= 0 && i < capacity)
      return (elements + i).memory
    }
    @transparent
    nonmutating set {
      _precondition(i >= 0 && i < capacity)
      (elements + i).memory = newValue
    }
  }

  //
  // Implementation details
  //

  var _bucketMask: Int {
    return capacity - 1
  }

  func _bucket(k: KeyType) -> Int {
    return k.hashValue & _bucketMask
  }

  func _next(bucket: Int) -> Int {
    return (bucket + 1) & _bucketMask
  }

  func _prev(bucket: Int) -> Int {
    return (bucket - 1) & _bucketMask
  }

  /// Search for a given key starting from the specified bucket.
  ///
  /// If the key is not present, returns the position where it could be
  /// inserted.
  func _find(k: KeyType, _ startBucket: Int) -> (pos: Index, found: Bool) {
    var bucket = startBucket

    // The invariant guarantees there's always a hole, so we just loop
    // until we find one
    while true {
      var keyVal = self[bucket]
      if !keyVal || keyVal!.key == k {
        return (Index(nativeStorage: self, offset: bucket), Bool(keyVal))
      }
      bucket = _next(bucket)
    }
  }

  @transparent
  static func getMinCapacity(
      requestedCount: Int, _ maxLoadFactorInverse: Double) -> Int {
    // `requestedCount + 1` below ensures that we don't fill in the last hole
    return max(Int(Double(requestedCount) * maxLoadFactorInverse),
               requestedCount + 1)
  }

  /// Storage should be uniquely referenced.
  /// The `key` should not be present in the dictionary.
  /// This function does *not* update `count`.
  mutating func unsafeAddNew(#key: KeyType, value: ValueType) {
    var (i, found) = _find(key, _bucket(key))
    _sanityCheck(!found, "unsafeAddNew was called, but the key is already present")
    self[i.offset] = Element(key: key, value: value)
  }

  //
  // _DictionaryStorage conformance
  //

  typealias Index = _NativeDictionaryIndex<KeyType, ValueType>

  var startIndex: Index {
    return Index(nativeStorage: self, offset: -1).succ()
  }

  var endIndex: Index {
    return Index(nativeStorage: self, offset: capacity)
  }

  func indexForKey(key: KeyType) -> Index? {
    var (i, found) = _find(key, _bucket(key))
    return found ? i : .None
  }

  func assertingGet(i: Index) -> (KeyType, ValueType) {
    let e = self[i.offset]
    _precondition(e, "attempting to access Dictionary elements using an invalid Index")
    return e!
  }

  func assertingGet(key: KeyType) -> ValueType {
    let e = self[_find(key, _bucket(key)).pos.offset]
    _precondition(e, "key not found in Dictionary")
    return e!.value
  }

  func maybeGet(key: KeyType) -> ValueType? {
    var (i, found) = _find(key, _bucket(key))
    if found {
      return self[i.offset]!.value
    }
    return .None
  }

  mutating func updateValue(value: ValueType, forKey: KeyType) -> ValueType? {
    _fatalError("don't call mutating methods on _NativeDictionaryStorage")
  }

  mutating func removeAtIndex(index: Index) {
    _fatalError("don't call mutating methods on _NativeDictionaryStorage")
  }

  mutating func removeValueForKey(key: KeyType) -> ValueType? {
    _fatalError("don't call mutating methods on _NativeDictionaryStorage")
  }

  static func fromArray(
      elements: Array<(KeyType, ValueType)>
  ) -> _NativeDictionaryStorage<KeyType, ValueType> {
    let requiredCapacity =
      _NativeDictionaryStorage<KeyType, ValueType>.getMinCapacity(
          elements.count, _dictionaryDefaultMaxLoadFactorInverse)
    var nativeStorage = _NativeDictionaryStorage<KeyType, ValueType>(
        minimumCapacity: requiredCapacity)
    for (key, value) in elements {
      var (i, found) = nativeStorage._find(key, nativeStorage._bucket(key))
      _precondition(!found, "dictionary literal contains duplicate keys")
      nativeStorage[i.offset] = Element(key: key, value: value)
    }
    nativeStorage.count = elements.count
    return nativeStorage
  }
}

/// This class existis only to work around a compiler limitation.
/// Specifically, we can not have @objc members in a generic class.  When this
/// limitation is gone, this class can be folded into
/// `_NativeDictionaryStorageKeyNSEnumerator`.
@objc
class _NativeDictionaryStorageKeyNSEnumeratorBase
    : _NSSwiftEnumerator, _SwiftNSEnumerator {

  init(dummy: (Int, ())) {}

  func bridgingNextObject(dummy: ()) -> AnyObject? {
    _fatalError("'bridgingNextObject' should be overridden")
  }

  // Don't implement a custom `bridgingCountByEnumeratingWithState` function.
  // `NSEnumerator` will provide a default implementation for us that is just
  // as fast as ours could be.  The issue is that there is some strange code
  // out there that wants to break out of a fast enumeration loop and continue
  // consuming elements of `NSEnumerator`.  Thus, fast enumeration on
  // `NSEnumerator` can not provide more than one element at a time, so it is
  // not fast anymore.

  //
  // NSEnumerator implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  @objc
  init() {
    _fatalError("don't call this designated initializer")
  }

  @objc
  func nextObject() -> AnyObject? {
    return bridgingNextObject(())
  }
}

@final @objc
class _NativeDictionaryStorageKeyNSEnumerator<KeyType : Hashable, ValueType>
    : _NativeDictionaryStorageKeyNSEnumeratorBase {

  typealias NativeStorage = _NativeDictionaryStorage<KeyType, ValueType>
  typealias Index = _NativeDictionaryIndex<KeyType, ValueType>

  init(_ nativeStorage: NativeStorage) {
    _precondition(isBridgedVerbatimToObjectiveC(KeyType.self) &&
           isBridgedVerbatimToObjectiveC(ValueType.self),
           "native Dictionary storage can be used as NSDictionary only when both key and value are bridged verbatim to Objective-C")

    nextIndex = nativeStorage.startIndex
    endIndex = nativeStorage.endIndex
    super.init(dummy: (0, ()))
  }

  var nextIndex: Index
  var endIndex: Index

  //
  // Dictionary -> NSDictionary bridging.
  //

  override func bridgingNextObject(dummy: ()) -> AnyObject? {
    if nextIndex == endIndex {
      return nil
    }
    let (nativeKey, _) = nextIndex.nativeStorage.assertingGet(nextIndex)
    nextIndex = nextIndex.succ()
    return _reinterpretCastToAnyObject(nativeKey)
  }
}

/// This class existis only to work around a compiler limitation.
/// Specifically, we can not have @objc members in a generic class.  When this
/// limitation is gone, this class can be folded into
/// `_NativeDictionaryStorageOwner`.
@objc
class _NativeDictionaryStorageOwnerBase
    : _NSSwiftDictionary, _SwiftNSDictionaryRequiredOverrides {

  init() {}

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  // subclass of an @objc class has no effect
  var bridgingCount: (Int, ()) {
    _fatalError("'bridgingCount' should be overridden")
  }

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  func bridgingObjectForKey(aKey: AnyObject, dummy: ()) -> AnyObject? {
    _fatalError("'bridgingObjectForKey' should be overridden")
  }

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  func bridgingKeyEnumerator(dummy: ()) -> _SwiftNSEnumerator {
    _fatalError("'bridgingKeyEnumerator' should be overridden")
  }

  func bridgingCountByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    _fatalError("'countByEnumeratingWithState' should be overridden")
  }

  //
  // NSDictionary implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `nativeStorage`.
  //

  @objc
  init(objects: CConstPointer<AnyObject?>, forKeys: CConstVoidPointer,
       count: Int) {
    _fatalError("don't call this designated initializer")
  }

  @objc
  var count: Int {
    return bridgingCount.0
  }

  @objc
  func objectForKey(aKey: AnyObject?) -> AnyObject? {
    if let nonNullKey: AnyObject = aKey {
      return bridgingObjectForKey(nonNullKey, dummy: ())
    }
    return nil
  }

  @objc
  func keyEnumerator() -> _SwiftNSEnumerator? {
    return bridgingKeyEnumerator(())
  }

  @objc
  func copyWithZone(zone: _SwiftNSZone) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSDictionary` type, which is immutable.
    return self
  }

  @objc
  func countByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int
  ) -> Int {
    return bridgingCountByEnumeratingWithState(
        state, objects: objects, count: count, dummy: ())
  }
}

/// This class is an artifact of the COW implementation.  This class only
/// exists to keep separate retain counts separate for:
/// - `Dictionary` and `NSDictionary`,
/// - `DictionaryIndex`.
///
/// This is important because the uniqueness check for COW only cares about
/// retain counts of the first kind.
///
/// Specifically, `Dictionary` points to instances of this class.  This class
/// is also a proper `NSDictionary` subclass, which is returned to Objective-C
/// during bridging.  `DictionaryIndex` points directly to
/// `_NativeDictionaryStorage`.
@final class _NativeDictionaryStorageOwner<KeyType : Hashable, ValueType>
    : _NativeDictionaryStorageOwnerBase {

  typealias NativeStorage = _NativeDictionaryStorage<KeyType, ValueType>

  init(minimumCapacity: Int = 2) {
    nativeStorage = NativeStorage(minimumCapacity: minimumCapacity)
    super.init()
  }

  init(nativeStorage: _NativeDictionaryStorage<KeyType, ValueType>) {
    self.nativeStorage = nativeStorage
    super.init()
  }

  var nativeStorage: NativeStorage

  //
  // Dictionary -> NSDictionary bridging.
  //

  override var bridgingCount: (Int, ()) {
    return (nativeStorage.count, ())
  }

  override func bridgingObjectForKey(aKey: AnyObject, dummy: ()) -> AnyObject? {
    _sanityCheck(isBridgedVerbatimToObjectiveC(KeyType.self) &&
                 isBridgedVerbatimToObjectiveC(ValueType.self),
                 "native Dictionary storage can be used as NSDictionary only when both key and value are bridged verbatim to Objective-C")
    let nativeKey = reinterpretCast(aKey) as KeyType
    if let nativeValue = nativeStorage.maybeGet(nativeKey) {
      return _reinterpretCastToAnyObject(nativeValue)
    }
    return nil
  }

  override func bridgingKeyEnumerator(dummy: ()) -> _SwiftNSEnumerator {
    // Extra variable to work around a bug:
    // <rdar://problem/16825366> Hole in type safety with initializer
    // requirements in protocols
    let result: _NativeDictionaryStorageKeyNSEnumeratorBase =
        _NativeDictionaryStorageKeyNSEnumerator<KeyType, ValueType>(
            nativeStorage)
    return result
  }

  override func bridgingCountByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    _sanityCheck(isBridgedVerbatimToObjectiveC(KeyType.self) &&
                 isBridgedVerbatimToObjectiveC(ValueType.self),
                 "native Dictionary storage can be used as NSDictionary only when both key and value are bridged verbatim to Objective-C")

    var theState = state.memory
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = UnsafePointer<AnyObject?>(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(nativeStorage.startIndex.offset)
    }
    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    var currIndex = _NativeDictionaryIndex<KeyType, ValueType>(
        nativeStorage: nativeStorage, offset: Int(theState.extra.0))
    let endIndex = nativeStorage.endIndex
    var stored = 0
    for i in 0..count {
      if (currIndex == endIndex) {
        break
      }
      var (nativeKey, _) = nativeStorage.assertingGet(currIndex)
      let bridgedKey: AnyObject = _reinterpretCastToAnyObject(nativeKey)
      unmanagedObjects[i] = bridgedKey
      ++stored
      currIndex = currIndex.succ()
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.memory = theState
    return stored
  }
}

struct _CocoaDictionaryStorage : _DictionaryStorage {
  var cocoaDictionary: _SwiftNSDictionary

  typealias Index = _CocoaDictionaryIndex

  var startIndex: Index {
    return Index(cocoaDictionary, startIndex: ())
  }

  var endIndex: Index {
    return Index(cocoaDictionary, endIndex: ())
  }

  func indexForKey(key: AnyObject) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if !maybeGet(key) {
      return .None
    }

    let allKeys = cocoaDictionary.allKeys
    let keyIndex = allKeys.indexOfObject(key)
    return Index(cocoaDictionary, allKeys, keyIndex)
  }

  func assertingGet(i: Index) -> (AnyObject, AnyObject) {
    let key: AnyObject = i.allKeys.objectAtIndex(i.nextKeyIndex)
    let value: AnyObject = i.cocoaDictionary.objectForKey(key)!
    return (key, value)
  }

  func assertingGet(key: AnyObject) -> AnyObject {
    let value: AnyObject? = cocoaDictionary.objectForKey(key)
    _precondition(value, "key not found in underlying NSDictionary")
    return value!
  }

  func maybeGet(key: AnyObject) -> AnyObject? {
    return cocoaDictionary.objectForKey(key)
  }

  mutating func updateValue(value: AnyObject, forKey: AnyObject) -> AnyObject? {
    _fatalError("can not mutate NSDictionary")
  }

  mutating func removeAtIndex(index: Index) {
    _fatalError("can not mutate NSDictionary")
  }

  mutating func removeValueForKey(key: AnyObject) -> AnyObject? {
    _fatalError("can not mutate NSDictionary")
  }

  var count: Int {
    return cocoaDictionary.count
  }

  static func fromArray(
      elements: Array<(AnyObject, AnyObject)>
  ) -> _CocoaDictionaryStorage {
    _fatalError("this function should never be called")
  }
}

enum _VariantDictionaryStorage<KeyType : Hashable, ValueType> :
    _DictionaryStorage {

  typealias _NativeStorageElement = _DictionaryElement<KeyType, ValueType>
  typealias NativeStorage =
      _NativeDictionaryStorage<KeyType, ValueType>
  typealias NativeStorageOwner =
      _NativeDictionaryStorageOwner<KeyType, ValueType>
  typealias CocoaStorage = _CocoaDictionaryStorage
  typealias NativeIndex = _NativeDictionaryIndex<KeyType, ValueType>

  case Native(NativeStorageOwner)
  case Cocoa(CocoaStorage)

  @transparent
  var guaranteedNative: Bool {
    return !_canBeClass(KeyType.self) && !_canBeClass(ValueType.self)
  }

  mutating func isUniquelyReferenced() -> Bool {
    if _fastPath(guaranteedNative) {
      return Swift.isUniquelyReferenced(&self)
    }

    switch self {
    case .Native:
      return Swift.isUniquelyReferenced(&self)
    case .Cocoa:
      // Don't consider Cocoa storage mutable, even if it is mutable and is
      // uniquely referenced.
      return false
    }
  }

  var native: NativeStorage {
    switch self {
    case .Native(let owner):
      return owner.nativeStorage
    case .Cocoa:
      _fatalError("internal error: not backed by native storage")
    }
  }

  var cocoa: CocoaStorage {
    switch self {
    case .Native:
      _fatalError("internal error: not backed by NSDictionary")
    case .Cocoa(let cocoaStorage):
      return cocoaStorage
    }
  }

  /// Ensure this we hold a unique reference to a native storage
  /// having at least `minimumCapacity` elements.
  mutating func ensureUniqueNativeStorage(minimumCapacity: Int)
      -> (reallocated: Bool, capacityChanged: Bool) {
    switch self {
    case .Native:
      let oldNativeStorage = native
      let oldCapacity = oldNativeStorage.capacity
      if isUniquelyReferenced() && oldCapacity >= minimumCapacity {
        return (reallocated: false, capacityChanged: false)
      }

      let newNativeOwner = NativeStorageOwner(minimumCapacity: minimumCapacity)
      var newNativeStorage = newNativeOwner.nativeStorage
      let newCapacity = newNativeStorage.capacity

      for i in 0..oldCapacity {
        var x = oldNativeStorage[i]
        if x {
          if oldCapacity == newCapacity {
            // FIXME(performance): optimize this case further: we don't have to
            // initialize the buffer first and then copy over the buckets, we
            // should initialize the new buffer with buckets directly.
            newNativeStorage[i] = x
          }
          else {
            newNativeStorage.unsafeAddNew(key: x!.key, value: x!.value)
          }
        }
      }
      newNativeStorage.count = oldNativeStorage.count

      self = .Native(newNativeOwner)
      return (reallocated: true,
              capacityChanged: oldCapacity != newNativeStorage.capacity)

    case .Cocoa(let cocoaStorage):
      let cocoaDictionary = cocoaStorage.cocoaDictionary
      let newNativeOwner = NativeStorageOwner(minimumCapacity: minimumCapacity)
      var newNativeStorage = newNativeOwner.nativeStorage
      var oldCocoaGenerator = _CocoaDictionaryGenerator(cocoaDictionary)
      while let (key: AnyObject, value: AnyObject) = oldCocoaGenerator.next() {
        newNativeStorage.unsafeAddNew(
            key: bridgeFromObjectiveCUnconditional(key, KeyType.self),
            value: bridgeFromObjectiveCUnconditional(value, ValueType.self))
      }
      newNativeStorage.count = cocoaDictionary.count

      self = .Native(newNativeOwner)
      return (reallocated: true, capacityChanged: true)
    }
  }

  mutating func migrateDataToNativeStorage(cocoaStorage: _CocoaDictionaryStorage) {
    var minCapacity = NativeStorage.getMinCapacity(
        cocoaStorage.count, _dictionaryDefaultMaxLoadFactorInverse)
    var allocated = ensureUniqueNativeStorage(minCapacity).reallocated
    _sanityCheck(allocated, "failed to allocate native dictionary storage")
  }

  //
  // _DictionaryStorage conformance
  //

  typealias Index = DictionaryIndex<KeyType, ValueType>

  var startIndex: Index {
    switch self {
    case .Native:
      return ._Native(native.startIndex)
    case .Cocoa(let cocoaStorage):
      return ._Cocoa(cocoaStorage.startIndex)
    }
  }

  var endIndex: Index {
    switch self {
    case .Native:
      return ._Native(native.endIndex)
    case .Cocoa(let cocoaStorage):
      return ._Cocoa(cocoaStorage.endIndex)
    }
  }

  func indexForKey(key: KeyType) -> Index? {
    switch self {
    case .Native:
      if let nativeIndex = native.indexForKey(key) {
        return .Some(._Native(nativeIndex))
      }
      return .None
    case .Cocoa(let cocoaStorage):
      let anyObjectKey: AnyObject = bridgeToObjectiveCUnconditional(key)
      if let cocoaIndex = cocoaStorage.indexForKey(anyObjectKey) {
        return .Some(._Cocoa(cocoaIndex))
      }
      return .None
    }
  }

  func assertingGet(i: Index) -> (KeyType, ValueType) {
    switch self {
    case .Native:
      return native.assertingGet(i._nativeIndex)
    case .Cocoa(let cocoaStorage):
      var (anyObjectKey: AnyObject, anyObjectValue: AnyObject) =
          cocoaStorage.assertingGet(i._cocoaIndex)
      let nativeKey =
          bridgeFromObjectiveCUnconditional(anyObjectKey, KeyType.self)
      let nativeValue =
          bridgeFromObjectiveCUnconditional(anyObjectValue, ValueType.self)
      return (nativeKey, nativeValue)
    }
  }

  func assertingGet(key: KeyType) -> ValueType {
    switch self {
    case .Native:
      return native.assertingGet(key)
    case .Cocoa(let cocoaStorage):
      // FIXME: This assumes that KeyType and ValueType are bridged verbatim.
      let anyObjectKey: AnyObject = _reinterpretCastToAnyObject(key)
      let anyObjectValue: AnyObject = cocoaStorage.assertingGet(anyObjectKey)
      return reinterpretCast(anyObjectValue) as ValueType
    }
  }

  func maybeGet(key: KeyType) -> ValueType? {
    switch self {
    case .Native:
      return native.maybeGet(key)
    case .Cocoa(let cocoaStorage):
      let anyObjectKey: AnyObject = bridgeToObjectiveCUnconditional(key)
      if let anyObjectValue: AnyObject = cocoaStorage.maybeGet(anyObjectKey) {
        return bridgeFromObjectiveCUnconditional(anyObjectValue, ValueType.self)
      }
      return .None
    }
  }

  mutating func nativeUpdateValue(
      value: ValueType, forKey key: KeyType
  ) -> ValueType? {
    var nativeStorage = native
    var (i, found) = nativeStorage._find(key, nativeStorage._bucket(key))

    let minCapacity = found
        ? nativeStorage.capacity
        : NativeStorage.getMinCapacity(
              nativeStorage.count + 1,
              nativeStorage.maxLoadFactorInverse)

    let (reallocated, capacityChanged) = ensureUniqueNativeStorage(minCapacity)
    if reallocated {
      nativeStorage = native
    }
    if capacityChanged {
      i = nativeStorage._find(key, nativeStorage._bucket(key)).pos
    }
    let oldValue: ValueType? = found ? nativeStorage[i.offset]!.value : .None
    nativeStorage[i.offset] = _NativeStorageElement(key: key, value: value)

    if !found {
      ++nativeStorage.count
    }
    return oldValue
  }

  mutating func updateValue(value: ValueType, forKey key: KeyType) -> ValueType? {
    if _fastPath(guaranteedNative) {
      return nativeUpdateValue(value, forKey: key)
    }

    switch self {
    case .Native:
      return nativeUpdateValue(value, forKey: key)
    case .Cocoa(let cocoaStorage):
      migrateDataToNativeStorage(cocoaStorage)
      return nativeUpdateValue(value, forKey: key)
    }
  }

  /// :param: start The offset of the beginning of the chain.
  /// :param: offset The offset of the element that will be deleted.
  mutating func nativeDeleteImpl(
      nativeStorage: NativeStorage, start: Int, offset: Int
  ) {
    // remove the element
    nativeStorage[offset] = .None
    --nativeStorage.count

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = offset

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    for var b = nativeStorage._next(lastInChain); nativeStorage[b];
        b = nativeStorage._next(b) {
      lastInChain = b
    }

    // Relocate out-of-place elements in the chain, repeating until
    // none are found.
    while hole != lastInChain {
      // Walk backwards from the end of the chain looking for
      // something out-of-place.
      var b: Int
      for b = lastInChain; b != hole; b = nativeStorage._prev(b) {
        var idealBucket = nativeStorage._bucket(nativeStorage[b]!.key)

        // Does this element belong between start and hole?  We need
        // two separate tests depending on whether [start,hole] wraps
        // around the end of the buffer
        var c0 = idealBucket >= start
        var c1 = idealBucket <= hole
        if start <= hole ? (c0 && c1) : (c0 || c1) {
          break // found it
        }
      }

      if b == hole { // No out-of-place elements found; we're done adjusting
        break
      }

      // Move the found element into the hole
      nativeStorage[hole] = nativeStorage[b]
      nativeStorage[b] = .None
      hole = b
    }
  }

  mutating func nativeRemoveObjectForKey(key: KeyType) -> ValueType? {
    var nativeStorage = native
    var start = nativeStorage._bucket(key)
    var (index, found) = nativeStorage._find(key, start)

    // Fast path: if the key is not present, we will not mutate the dictionary,
    // so don't force unique storage.
    if !found {
      return .None
    }

    let (reallocated, capacityChanged) =
        ensureUniqueNativeStorage(nativeStorage.capacity)
    if reallocated {
      nativeStorage = native
    }
    if capacityChanged {
      start = nativeStorage._bucket(key)
      (index, found) = nativeStorage._find(key, start)
      _sanityCheck(found, "key was lost during storage migration")
    }

    let oldValue = nativeStorage[index.offset]!.value
    nativeDeleteImpl(nativeStorage, start: start, offset: index.offset)
    return oldValue
  }

  mutating func nativeRemoveAtIndex(nativeIndex: NativeIndex) {
    var nativeStorage = native

    // The provided index should be valid, so we will always mutating the
    // dictionary storage.  Request unique storage.
    let (reallocated, capacityChanged) =
        ensureUniqueNativeStorage(nativeStorage.capacity)
    if reallocated {
      nativeStorage = native
    }

    let key = nativeStorage.assertingGet(nativeIndex).0
    let start = nativeStorage._bucket(key)
    nativeDeleteImpl(nativeStorage, start: start, offset: nativeIndex.offset)
  }

  mutating func removeAtIndex(index: Index) {
    if _fastPath(guaranteedNative) {
      nativeRemoveAtIndex(index._nativeIndex)
    }

    switch self {
    case .Native:
      nativeRemoveAtIndex(index._nativeIndex)
    case .Cocoa(let cocoaStorage):
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      //
      // FIXME(performance): fuse data migration and element deletion into one
      // operation.
      let cocoaIndex = index._cocoaIndex
      let anyObjectKey: AnyObject =
          cocoaIndex.allKeys.objectAtIndex(cocoaIndex.nextKeyIndex)
      migrateDataToNativeStorage(cocoaStorage)
      nativeRemoveObjectForKey(
          bridgeFromObjectiveCUnconditional(anyObjectKey, KeyType.self))
    }
  }

  mutating func removeValueForKey(key: KeyType) -> ValueType? {
    if _fastPath(guaranteedNative) {
      return nativeRemoveObjectForKey(key)
    }

    switch self {
    case .Native:
      return nativeRemoveObjectForKey(key)
    case .Cocoa(let cocoaStorage):
      // FIXME: This assumes that KeyType and ValueType are bridged verbatim.
      let anyObjectKey: AnyObject = _reinterpretCastToAnyObject(key)
      if !cocoaStorage.maybeGet(anyObjectKey) {
        return .None
      }
      migrateDataToNativeStorage(cocoaStorage)
      return nativeRemoveObjectForKey(key)
    }
  }

  var count: Int {
    switch self {
    case .Native:
      return native.count
    case .Cocoa(let cocoaStorage):
      return cocoaStorage.count
    }
  }

  func generate() -> DictionaryGenerator<KeyType, ValueType> {
    switch self {
    case .Native:
      return ._Native(start: native.startIndex, end: native.endIndex)
    case .Cocoa(let cocoaStorage):
      return ._Cocoa(_CocoaDictionaryGenerator(cocoaStorage.cocoaDictionary))
    }
  }

  static func fromArray(
      elements: Array<(KeyType, ValueType)>
  ) -> _VariantDictionaryStorage<KeyType, ValueType> {
    _fatalError("this function should never be called")
  }
}

struct _NativeDictionaryIndex<KeyType : Hashable, ValueType> :
    BidirectionalIndex {

  typealias NativeStorage = _NativeDictionaryStorage<KeyType, ValueType>
  typealias NativeIndex = _NativeDictionaryIndex<KeyType, ValueType>

  var nativeStorage: NativeStorage
  var offset: Int

  init(nativeStorage: NativeStorage, offset: Int) {
    self.nativeStorage = nativeStorage
    self.offset = offset
  }

  func pred() -> NativeIndex {
    var j = offset
    while --j > 0 {
      if nativeStorage[j] {
        return NativeIndex(nativeStorage: nativeStorage, offset: j)
      }
    }
    return self
  }

  func succ() -> NativeIndex {
    var i = offset + 1
    // FIXME: Can't write the simple code pending
    // <rdar://problem/15484639> Refcounting bug
    while i < nativeStorage.capacity /*&& !nativeStorage[i]*/ {
      // FIXME: workaround for <rdar://problem/15484639>
      if nativeStorage[i] {
        break
      }
      // end workaround
      ++i
    }
    return NativeIndex(nativeStorage: nativeStorage, offset: i)
  }
}

func == <KeyType : Hashable, ValueType> (
  lhs: _NativeDictionaryIndex<KeyType, ValueType>,
  rhs: _NativeDictionaryIndex<KeyType, ValueType>
) -> Bool {
  // FIXME: assert that lhs and rhs are from the same dictionary.
  return lhs.offset == rhs.offset
}

struct _CocoaDictionaryIndex : BidirectionalIndex {
  // Assumption: we rely on NSDictionary.allKeys, when being repeatedly
  // called on the same NSDictionary, returning keys in the same order
  // every time.

  typealias Index = _CocoaDictionaryIndex

  let cocoaDictionary: _SwiftNSDictionary
  var allKeys: _SwiftNSArray
  var nextKeyIndex: Int

  init(_ cocoaDictionary: _SwiftNSDictionary, startIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = cocoaDictionary.allKeys
    self.nextKeyIndex = 0
  }

  init(_ cocoaDictionary: _SwiftNSDictionary, endIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = cocoaDictionary.allKeys
    self.nextKeyIndex = allKeys.count
  }

  init(_ cocoaDictionary: _SwiftNSDictionary, _ allKeys: _SwiftNSArray,
       _ nextKeyIndex: Int) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = allKeys
    self.nextKeyIndex = nextKeyIndex
  }

  func pred() -> Index {
    _precondition(nextKeyIndex >= 1, "can not advance startIndex backwards")
    return _CocoaDictionaryIndex(cocoaDictionary, allKeys, nextKeyIndex - 1)
  }

  func succ() -> Index {
    _precondition(nextKeyIndex < allKeys.count, "can not advance endIndex forward")
    return _CocoaDictionaryIndex(cocoaDictionary, allKeys, nextKeyIndex + 1)
  }
}

func ==(lhs: _CocoaDictionaryIndex, rhs: _CocoaDictionaryIndex) -> Bool {
  _precondition(lhs.cocoaDictionary === rhs.cocoaDictionary,
      "can not compare indexes pointing to different dictionaries")
  _precondition(lhs.allKeys.count == rhs.allKeys.count,
      "one or both of the indexes have been invalidated")

  return lhs.nextKeyIndex == rhs.nextKeyIndex
}

enum DictionaryIndex<KeyType : Hashable, ValueType> : BidirectionalIndex {
  // Index for native storage is efficient.  Index for bridged NSDictionary is
  // not, because neither NSEnumerator nor fast enumeration support moving
  // backwards.  Even if they did, there is another issue: NSEnumerator does
  // not support NSCopying, and fast enumeration does not document that it is
  // safe to copy the state.  So, we can not implement Index that is a value
  // type for bridged NSDictionary in terms of Cocoa enumeration facilities.

  typealias _NativeIndex = _NativeDictionaryIndex<KeyType, ValueType>
  typealias _CocoaIndex = _CocoaDictionaryIndex
  case _Native(_NativeIndex)
  case _Cocoa(_CocoaIndex)

  @transparent
  var _guaranteedNative: Bool {
    return !_canBeClass(KeyType.self) && !_canBeClass(ValueType.self)
  }

  @transparent
  var _nativeIndex: _NativeIndex {
    switch self {
    case ._Native(let nativeIndex):
      return nativeIndex
    case ._Cocoa:
      _fatalError("internal error: does not contain a native index")
    }
  }

  @transparent
  var _cocoaIndex: _CocoaIndex {
    switch self {
    case ._Native:
      _fatalError("internal error: does not contain a Cocoa index")
    case ._Cocoa(let cocoaIndex):
      return cocoaIndex
    }
  }

  typealias Index = DictionaryIndex<KeyType, ValueType>

  func pred() -> Index {
    if _fastPath(_guaranteedNative) {
      return ._Native(_nativeIndex.pred())
    }

    switch self {
    case ._Native(let nativeIndex):
      return ._Native(nativeIndex.pred())
    case ._Cocoa(let cocoaIndex):
      return ._Cocoa(cocoaIndex.pred())
    }
  }

  func succ() -> Index {
    if _fastPath(_guaranteedNative) {
      return ._Native(_nativeIndex.succ())
    }

    switch self {
    case ._Native(let nativeIndex):
      return ._Native(nativeIndex.succ())
    case ._Cocoa(let cocoaIndex):
      return ._Cocoa(cocoaIndex.succ())
    }
  }
}

func == <KeyType : Hashable, ValueType> (
  lhs: DictionaryIndex<KeyType, ValueType>,
  rhs: DictionaryIndex<KeyType, ValueType>
) -> Bool {
  if _fastPath(lhs._guaranteedNative) {
    return lhs._nativeIndex == rhs._nativeIndex
  }

  switch (lhs, rhs) {
  case (._Native(let lhsNative), ._Native(let rhsNative)):
    return lhsNative == rhsNative
  case (._Cocoa(let lhsCocoa), ._Cocoa(let rhsCocoa)):
    return lhsCocoa == rhsCocoa
  default:
    _preconditionFailure("comparing indexes from different dictionaries")
  }
}

struct _CocoaFastEnumerationStackBuf {
  // Clang uses 16 pointers.  So do we.
  var item0: Builtin.RawPointer
  var item1: Builtin.RawPointer
  var item2: Builtin.RawPointer
  var item3: Builtin.RawPointer
  var item4: Builtin.RawPointer
  var item5: Builtin.RawPointer
  var item6: Builtin.RawPointer
  var item7: Builtin.RawPointer
  var item8: Builtin.RawPointer
  var item9: Builtin.RawPointer
  var item10: Builtin.RawPointer
  var item11: Builtin.RawPointer
  var item12: Builtin.RawPointer
  var item13: Builtin.RawPointer
  var item14: Builtin.RawPointer
  var item15: Builtin.RawPointer

  @transparent
  var length: Int {
    return 16
  }

  init() {
    item0 = UnsafePointer<RawByte>.null().value
    item1 = item0
    item2 = item0
    item3 = item0
    item4 = item0
    item5 = item0
    item6 = item0
    item7 = item0
    item8 = item0
    item9 = item0
    item10 = item0
    item11 = item0
    item12 = item0
    item13 = item0
    item14 = item0
    item15 = item0

    _sanityCheck(sizeofValue(self) >= sizeof(Builtin.RawPointer.self) * length)
  }
}

@final
class _CocoaDictionaryGenerator : Generator {
  // Cocoa dictionary generator has to be a class, otherwise we can not
  // guarantee that the fast enumeration struct is pinned to a certain memory
  // location.

  let cocoaDictionary: _SwiftNSDictionary
  var fastEnumerationState = _makeSwiftNSFastEnumerationState()
  var fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

  // These members have to be full-sized integers, they can not be limited to
  // Int8 just because our buffer holds 16 elements: fast enumeration is
  // allowed to return inner pointers to the container, which can be much
  // larger.
  var itemIndex: Int = 0
  var itemCount: Int = 0

  init(_ cocoaDictionary: _SwiftNSDictionary) {
    self.cocoaDictionary = cocoaDictionary
  }

  func next() -> (AnyObject, AnyObject)? {
    if itemIndex < 0 {
      return .None
    }
    let cocoaDictionary = self.cocoaDictionary
    if itemIndex == itemCount {
      let stackBufLength = fastEnumerationStackBuf.length
      itemCount = withUnsafePointers(
          &fastEnumerationState, &fastEnumerationStackBuf) {
        (statePtr, bufPtr) -> Int in
        cocoaDictionary.countByEnumeratingWithState(
            statePtr, objects: reinterpretCast(bufPtr),
            count: stackBufLength)
      }
      if itemCount == 0 {
        itemIndex = -1
        return .None
      }
      itemIndex = 0
    }
    let itemsPtrUP: UnsafePointer<AnyObject> =
        UnsafePointer(fastEnumerationState.itemsPtr)
    let itemsPtr = _UnmanagedAnyObjectArray(itemsPtrUP)
    let key: AnyObject = itemsPtr[itemIndex]
    ++itemIndex
    let value: AnyObject = cocoaDictionary.objectForKey(key)!
    return (key, value)
  }
}

enum DictionaryGenerator<KeyType : Hashable, ValueType> : Generator {
  // Dictionary has a separate Generator and Index because of efficiency
  // and implementability reasons.
  //
  // Index for native storage is efficient.  Index for bridged NSDictionary is
  // not.
  //
  // Even though fast enumeration is not suitable for implementing Index, which
  // is multi-pass, it is suitable for implementing a Generator, which is being
  // consumed as iteration proceeds.

  typealias _NativeIndex = _NativeDictionaryIndex<KeyType, ValueType>

  case _Native(start: _NativeIndex, end: _NativeIndex)
  case _Cocoa(_CocoaDictionaryGenerator)

  @transparent
  var _guaranteedNative: Bool {
    return !_canBeClass(KeyType.self) && !_canBeClass(ValueType.self)
  }

  mutating func _nativeNext() -> (KeyType, ValueType)? {
    switch self {
    case ._Native(var startIndex, var endIndex):
      if startIndex == endIndex {
        return .None
      }
      let result = startIndex.nativeStorage.assertingGet(startIndex)
      self = ._Native(start: startIndex.succ(), end: endIndex)
      return result
    case ._Cocoa:
      _fatalError("internal error: not baked by NSDictionary")
    }
  }

  mutating func next() -> (KeyType, ValueType)? {
    if _fastPath(_guaranteedNative) {
      return _nativeNext()
    }

    switch self {
    case ._Native(var startIndex, var endIndex):
      return _nativeNext()
    case ._Cocoa(var cocoaGenerator):
      if let (key: AnyObject, value: AnyObject) = cocoaGenerator.next() {
        // FIXME: This assumes that KeyType and ValueType are bridged verbatim.
        let nativeKey = reinterpretCast(key) as KeyType
        let nativeValue = reinterpretCast(value) as ValueType
        return (nativeKey, nativeValue)
      }
      return .None
    }
  }
}

struct Dictionary<KeyType : Hashable, ValueType> : Collection,
                                                   DictionaryLiteralConvertible {
  typealias _Self = Dictionary<KeyType, ValueType>
  typealias _VariantStorage = _VariantDictionaryStorage<KeyType, ValueType>
  typealias _NativeStorage = _NativeDictionaryStorage<KeyType, ValueType>
  typealias Element = (KeyType, ValueType)
  typealias Index = DictionaryIndex<KeyType, ValueType>

  var _variantStorage: _VariantStorage

  /// Create a dictionary with at least the given number of
  /// elements worth of storage.  The actual capacity will be the
  /// smallest power of 2 that's >= `minimumCapacity`.
  init(minimumCapacity: Int = 2) {
    _variantStorage =
        .Native(_NativeStorage.Owner(minimumCapacity: minimumCapacity))
  }

  /// Private initializer.
  init(_nativeStorage: _NativeDictionaryStorage<KeyType, ValueType>) {
    _variantStorage =
        .Native(_NativeStorage.Owner(nativeStorage: _nativeStorage))
  }

  /// Private initializer used for bridging.
  init(_cocoaDictionary: _SwiftNSDictionary) {
    _variantStorage =
        .Cocoa(_CocoaDictionaryStorage(cocoaDictionary: _cocoaDictionary))
  }

  //
  // All APIs below should dispatch to `_variantStorage`, without doing any
  // additional processing.
  //

  var startIndex: Index {
    // Complexity: amortized O(1) for native storage, O(N) when wrapping an
    // NSDictionary.
    return _variantStorage.startIndex
  }

  var endIndex: Index {
    // Complexity: amortized O(1) for native storage, O(N) when wrapping an
    // NSDictionary.
    return _variantStorage.endIndex
  }

  /// Returns the `Index` for the given key, or `nil` if the key is not
  /// present in the dictionary.
  func indexForKey(key: KeyType) -> Index? {
    // Complexity: amortized O(1) for native storage, O(N) when wrapping an
    // NSDictionary.
    return _variantStorage.indexForKey(key)
  }

  /// Access the key-value pair referenced by the given index.
  ///
  /// Complexity: O(1)
  subscript(i: Index) -> Element {
    return _variantStorage.assertingGet(i)
  }

  subscript(key: KeyType) -> ValueType? {
    get {
      return _variantStorage.maybeGet(key)
    }
    set(newValue) {
      if let x = newValue {
        // FIXME(performance): this loads and discards the old value.
        _variantStorage.updateValue(x, forKey: key)
      }
      else {
        // FIXME(performance): this loads and discards the old value.
        removeValueForKey(key)
      }
    }
  }

  /// Update the value stored in the dictionary for the given key, or, if they
  /// key does not exist, add a new key-value pair to the dictionary.
  ///
  /// Returns the value that was replaced, or `nil` if a new key-value pair
  /// was added.
  mutating func updateValue(value: ValueType, forKey key: KeyType) -> ValueType? {
    return _variantStorage.updateValue(value, forKey: key)
  }

  /// Remove the key-value pair referenced by the given index.
  mutating func removeAtIndex(index: Index) {
    _variantStorage.removeAtIndex(index)
  }

  /// Remove a given key and the associated value from the dictionary.
  /// Returns the value that was removed, or `nil` if the key was not present
  /// in the dictionary.
  mutating func removeValueForKey(key: KeyType) -> ValueType? {
    return _variantStorage.removeValueForKey(key)
  }

  /// The number of entries in the dictionary.
  ///
  /// Complexity: O(1)
  var count: Int {
    return _variantStorage.count
  }

  //
  // `Sequence` conformance
  //

  func generate() -> DictionaryGenerator<KeyType, ValueType> {
    return _variantStorage.generate()
  }

  //
  // DictionaryLiteralConvertible conformance
  //
  static func convertFromDictionaryLiteral(elements: (KeyType, ValueType)...)
                -> Dictionary<KeyType, ValueType> {
    return Dictionary<KeyType, ValueType>(
        _nativeStorage: _NativeDictionaryStorage.fromArray(elements))
  }

  //
  // APIs below this comment should be implemented strictly in terms of
  // *public* APIs above.  `_variantStorage` should not be accessed directly.
  //
  // This separates concerns for testing.  Tests for the following APIs need
  // not to concern themselves with testing correctness of behavior of
  // underlying storage (and different variants of it), only correctness of the
  // API itself.
  //

  var keys: MapCollectionView<Dictionary, KeyType> {
    return map(self) { $0.0 }
  }

  var values: MapCollectionView<Dictionary, ValueType> {
    return map(self) { $0.1 }
  }
}

func == <KeyType : Equatable, ValueType : Equatable>(
  lhs: Dictionary<KeyType, ValueType>,
  rhs: Dictionary<KeyType, ValueType>
) -> Bool {
  switch (lhs._variantStorage, rhs._variantStorage) {
  case (.Native(let lhsNativeOwner), .Native(let rhsNativeOwner)):
    let lhsNative = lhsNativeOwner.nativeStorage
    let rhsNative = rhsNativeOwner.nativeStorage
    // FIXME(performance): early exit if lhs and rhs reference the same
    // storage?

    if lhsNative.count != rhsNative.count {
      return false
    }

    for (k, v) in lhs {
      var (pos, found) = rhsNative._find(k, rhsNative._bucket(k))
      // FIXME: Can't write the simple code pending
      // <rdar://problem/15484639> Refcounting bug
      /*
      if !found || rhs[pos].value != lhsElement.value {
        return false
      }
      */
      if !found {
        return false
      }
      if rhsNative[pos.offset]!.value != v {
        return false
      }
    }
    return true

  case (.Cocoa(let lhsCocoa), .Cocoa(let rhsCocoa)):
    if lhsCocoa.cocoaDictionary === rhsCocoa.cocoaDictionary {
      return true
    }
    // FIXME: implement when we have dictionary casts.
    //return lhsCocoa.cocoaDictionary.isEqual(rhsCocoa.cocoaDictionary)
    _fatalError("dictionaries backed by Cocoa storage are not Equatable when obtained using public interface")

  case (.Native(let lhsNativeOwner), .Cocoa(let rhsCocoa)):
    // FIXME: compare elements.
    // FIXME: implement when we have dictionary casts.
    _fatalError("dictionaries backed by Cocoa storage are not Equatable when obtained using public interface")

  case (.Cocoa, .Native):
    return rhs == lhs
  }
}

func != <KeyType : Equatable, ValueType : Equatable>(
  lhs: Dictionary<KeyType, ValueType>,
  rhs: Dictionary<KeyType, ValueType>
) -> Bool {
  return !(lhs == rhs)
}

extension Dictionary : Printable, DebugPrintable {
  func _makeDescription(#isDebug: Bool) -> String {
    if count == 0 {
      return "[:]"
    }

    var result = "["
    var first = true
    for (k, v) in self {
      if first {
        first = false
      } else {
        result += ", "
      }
      if isDebug {
        debugPrint(k, &result)
      } else {
        print(k, &result)
      }
      result += ": "
      if isDebug {
        debugPrint(v, &result)
      } else {
        print(v, &result)
      }
    }
    result += "]"
    return result
  }

  var description: String {
    return _makeDescription(isDebug: false)
  }

  var debugDescription: String {
    return _makeDescription(isDebug: true)
  }
}

// this should be nested within _DictionaryMirror, but that causes
// the compiler to crash
struct _DictionaryMirrorPosition<Key : Hashable,Value> {
  typealias Dict = Dictionary<Key,Value>

  var _intPos : Int
  var _dicPos : Dict.Index

  init(_ d : Dict) {
    _intPos = 0
    _dicPos = d.startIndex
  }

  mutating func succ() {
    _intPos = _intPos + 1
    _dicPos = _dicPos.succ()
  }

  mutating func prec() {
    _intPos = _intPos - 1
    _dicPos = _dicPos.pred()
  }
}

func ==<K : Hashable,V> (lhs : _DictionaryMirrorPosition<K,V>, rhs : Int) -> Bool {
  return lhs._intPos == rhs
}

func > <K : Hashable,V> (lhs : _DictionaryMirrorPosition<K,V>, rhs : Int) -> Bool {
  return lhs._intPos > rhs
}

func < <K : Hashable,V> (lhs : _DictionaryMirrorPosition<K,V>, rhs : Int) -> Bool {
  return lhs._intPos < rhs
}

//===--- Mirroring---------------------------------------------------------===//
class _DictionaryMirror<Key : Hashable,Value> : Mirror {
  typealias Dict = Dictionary<Key,Value>
  let _dict : Dict
  var _pos : _DictionaryMirrorPosition<Key,Value>

  init(_ d : Dict) {
    _dict = d
    _pos = _DictionaryMirrorPosition(d)
  }

  var value: Any { return (_dict as Any) }

  var valueType: Any.Type { return (_dict as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return nil }

  var count: Int { return _dict.count }

  subscript(i: Int) -> (String, Mirror) {
    // this use of indexes is optimized for a world of contiguous accesses
    // i.e. we expect users to start asking for children in a range, then maybe
    // shift to a different range, .. and so on
    if (i >= 0) && (i < count) {
      while _pos < i {
        _pos.succ()
      }
      while _pos > i {
        _pos.prec()
      }
      return ("[\(_pos._intPos)]",reflect(_dict[_pos._dicPos]))
    }
    _fatalError("don't ask")
  }

  var summary: String {
    if count == 1 {
      return "1 key/value pair"
    }
    return "\(count) key/value pairs"
  }

  var quickLookObject: QuickLookObject? { return nil }

  var disposition: MirrorDisposition { return .KeyContainer }
}

extension Dictionary : Reflectable {
  func getMirror() -> Mirror {
    return _DictionaryMirror(self)
  }
}

//===--- Mocks of Cocoa types that we use ---------------------------------===//

import SwiftShims

@objc
protocol _SwiftNSFastEnumeration {
  func countByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int
  ) -> Int
}

@objc
protocol _SwiftNSEnumerator {
  init()
  func nextObject() -> AnyObject?
}

typealias _SwiftNSZone = COpaquePointer

@objc
protocol _SwiftNSCopying {
  func copyWithZone(zone: _SwiftNSZone) -> AnyObject
}

@objc
protocol _SwiftNSArrayRequiredOverrides :
    _SwiftNSCopying, _SwiftNSFastEnumeration {

  func objectAtIndex(index: Int) -> AnyObject

  func getObjects(UnsafePointer<AnyObject>, range: _SwiftNSRange)

  func countByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int
  ) -> Int

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  var count: Int { get }
}

// FIXME: replace _CocoaArray with this.
@objc
protocol _SwiftNSArray : _SwiftNSArrayRequiredOverrides {
  func indexOfObject(anObject: AnyObject) -> Int
}

@objc
protocol _SwiftNSDictionaryRequiredOverrides :
    _SwiftNSCopying, _SwiftNSFastEnumeration {

  // The following methods should be overridden when implementing an
  // NSDictionary subclass.

  // The designated initializer of `NSDictionary`.
  init(objects: CConstPointer<AnyObject?>, forKeys: CConstVoidPointer,
       count: Int)
  var count: Int { get }
  func objectForKey(aKey: AnyObject?) -> AnyObject?
  func keyEnumerator() -> _SwiftNSEnumerator?

  // We also override the following methods for efficiency.

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  func countByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafePointer<AnyObject>, count: Int
  ) -> Int
}

@objc
protocol _SwiftNSDictionary : _SwiftNSDictionaryRequiredOverrides {
  var allKeys:  _SwiftNSArray { get }
  func isEqual(anObject: AnyObject) -> Bool
}

/// This class is derived from `_NSSwiftDictionaryBase` (through runtime magic),
/// which is derived from `NSDictionary`.
///
/// This allows us to subclass an Objective-C class and use the fast Swift
/// memory allocator.
@objc
class _NSSwiftDictionary {}

/// This class is derived from `_NSSwiftEnumeratorBase` (through runtime magic),
/// which is derived from `NSEnumerator`.
///
/// This allows us to subclass an Objective-C class and use the fast Swift
/// memory allocator.
@objc
class _NSSwiftEnumerator {}

//===--- Hacks and workarounds --------------------------------------------===//

/// Like `UnsafePointer<Unmanaged<AnyObject>>`, or `id __unsafe_unretained *` in
/// Objective-C ARC.
struct _UnmanagedAnyObjectArray {
  // `UnsafePointer<Unmanaged<AnyObject>>` fails because of:
  // <rdar://problem/16836348> IRGen: Couldn't find conformance

  /// Underlying pointer, typed as an integer to escape from reference
  /// counting.
  var value: UnsafePointer<Word>

  init(_ up: UnsafePointer<AnyObject>) {
    self.value = UnsafePointer(up)
  }

  subscript(i: Int) -> AnyObject {
    get {
      return _reinterpretCastToAnyObject(value[i])
    }
    nonmutating set(newValue) {
      value[i] = reinterpretCast(newValue) as Word
    }
  }
}

