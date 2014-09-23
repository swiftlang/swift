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
// incrementing/decrementing indices of an CollectionType is the usual ++i,
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
// The buckets are typed as `Optional<(Key, Value)>`.  A `.None` value
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
// be returned to Objective-C during bridging if both `Key` and `Value`
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
//   to `NSDictionary` in `O(1)`, without memory allocation.  Access to
//   elements does not cause memory allocation.
//
// * otherwise, `K` and/or `V` are unconditionally or conditionally bridged.
//   In this case, `Dictionary<K, V>` is bridged to `NSDictionary` in `O(1)`,
//   without memory allocation.  Complete bridging is performed when the first
//   access to elements happens.  The bridged `NSDictionary` has a cache of
//   pointers it returned, so that:
//   - Every time keys or values are accessed on the bridged `NSDictionary`,
//     new objects are not created.
//   - Accessing the same element (key or value) multiple times will return
//     the same pointer.
//

/// This protocol is only used for compile-time checks that
/// every storage type implements all required operations.
protocol _DictionaryStorageType {
  typealias Key
  typealias Value
  typealias Index
  var startIndex: Index { get }
  var endIndex: Index { get }
  func indexForKey(key: Key) -> Index?
  func assertingGet(i: Index) -> (Key, Value)
  func assertingGet(key: Key) -> Value
  func maybeGet(key: Key) -> Value?
  mutating func updateValue(value: Value, forKey: Key) -> Value?
  mutating func removeAtIndex(index: Index)
  mutating func removeValueForKey(key: Key) -> Value?
  mutating func removeAll(#keepCapacity: Bool)
  var count: Int { get }

  class func fromArray(elements: Array<(Key, Value)>) -> Self
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
struct _DictionaryElement<Key, Value> {
  let key: Key
  var value: Value
}

/// An instance of this class has all dictionary data tail-allocated.  It is
/// used as a `HeapBuffer` storage.
final class _NativeDictionaryStorageImpl<Key, Value> {

  typealias Element = _DictionaryElement<Key, Value>
  typealias DictionaryHeapBuffer = HeapBuffer<_DictionaryBody, Element?>

  deinit {
    // FIXME: this cast is invalid.
    let buffer = DictionaryHeapBuffer(
      unsafeBitCast(self, DictionaryHeapBuffer.Storage.self))
    let body = buffer.value
    buffer._value.destroy()
    buffer.baseAddress.destroy(body.capacity)
  }
  final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    let buffer = DictionaryHeapBuffer(
      unsafeBitCast(self, DictionaryHeapBuffer.Storage.self))
    return buffer._allocatedSizeAndAlignMask()
  }
}

struct _NativeDictionaryStorage<Key : Hashable, Value> :
    _DictionaryStorageType, Printable {

  typealias Owner = _NativeDictionaryStorageOwner<Key, Value>
  typealias StorageImpl = _NativeDictionaryStorageImpl<Key, Value>
  typealias Element = _DictionaryElement<Key, Value>

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
  var elements: UnsafeMutablePointer<Element?> {
    return buffer.baseAddress
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

    self = _NativeDictionaryStorage<Key, Value>(capacity: capacity)
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
      _sanityCheck(maxLoadFactorInverse > 0)
      return 1.0 / maxLoadFactorInverse
    }
    set(newValue) {
      // 1.0 might be useful for testing purposes; anything more is
      // crazy
      _sanityCheck(newValue <= 1.0)
      _sanityCheck(newValue > 0)
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

  func _bucket(k: Key) -> Int {
    return _squeezeHashValue(k.hashValue, 0..<capacity)
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
  func _find(k: Key, _ startBucket: Int) -> (pos: Index, found: Bool) {
    var bucket = startBucket

    // The invariant guarantees there's always a hole, so we just loop
    // until we find one
    while true {
      var keyVal = self[bucket]
      if (keyVal == nil) || keyVal!.key == k {
        return (Index(nativeStorage: self, offset: bucket), (keyVal != nil))
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
  mutating func unsafeAddNew(#key: Key, value: Value) {
    var (i, found) = _find(key, _bucket(key))
    _sanityCheck(
      !found, "unsafeAddNew was called, but the key is already present")
    self[i.offset] = Element(key: key, value: value)
  }

  var description: String {
    var result = ""
#if INTERNAL_CHECKS_ENABLED
    for var i = 0; i != capacity; ++i {
      if let key = self[i]?.key {
        result += "bucket \(i), ideal bucket = \(_bucket(key)), key = \(key)\n"
      } else {
        result += "bucket \(i), empty\n"
      }
    }
#endif
    return result
  }

  //
  // _DictionaryStorageType conformance
  //

  typealias Index = _NativeDictionaryIndex<Key, Value>

  var startIndex: Index {
    return Index(nativeStorage: self, offset: -1).successor()
  }

  var endIndex: Index {
    return Index(nativeStorage: self, offset: capacity)
  }

  func indexForKey(key: Key) -> Index? {
    let (i, found) = _find(key, _bucket(key))
    return found ? i : .None
  }

  func assertingGet(i: Index) -> (Key, Value) {
    let e = self[i.offset]
    _precondition(
      e != nil, "attempting to access Dictionary elements using an invalid Index")
    return (e!.key, e!.value)
  }

  func assertingGet(key: Key) -> Value {
    let e = self[_find(key, _bucket(key)).pos.offset]
    _precondition(e != nil, "key not found in Dictionary")
    return e!.value
  }

  func maybeGet(key: Key) -> Value? {
    let (i, found) = _find(key, _bucket(key))
    if found {
      return self[i.offset]!.value
    }
    return .None
  }

  mutating func updateValue(value: Value, forKey: Key) -> Value? {
    _sanityCheckFailure(
      "don't call mutating methods on _NativeDictionaryStorage")
  }

  mutating func removeAtIndex(index: Index) {
    _sanityCheckFailure(
      "don't call mutating methods on _NativeDictionaryStorage")
  }

  mutating func removeValueForKey(key: Key) -> Value? {
    _sanityCheckFailure(
      "don't call mutating methods on _NativeDictionaryStorage")
  }

  mutating func removeAll(#keepCapacity: Bool) {
    _sanityCheckFailure(
      "don't call mutating methods on _NativeDictionaryStorage")
  }

  static func fromArray(
      elements: Array<(Key, Value)>
  ) -> _NativeDictionaryStorage<Key, Value> {
    let requiredCapacity =
      _NativeDictionaryStorage<Key, Value>.getMinCapacity(
          elements.count, _dictionaryDefaultMaxLoadFactorInverse)
    var nativeStorage = _NativeDictionaryStorage<Key, Value>(
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

/// Storage for bridged `Dictionary` elements.  We could have used
/// `Dictionary<AnyObject, AnyObject>`, but `AnyObject` can not be a
/// `Dictionary` key because it is not `Hashable`.
struct _BridgedNativeDictionaryStorage {

  typealias Element = _DictionaryElement<AnyObject, AnyObject>
  typealias StorageImpl = _NativeDictionaryStorageImpl<AnyObject, AnyObject>

  let buffer: StorageImpl.DictionaryHeapBuffer

  init(buffer: StorageImpl.DictionaryHeapBuffer) {
    self.buffer = buffer
  }

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
  var elements: UnsafeMutablePointer<Element?> {
    return buffer.baseAddress
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

  func assertingGet(i: Int) -> (AnyObject, AnyObject) {
    let e = self[i]
    _precondition(
      e != nil, "attempting to access Dictionary elements using an invalid Index")
    return (e!.key, e!.value)
  }
}

/// This class existis only to work around a compiler limitation.
/// Specifically, we can not have @objc members in a generic class.  When this
/// limitation is gone, this class can be folded into
/// `_NativeDictionaryStorageKeyNSEnumerator`.
@objc
class _NativeDictionaryStorageKeyNSEnumeratorBase
    : _NSSwiftEnumerator, _SwiftNSEnumeratorType {

  init(dummy: (Int, ())) {}

  func bridgingNextObject(dummy: ()) -> AnyObject? {
    _sanityCheckFailure("'bridgingNextObject' should be overridden")
  }

  func bridgingCountByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    _sanityCheckFailure("'countByEnumeratingWithState' should be overridden")
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
  required override init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc
  func nextObject() -> AnyObject? {
    return bridgingNextObject(())
  }

  @objc
  func countByEnumeratingWithState(
      state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
      objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int {
    return bridgingCountByEnumeratingWithState(
        state, objects: objects, count: count, dummy: ())
  }
}

@objc final
class _NativeDictionaryStorageKeyNSEnumerator<Key : Hashable, Value>
    : _NativeDictionaryStorageKeyNSEnumeratorBase {

  typealias NativeStorageOwner = _NativeDictionaryStorageOwner<Key, Value>
  typealias Index = _NativeDictionaryIndex<Key, Value>

  required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  init(_ nativeStorageOwner: NativeStorageOwner) {
    self.nativeStorageOwner = nativeStorageOwner
    nextIndex = nativeStorageOwner.nativeStorage.startIndex
    endIndex = nativeStorageOwner.nativeStorage.endIndex
    super.init(dummy: (0, ()))
  }

  var nativeStorageOwner: NativeStorageOwner
  var nextIndex: Index
  var endIndex: Index

  //
  // Dictionary -> NSDictionary bridging.
  //

  override func bridgingNextObject(dummy: ()) -> AnyObject? {
    if nextIndex == endIndex {
      return nil
    }
    let bridgedKey: AnyObject = nativeStorageOwner._getBridgedKey(nextIndex)
    nextIndex = nextIndex.successor()
    return bridgedKey
  }

  override func bridgingCountByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    var theState = state.memory
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }

    if nextIndex == endIndex {
      state.memory = theState
      return 0
    }

    // Return only a single element so that code can start iterating via fast
    // enumeration, terminate it, and continue via NSEnumerator.
    let bridgedKey: AnyObject = nativeStorageOwner._getBridgedKey(nextIndex)
    nextIndex = nextIndex.successor()

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = bridgedKey
    state.memory = theState
    return 1
  }
}

/// This class existis only to work around a compiler limitation.
/// Specifically, we can not have objc members in a generic class.  When this
/// limitation is gone, this class can be folded into
/// `_NativeDictionaryStorageOwner`.
@objc
class _NativeDictionaryStorageOwnerBase
    : _NSSwiftDictionary, _SwiftNSDictionaryRequiredOverridesType {

  override init() {}

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  // subclass of an @objc class has no effect
  var bridgingCount: (Int, ()) {
    _sanityCheckFailure("'bridgingCount' should be overridden")
  }

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  func bridgingObjectForKey(aKey: AnyObject, dummy: ()) -> AnyObject? {
    _sanityCheckFailure("'bridgingObjectForKey' should be overridden")
  }

  // Empty tuple is a workaround for
  // <rdar://problem/16824792> Overriding functions and properties in a generic
  func bridgingKeyEnumerator(dummy: ()) -> _SwiftNSEnumeratorType {
    _sanityCheckFailure("'bridgingKeyEnumerator' should be overridden")
  }

  func bridgingCountByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    _sanityCheckFailure("'countByEnumeratingWithState' should be overridden")
  }

  //
  // NSDictionary implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `nativeStorage`.
  //

  @objc
  required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafePointer<Void>,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
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
  func keyEnumerator() -> _SwiftNSEnumeratorType? {
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
      state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
      objects: UnsafeMutablePointer<AnyObject>, count: Int
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
final class _NativeDictionaryStorageOwner<Key : Hashable, Value>
    : _NativeDictionaryStorageOwnerBase {

  typealias NativeStorage = _NativeDictionaryStorage<Key, Value>
  typealias BridgedNativeStorage = _BridgedNativeDictionaryStorage

  required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafePointer<Void>,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  init(minimumCapacity: Int = 2) {
    nativeStorage = NativeStorage(minimumCapacity: minimumCapacity)
    super.init()
  }

  init(nativeStorage: _NativeDictionaryStorage<Key, Value>) {
    self.nativeStorage = nativeStorage
    super.init()
  }

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  var _heapBufferBridged_DoNotUse: AnyObject? = nil

  var nativeStorage: NativeStorage

  /// Returns the pointer to the stored property, which contains bridged
  /// Dictionary elements.
  var _heapBufferBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return UnsafeMutablePointer(_getUnsafePointerToStoredProperties(self))
  }

  /// The storage for bridged Dictionary elements, if present.
  var _heapBufferBridged:
    BridgedNativeStorage.StorageImpl.DictionaryHeapBuffer.Storage? {
    if let ref: AnyObject =
      _stdlib_atomicLoadARCRef(object: _heapBufferBridgedPtr) {
      // FIXME: this cast is invalid.
      return unsafeBitCast(
        ref,
        BridgedNativeStorage.StorageImpl.DictionaryHeapBuffer.Storage.self)
    }
    return nil
  }

  /// Attach a storage for bridged Dictionary elements.
  func _initializeHeapBufferBridged(newBuffer: AnyObject) {
    _stdlib_atomicInitializeARCRef(
      object: _heapBufferBridgedPtr, desired: newBuffer)
  }

  /// Detach the storage of bridged Dictionary elements.
  ///
  /// Call this before mutating the dictionary storage owned by this owner.
  func deinitializeHeapBufferBridged() {
    // Perform a non-atomic store because storage should be
    // uniquely-referenced.
    let ptr = UnsafeMutablePointer<COpaquePointer>(_heapBufferBridgedPtr).memory
    if ptr != nil {
      Unmanaged<AnyObject>.fromOpaque(ptr).takeRetainedValue()
      _heapBufferBridgedPtr.memory = nil
    }
  }

  /// Returns the bridged Dictionary values.
  var bridgedNativeStorage: BridgedNativeStorage {
    return BridgedNativeStorage(buffer: HeapBuffer(_heapBufferBridged!))
  }

  func _createBridgedNativeStorage(capacity: Int) -> BridgedNativeStorage {
    let body = _DictionaryBody(capacity: capacity)
    let buffer = BridgedNativeStorage.StorageImpl.DictionaryHeapBuffer(
      BridgedNativeStorage.StorageImpl.self, body, capacity)
    let elements = buffer.baseAddress
    for var i = 0; i < capacity; ++i {
      (elements + i).initialize(.None)
    }
    return BridgedNativeStorage(buffer: buffer)
  }

  func bridgeEverything() {
    if _fastPath(_heapBufferBridged != nil) {
      return
    }

    // Create storage for bridged data.
    let bridged = _createBridgedNativeStorage(nativeStorage.capacity)

    // Bridge everything.
    for var i = 0; i < nativeStorage.capacity; ++i {
      if let nativeElement = nativeStorage[i] {
        bridged[i] = _DictionaryElement<AnyObject, AnyObject>(
          key: _bridgeToObjectiveCUnconditional(nativeElement.key),
          value: _bridgeToObjectiveCUnconditional(nativeElement.value))
      }
    }

    // Atomically put the bridged elements in place.
    _initializeHeapBufferBridged(bridged.buffer.storage!)
  }

  //
  // Entry points for bridging Dictionary elements.  In implementations of
  // Foundation subclasses (NSDictionary, NSEnumerator), don't access any
  // storage directly, use these functions.
  //

  func _getBridgedKey(i: _NativeDictionaryIndex<Key, Value>) -> AnyObject {
    if _fastPath(_isClassOrObjCExistential(Key.self)) {
      return _bridgeToObjectiveCUnconditional(nativeStorage.assertingGet(i).0)
    }
    bridgeEverything()
    return bridgedNativeStorage.assertingGet(i.offset).0
  }

  func _getBridgedValue(i: _NativeDictionaryIndex<Key, Value>) -> AnyObject {
    if _fastPath(_isClassOrObjCExistential(Value.self)) {
      return _bridgeToObjectiveCUnconditional(nativeStorage.assertingGet(i).1)
    }
    bridgeEverything()
    return bridgedNativeStorage.assertingGet(i.offset).1
  }

  //
  // Dictionary -> NSDictionary bridging.
  //

  override var bridgingCount: (Int, ()) {
    return (nativeStorage.count, ())
  }

  override func bridgingObjectForKey(aKey: AnyObject, dummy: ()) -> AnyObject? {
    let nativeKey = _forceBridgeFromObjectiveC(aKey, Key.self)
    let (i, found) = nativeStorage._find(
      nativeKey, nativeStorage._bucket(nativeKey))
    if found {
      return _getBridgedValue(i)
    }
    return nil
  }

  override func bridgingKeyEnumerator(dummy: ()) -> _SwiftNSEnumeratorType {
    return _NativeDictionaryStorageKeyNSEnumerator<Key, Value>(self)
  }

  override func bridgingCountByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int, dummy: ()
  ) -> Int {
    var theState = state.memory
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(nativeStorage.startIndex.offset)
    }
    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    var currIndex = _NativeDictionaryIndex<Key, Value>(
        nativeStorage: nativeStorage, offset: Int(theState.extra.0))
    let endIndex = nativeStorage.endIndex
    var stored = 0
    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      var bridgedKey: AnyObject = _getBridgedKey(currIndex)
      unmanagedObjects[i] = bridgedKey
      ++stored
      currIndex = currIndex.successor()
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.memory = theState
    return stored
  }
}

struct _CocoaDictionaryStorage : _DictionaryStorageType {
  var cocoaDictionary: _SwiftNSDictionaryType

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
    if maybeGet(key) == nil {
      return .None
    }

    let allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    var keyIndex = -1
    for i in 0..<allKeys.value {
      if _stdlib_NSObject_isEqual(key, allKeys[i]) {
        keyIndex = i
        break
      }
    }
    _sanityCheck(keyIndex >= 0,
        "key was found in fast path, but not found later?")
    return Index(cocoaDictionary, allKeys, keyIndex)
  }

  func assertingGet(i: Index) -> (AnyObject, AnyObject) {
    let key: AnyObject = i.allKeys[i.currentKeyIndex]
    let value: AnyObject = i.cocoaDictionary.objectForKey(key)!
    return (key, value)
  }

  func assertingGet(key: AnyObject) -> AnyObject {
    let value: AnyObject? = cocoaDictionary.objectForKey(key)
    _precondition(value != nil, "key not found in underlying NSDictionary")
    return value!
  }

  func maybeGet(key: AnyObject) -> AnyObject? {
    return cocoaDictionary.objectForKey(key)
  }

  mutating func updateValue(value: AnyObject, forKey: AnyObject) -> AnyObject? {
    _sanityCheckFailure("can not mutate NSDictionary")
  }

  mutating func removeAtIndex(index: Index) {
    _sanityCheckFailure("can not mutate NSDictionary")
  }

  mutating func removeValueForKey(key: AnyObject) -> AnyObject? {
    _sanityCheckFailure("can not mutate NSDictionary")
  }

  mutating func removeAll(#keepCapacity: Bool) {
    _sanityCheckFailure("can not mutate NSDictionary")
  }

  var count: Int {
    return cocoaDictionary.count
  }

  static func fromArray(
      elements: Array<(AnyObject, AnyObject)>
  ) -> _CocoaDictionaryStorage {
    _sanityCheckFailure("this function should never be called")
  }
}

enum _VariantDictionaryStorage<Key : Hashable, Value> :
    _DictionaryStorageType {

  typealias _NativeStorageElement = _DictionaryElement<Key, Value>
  typealias NativeStorage = _NativeDictionaryStorage<Key, Value>
  typealias NativeStorageOwner = _NativeDictionaryStorageOwner<Key, Value>
  typealias CocoaStorage = _CocoaDictionaryStorage
  typealias NativeIndex = _NativeDictionaryIndex<Key, Value>

  case Native(NativeStorageOwner)
  case Cocoa(CocoaStorage)

  @transparent
  var guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 && _canBeClass(Value.self) == 0
  }

  mutating func isUniquelyReferenced() -> Bool {
    if _fastPath(guaranteedNative) {
      return Swift._isUniquelyReferenced(&self)
    }

    switch self {
    case .Native:
      return Swift._isUniquelyReferenced(&self)
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
      _sanityCheckFailure("internal error: not backed by native storage")
    }
  }

  var cocoa: CocoaStorage {
    switch self {
    case .Native:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
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
        // Clear the cache of bridged elements.
        switch self {
        case .Native(let owner):
          owner.deinitializeHeapBufferBridged()
        case .Cocoa:
          _sanityCheckFailure("internal error: not backed by native storage")
        }
        return (reallocated: false, capacityChanged: false)
      }

      let newNativeOwner = NativeStorageOwner(minimumCapacity: minimumCapacity)
      var newNativeStorage = newNativeOwner.nativeStorage
      let newCapacity = newNativeStorage.capacity

      for i in 0..<oldCapacity {
        var x = oldNativeStorage[i]
        if x != nil {
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
            key: _forceBridgeFromObjectiveC(key, Key.self),
            value: _forceBridgeFromObjectiveC(value, Value.self))
      }
      newNativeStorage.count = cocoaDictionary.count

      self = .Native(newNativeOwner)
      return (reallocated: true, capacityChanged: true)
    }
  }

  mutating func migrateDataToNativeStorage(
    cocoaStorage: _CocoaDictionaryStorage
  ) {
    let minCapacity = NativeStorage.getMinCapacity(
        cocoaStorage.count, _dictionaryDefaultMaxLoadFactorInverse)
    let allocated = ensureUniqueNativeStorage(minCapacity).reallocated
    _sanityCheck(allocated, "failed to allocate native dictionary storage")
  }

  //
  // _DictionaryStorageType conformance
  //

  typealias Index = DictionaryIndex<Key, Value>

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

  func indexForKey(key: Key) -> Index? {
    switch self {
    case .Native:
      if let nativeIndex = native.indexForKey(key) {
        return .Some(._Native(nativeIndex))
      }
      return .None
    case .Cocoa(let cocoaStorage):
      let anyObjectKey: AnyObject = _bridgeToObjectiveCUnconditional(key)
      if let cocoaIndex = cocoaStorage.indexForKey(anyObjectKey) {
        return .Some(._Cocoa(cocoaIndex))
      }
      return .None
    }
  }

  func assertingGet(i: Index) -> (Key, Value) {
    switch self {
    case .Native:
      return native.assertingGet(i._nativeIndex)
    case .Cocoa(let cocoaStorage):
      var (anyObjectKey: AnyObject, anyObjectValue: AnyObject) =
          cocoaStorage.assertingGet(i._cocoaIndex)
      let nativeKey = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
      return (nativeKey, nativeValue)
    }
  }

  func assertingGet(key: Key) -> Value {
    switch self {
    case .Native:
      return native.assertingGet(key)
    case .Cocoa(let cocoaStorage):
      // FIXME: This assumes that Key and Value are bridged verbatim.
      let anyObjectKey: AnyObject = _bridgeToObjectiveCUnconditional(key)
      let anyObjectValue: AnyObject = cocoaStorage.assertingGet(anyObjectKey)
      return _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
    }
  }

  func maybeGet(key: Key) -> Value? {
    switch self {
    case .Native:
      return native.maybeGet(key)
    case .Cocoa(let cocoaStorage):
      let anyObjectKey: AnyObject = _bridgeToObjectiveCUnconditional(key)
      if let anyObjectValue: AnyObject = cocoaStorage.maybeGet(anyObjectKey) {
        return _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
      }
      return .None
    }
  }

  mutating func nativeUpdateValue(
      value: Value, forKey key: Key
  ) -> Value? {
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
    let oldValue: Value? = found ? nativeStorage[i.offset]!.value : .None
    nativeStorage[i.offset] = _NativeStorageElement(key: key, value: value)

    if !found {
      ++nativeStorage.count
    }
    return oldValue
  }

  mutating func updateValue(
    value: Value, forKey key: Key
  ) -> Value? {

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

  /// :param: idealBucket The ideal bucket for the element being deleted.
  /// :param: offset The offset of the element that will be deleted.
  mutating func nativeDeleteImpl(
      nativeStorage: NativeStorage, idealBucket: Int, offset: Int
  ) {
    // remove the element
    nativeStorage[offset] = .None
    --nativeStorage.count

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = offset

    // Find the first bucket in the contigous chain
    var start = idealBucket
    while nativeStorage[nativeStorage._prev(start)] != nil {
      start = nativeStorage._prev(start)
    }

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    for var b = nativeStorage._next(lastInChain); nativeStorage[b] != nil;
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

  mutating func nativeRemoveObjectForKey(key: Key) -> Value? {
    var nativeStorage = native
    var idealBucket = nativeStorage._bucket(key)
    var (index, found) = nativeStorage._find(key, idealBucket)

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
      idealBucket = nativeStorage._bucket(key)
      (index, found) = nativeStorage._find(key, idealBucket)
      _sanityCheck(found, "key was lost during storage migration")
    }

    let oldValue = nativeStorage[index.offset]!.value
    nativeDeleteImpl(nativeStorage, idealBucket: idealBucket,
        offset: index.offset)
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
    nativeDeleteImpl(nativeStorage, idealBucket: nativeStorage._bucket(key),
        offset: nativeIndex.offset)
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
          cocoaIndex.allKeys[cocoaIndex.currentKeyIndex]
      migrateDataToNativeStorage(cocoaStorage)
      nativeRemoveObjectForKey(
          _forceBridgeFromObjectiveC(anyObjectKey, Key.self))
    }
  }

  mutating func removeValueForKey(key: Key) -> Value? {
    if _fastPath(guaranteedNative) {
      return nativeRemoveObjectForKey(key)
    }

    switch self {
    case .Native:
      return nativeRemoveObjectForKey(key)
    case .Cocoa(let cocoaStorage):
      let anyObjectKey: AnyObject = _bridgeToObjectiveCUnconditional(key)
      if cocoaStorage.maybeGet(anyObjectKey) == nil {
        return .None
      }
      migrateDataToNativeStorage(cocoaStorage)
      return nativeRemoveObjectForKey(key)
    }
  }

  mutating func nativeRemoveAll() {
    var nativeStorage = native

    // We have already checked for the empty dictionary case, so we will always
    // mutating the dictionary storage.  Request unique storage.
    let (reallocated, capacityChanged) =
        ensureUniqueNativeStorage(nativeStorage.capacity)
    if reallocated {
      nativeStorage = native
    }

    for var b = 0; b != nativeStorage.capacity; ++b {
      nativeStorage[b] = .None
    }
    nativeStorage.count = 0
  }

  mutating func removeAll(#keepCapacity: Bool) {
    if count == 0 {
      return
    }

    if !keepCapacity {
      self = .Native(NativeStorage.Owner(minimumCapacity: 2))
      return
    }

    if _fastPath(guaranteedNative) {
      nativeRemoveAll()
      return
    }

    switch self {
    case .Native:
      nativeRemoveAll()
    case .Cocoa(let cocoaStorage):
      self = .Native(NativeStorage.Owner(minimumCapacity: cocoaStorage.count))
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

  /// Return a *generator* over the (key, value) pairs.
  ///
  /// Complexity: O(1)
  func generate() -> DictionaryGenerator<Key, Value> {
    switch self {
    case .Native:
      return ._Native(start: native.startIndex, end: native.endIndex)
    case .Cocoa(let cocoaStorage):
      return ._Cocoa(_CocoaDictionaryGenerator(cocoaStorage.cocoaDictionary))
    }
  }

  static func fromArray(
      elements: Array<(Key, Value)>
  ) -> _VariantDictionaryStorage<Key, Value> {
    _sanityCheckFailure("this function should never be called")
  }
}

struct _NativeDictionaryIndex<Key : Hashable, Value> :
  BidirectionalIndexType, Comparable {

  typealias NativeStorage = _NativeDictionaryStorage<Key, Value>
  typealias NativeIndex = _NativeDictionaryIndex<Key, Value>

  var nativeStorage: NativeStorage
  var offset: Int

  init(nativeStorage: NativeStorage, offset: Int) {
    self.nativeStorage = nativeStorage
    self.offset = offset
  }

  func predecessor() -> NativeIndex {
    var j = offset
    while --j > 0 {
      if nativeStorage[j] != nil {
        return NativeIndex(nativeStorage: nativeStorage, offset: j)
      }
    }
    return self
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// Requires: the next value is representable.
  func successor() -> NativeIndex {
    var i = offset + 1
    // FIXME: Can't write the simple code pending
    // <rdar://problem/15484639> Refcounting bug
    while i < nativeStorage.capacity /*&& !nativeStorage[i]*/ {
      // FIXME: workaround for <rdar://problem/15484639>
      if nativeStorage[i] != nil {
        break
      }
      // end workaround
      ++i
    }
    return NativeIndex(nativeStorage: nativeStorage, offset: i)
  }
}

func == <Key : Hashable, Value> (
  lhs: _NativeDictionaryIndex<Key, Value>,
  rhs: _NativeDictionaryIndex<Key, Value>
) -> Bool {
  // FIXME: assert that lhs and rhs are from the same dictionary.
  return lhs.offset == rhs.offset
}

func < <Key : Hashable, Value> (
  lhs: _NativeDictionaryIndex<Key, Value>,
  rhs: _NativeDictionaryIndex<Key, Value>
) -> Bool {
  // FIXME: assert that lhs and rhs are from the same dictionary.
  return lhs.offset < rhs.offset
}

struct _CocoaDictionaryIndex : BidirectionalIndexType, Comparable {
  // Assumption: we rely on NSDictionary.getObjects:andKeys: when being
  // repeatedly called on the same NSDictionary, returning keys in the same
  // order every time.

  /// A reference to the NSDictionary, which owns keys in `allKeys`.
  let cocoaDictionary: _SwiftNSDictionaryType

  /// An unowned array of keys.
  var allKeys: HeapBuffer<Int, AnyObject>

  /// Index into `allKeys`.
  var currentKeyIndex: Int

  init(_ cocoaDictionary: _SwiftNSDictionaryType, startIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    self.currentKeyIndex = 0
  }

  init(_ cocoaDictionary: _SwiftNSDictionaryType, endIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    self.currentKeyIndex = allKeys.value
  }

  init(_ cocoaDictionary: _SwiftNSDictionaryType,
       _ allKeys: HeapBuffer<Int, AnyObject>,
       _ currentKeyIndex: Int) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = allKeys
    self.currentKeyIndex = currentKeyIndex
  }

  func predecessor() -> _CocoaDictionaryIndex {
    _precondition(currentKeyIndex >= 1, "can not decrement startIndex")
    return _CocoaDictionaryIndex(cocoaDictionary, allKeys, currentKeyIndex - 1)
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// Requires: the next value is representable.
  func successor() -> _CocoaDictionaryIndex {
    _precondition(
        currentKeyIndex < allKeys.value, "can not increment endIndex")
    return _CocoaDictionaryIndex(cocoaDictionary, allKeys, currentKeyIndex + 1)
  }
}

func ==(lhs: _CocoaDictionaryIndex, rhs: _CocoaDictionaryIndex) -> Bool {
  _precondition(lhs.cocoaDictionary === rhs.cocoaDictionary,
      "can not compare indexes pointing to different dictionaries")
  _precondition(lhs.allKeys.value == rhs.allKeys.value,
      "one or both of the indexes have been invalidated")

  return lhs.currentKeyIndex == rhs.currentKeyIndex
}

func <(lhs: _CocoaDictionaryIndex, rhs: _CocoaDictionaryIndex) -> Bool {
  _precondition(lhs.cocoaDictionary === rhs.cocoaDictionary,
      "can not compare indexes pointing to different dictionaries")
  _precondition(lhs.allKeys.value == rhs.allKeys.value,
      "one or both of the indexes have been invalidated")

  return lhs.currentKeyIndex < rhs.currentKeyIndex
}

enum _DictionaryIndexRepresentation<Key : Hashable, Value> {
  typealias _Index = DictionaryIndex<Key, Value>
  typealias _NativeIndex = _Index._NativeIndex
  typealias _CocoaIndex = _Index._CocoaIndex

  case _Native(_NativeIndex)
  case _Cocoa(_CocoaIndex)
}

public struct DictionaryIndex<Key : Hashable, Value> :
  BidirectionalIndexType, Comparable {
  // Index for native storage is efficient.  Index for bridged NSDictionary is
  // not, because neither NSEnumerator nor fast enumeration support moving
  // backwards.  Even if they did, there is another issue: NSEnumerator does
  // not support NSCopying, and fast enumeration does not document that it is
  // safe to copy the state.  So, we can not implement Index that is a value
  // type for bridged NSDictionary in terms of Cocoa enumeration facilities.

  typealias _NativeIndex = _NativeDictionaryIndex<Key, Value>
  typealias _CocoaIndex = _CocoaDictionaryIndex

  var _value: _DictionaryIndexRepresentation<Key, Value>

  static func _Native(index: _NativeIndex) -> DictionaryIndex {
    return DictionaryIndex(_value: ._Native(index))
  }
  static func _Cocoa(index: _CocoaIndex) -> DictionaryIndex {
    return DictionaryIndex(_value: ._Cocoa(index))
  }

  @transparent
  var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 && _canBeClass(Value.self) == 0
  }

  @transparent
  var _nativeIndex: _NativeIndex {
    switch _value {
    case ._Native(let nativeIndex):
      return nativeIndex
    case ._Cocoa:
      _sanityCheckFailure("internal error: does not contain a native index")
    }
  }

  @transparent
  var _cocoaIndex: _CocoaIndex {
    switch _value {
    case ._Native:
      _sanityCheckFailure("internal error: does not contain a Cocoa index")
    case ._Cocoa(let cocoaIndex):
      return cocoaIndex
    }
  }

  public typealias Index = DictionaryIndex<Key, Value>

  /// Returns the previous consecutive value before `self`.
  ///
  /// Requires: the previous value is representable.
  public func predecessor() -> Index {
    if _fastPath(_guaranteedNative) {
      return ._Native(_nativeIndex.predecessor())
    }

    switch _value {
    case ._Native(let nativeIndex):
      return ._Native(nativeIndex.predecessor())
    case ._Cocoa(let cocoaIndex):
      return ._Cocoa(cocoaIndex.predecessor())
    }
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// Requires: the next value is representable.
  public func successor() -> Index {
    if _fastPath(_guaranteedNative) {
      return ._Native(_nativeIndex.successor())
    }

    switch _value {
    case ._Native(let nativeIndex):
      return ._Native(nativeIndex.successor())
    case ._Cocoa(let cocoaIndex):
      return ._Cocoa(cocoaIndex.successor())
    }
  }
}

public func == <Key : Hashable, Value> (
  lhs: DictionaryIndex<Key, Value>,
  rhs: DictionaryIndex<Key, Value>
) -> Bool {
  if _fastPath(lhs._guaranteedNative) {
    return lhs._nativeIndex == rhs._nativeIndex
  }

  switch (lhs._value, rhs._value) {
  case (._Native(let lhsNative), ._Native(let rhsNative)):
    return lhsNative == rhsNative
  case (._Cocoa(let lhsCocoa), ._Cocoa(let rhsCocoa)):
    return lhsCocoa == rhsCocoa
  default:
    _preconditionFailure("comparing indexes from different dictionaries")
  }
}

public func < <Key : Hashable, Value> (
  lhs: DictionaryIndex<Key, Value>,
  rhs: DictionaryIndex<Key, Value>
) -> Bool {
  if _fastPath(lhs._guaranteedNative) {
    return lhs._nativeIndex < rhs._nativeIndex
  }

  switch (lhs._value, rhs._value) {
  case (._Native(let lhsNative), ._Native(let rhsNative)):
    return lhsNative < rhsNative
  case (._Cocoa(let lhsCocoa), ._Cocoa(let rhsCocoa)):
    return lhsCocoa < rhsCocoa
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
    item0 = UnsafeMutablePointer<RawByte>.null().value
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

final
class _CocoaDictionaryGenerator : GeneratorType {
  // Cocoa dictionary generator has to be a class, otherwise we can not
  // guarantee that the fast enumeration struct is pinned to a certain memory
  // location.

  let cocoaDictionary: _SwiftNSDictionaryType
  var fastEnumerationState = _makeSwiftNSFastEnumerationState()
  var fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

  // These members have to be full-sized integers, they can not be limited to
  // Int8 just because our buffer holds 16 elements: fast enumeration is
  // allowed to return inner pointers to the container, which can be much
  // larger.
  var itemIndex: Int = 0
  var itemCount: Int = 0

  init(_ cocoaDictionary: _SwiftNSDictionaryType) {
    self.cocoaDictionary = cocoaDictionary
  }

  func next() -> (AnyObject, AnyObject)? {
    if itemIndex < 0 {
      return .None
    }
    let cocoaDictionary = self.cocoaDictionary
    if itemIndex == itemCount {
      let stackBufLength = fastEnumerationStackBuf.length
      itemCount = withUnsafeMutablePointers(
          &fastEnumerationState, &fastEnumerationStackBuf) {
        (statePtr, bufPtr) -> Int in
        cocoaDictionary.countByEnumeratingWithState(
          statePtr, objects: UnsafeMutablePointer(bufPtr),
          count: stackBufLength)
      }
      if itemCount == 0 {
        itemIndex = -1
        return .None
      }
      itemIndex = 0
    }
    let itemsPtrUP: UnsafeMutablePointer<AnyObject> =
        UnsafeMutablePointer(fastEnumerationState.itemsPtr)
    let itemsPtr = _UnmanagedAnyObjectArray(itemsPtrUP)
    let key: AnyObject = itemsPtr[itemIndex]
    ++itemIndex
    let value: AnyObject = cocoaDictionary.objectForKey(key)!
    return (key, value)
  }
}

enum _DictionaryGeneratorRepresentation<Key : Hashable, Value> {
  typealias _Generator = DictionaryGenerator<Key, Value>
  typealias _NativeIndex = _Generator._NativeIndex
  case _Native(start: _NativeIndex, end: _NativeIndex)
  case _Cocoa(_CocoaDictionaryGenerator)
}

public struct DictionaryGenerator<Key : Hashable, Value> : GeneratorType {
  // Dictionary has a separate GeneratorType and Index because of efficiency
  // and implementability reasons.
  //
  // Index for native storage is efficient.  Index for bridged NSDictionary is
  // not.
  //
  // Even though fast enumeration is not suitable for implementing
  // Index, which is multi-pass, it is suitable for implementing a
  // GeneratorType, which is being consumed as iteration proceeds.

  typealias _NativeIndex = _NativeDictionaryIndex<Key, Value>

  var _state: _DictionaryGeneratorRepresentation<Key, Value>

  static func _Native(
    #start: _NativeIndex, end: _NativeIndex
  ) -> DictionaryGenerator {
    return DictionaryGenerator(_state: ._Native(start: start, end: end))
  }
  static func _Cocoa(
    generator: _CocoaDictionaryGenerator
  ) -> DictionaryGenerator{
    return DictionaryGenerator(_state: ._Cocoa(generator))
  }

  @transparent
  var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 && _canBeClass(Value.self) == 0
  }

  mutating func _nativeNext() -> (Key, Value)? {
    switch _state {
    case ._Native(var startIndex, var endIndex):
      if startIndex == endIndex {
        return .None
      }
      let result = startIndex.nativeStorage.assertingGet(startIndex)
      _state = ._Native(start: startIndex.successor(), end: endIndex)
      return result
    case ._Cocoa:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
    }
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: no preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> (Key, Value)? {
    if _fastPath(_guaranteedNative) {
      return _nativeNext()
    }

    switch _state {
    case ._Native(var startIndex, var endIndex):
      return _nativeNext()
    case ._Cocoa(var cocoaGenerator):
      if let (anyObjectKey: AnyObject, anyObjectValue: AnyObject) =
          cocoaGenerator.next() {
        let nativeKey = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
        let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
        return (nativeKey, nativeValue)
      }
      return .None
    }
  }
}

public struct Dictionary<
  Key : Hashable, Value
> : CollectionType, DictionaryLiteralConvertible {

  typealias _Self = Dictionary<Key, Value>
  typealias _VariantStorage = _VariantDictionaryStorage<Key, Value>
  typealias _NativeStorage = _NativeDictionaryStorage<Key, Value>
  public typealias Element = (Key, Value)
  public typealias Index = DictionaryIndex<Key, Value>

  var _variantStorage: _VariantStorage

  /// Create a dictionary with at least the given number of
  /// elements worth of storage.  The actual capacity will be the
  /// smallest power of 2 that's >= `minimumCapacity`.
  public init() {
    _variantStorage =
        .Native(_NativeStorage.Owner(minimumCapacity: 2))
  }

  /// Create a dictionary with at least the given number of
  /// elements worth of storage.  The actual capacity will be the
  /// smallest power of 2 that's >= `minimumCapacity`.
  public init(minimumCapacity: Int) {
    _variantStorage =
        .Native(_NativeStorage.Owner(minimumCapacity: minimumCapacity))
  }

  /// Private initializer.
  init(_nativeStorage: _NativeDictionaryStorage<Key, Value>) {
    _variantStorage =
        .Native(_NativeStorage.Owner(nativeStorage: _nativeStorage))
  }

  /// Private initializer.
  init(
      _nativeStorageOwner: _NativeDictionaryStorageOwner<Key, Value>) {
    _variantStorage = .Native(_nativeStorageOwner)
  }

  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  /// * it is statically known that the given `NSDictionary` is immutable;
  /// * `Key` and `Value` are bridged verbatim to Objective-C (i.e.,
  ///   are reference types).
  public init(_immutableCocoaDictionary: _SwiftNSDictionaryType) {
    _sanityCheck(
        _isBridgedVerbatimToObjectiveC(Key.self) &&
        _isBridgedVerbatimToObjectiveC(Value.self),
        "Dictionary be backed by NSDictionary storage only when both key and value are bridged verbatim to Objective-C")
    _variantStorage = .Cocoa(
        _CocoaDictionaryStorage(cocoaDictionary: _immutableCocoaDictionary))
  }

  //
  // All APIs below should dispatch to `_variantStorage`, without doing any
  // additional processing.
  //

  /// The position of the first element in a non-empty dictionary.
  ///
  /// Identical to `endIndex` in an empty dictionary
  ///
  /// Complexity: amortized O(1) if `self` does not wrap a bridged
  /// `NSDictionary`, O(N) otherwise.
  public var startIndex: Index {
    return _variantStorage.startIndex
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// Complexity: amortized O(1) if `self` does not wrap a bridged
  /// `NSDictionary`, O(N) otherwise.
  public var endIndex: Index {
    return _variantStorage.endIndex
  }

  /// Returns the `Index` for the given key, or `nil` if the key is not
  /// present in the dictionary.
  public func indexForKey(key: Key) -> Index? {
    // Complexity: amortized O(1) for native storage, O(N) when wrapping an
    // NSDictionary.
    return _variantStorage.indexForKey(key)
  }

  /// Access the key-value pair at `position`.
  ///
  /// Complexity: O(1)
  public subscript(position: Index) -> Element {
    return _variantStorage.assertingGet(position)
  }

  public subscript(key: Key) -> Value? {
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
  public mutating func updateValue(
    value: Value, forKey key: Key
  ) -> Value? {
    return _variantStorage.updateValue(value, forKey: key)
  }

  /// Remove the key-value pair referenced by the given index.
  public mutating func removeAtIndex(index: Index) {
    _variantStorage.removeAtIndex(index)
  }

  /// Remove a given key and the associated value from the dictionary.
  /// Returns the value that was removed, or `nil` if the key was not present
  /// in the dictionary.
  public mutating func removeValueForKey(key: Key) -> Value? {
    return _variantStorage.removeValueForKey(key)
  }

  /// Erase all the elements.  If `keepCapacity` is `true`, `capacity`
  /// will not decrease.
  public mutating func removeAll(keepCapacity: Bool = false) {
    // The 'will not decrease' part in the documentation comment is worded very
    // carefully.  The capacity can increase if we replace Cocoa storage with
    // native storage.
    _variantStorage.removeAll(keepCapacity: keepCapacity)
  }

  /// The number of entries in the dictionary.
  ///
  /// Complexity: O(1)
  public var count: Int {
    return _variantStorage.count
  }

  //
  // `SequenceType` conformance
  //

  /// Return a *generator* over the (key, value) pairs.
  ///
  /// Complexity: O(1)
  public func generate() -> DictionaryGenerator<Key, Value> {
    return _variantStorage.generate()
  }

  //
  // DictionaryLiteralConvertible conformance
  //
  /// Create an instance initialized with `elements`.
  @effects(readnone)
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(_nativeStorage: _NativeDictionaryStorage.fromArray(elements))
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

  public var isEmpty: Bool {
    return count == 0
  }

  public var keys: LazyBidirectionalCollection<
    MapCollectionView<Dictionary, Key>
  > {
    return lazy(self).map { $0.0 }
  }

  public var values: LazyBidirectionalCollection<
    MapCollectionView<Dictionary, Value>
  > {
    return lazy(self).map { $0.1 }
  }
}

public func == <Key : Equatable, Value : Equatable>(
  lhs: [Key : Value],
  rhs: [Key : Value]
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
    return _stdlib_NSObject_isEqual(
        lhsCocoa.cocoaDictionary, rhsCocoa.cocoaDictionary)

  case (.Native(let lhsNativeOwner), .Cocoa(let rhsCocoa)):
    let lhsNative = lhsNativeOwner.nativeStorage

    if lhsNative.count != rhsCocoa.count {
      return false
    }

    let endIndex = lhsNative.endIndex
    for var index = lhsNative.startIndex; index != endIndex; ++index {
      let (key, value) = lhsNative.assertingGet(index)
      let optRhsValue: AnyObject? =
          rhsCocoa.maybeGet(_bridgeToObjectiveCUnconditional(key))
      if let rhsValue: AnyObject = optRhsValue {
        if value == _forceBridgeFromObjectiveC(rhsValue, Value.self) {
          continue
        }
      }
      return false
    }
    return true

  case (.Cocoa, .Native):
    return rhs == lhs
  }
}

public func != <Key : Equatable, Value : Equatable>(
  lhs: [Key : Value],
  rhs: [Key : Value]
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

  public var description: String {
    return _makeDescription(isDebug: false)
  }

  public var debugDescription: String {
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

  mutating func successor() {
    _intPos = _intPos + 1
    _dicPos = _dicPos.successor()
  }

  mutating func prec() {
    _intPos = _intPos - 1
    _dicPos = _dicPos.predecessor()
  }
}

func ==<K : Hashable,V> (
  lhs : _DictionaryMirrorPosition<K,V>, rhs : Int
) -> Bool {
  return lhs._intPos == rhs
}

func > <K : Hashable,V> (
  lhs : _DictionaryMirrorPosition<K,V>, rhs : Int
) -> Bool {
  return lhs._intPos > rhs
}

func < <K : Hashable,V> (
  lhs : _DictionaryMirrorPosition<K,V>, rhs : Int
) -> Bool {
  return lhs._intPos < rhs
}

//===--- Mirroring---------------------------------------------------------===//
class _DictionaryMirror<Key : Hashable,Value> : MirrorType {
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

  subscript(i: Int) -> (String, MirrorType) {
    // this use of indexes is optimized for a world of contiguous accesses
    // i.e. we expect users to start asking for children in a range, then maybe
    // shift to a different range, .. and so on
    _precondition(i >= 0 && i < count, "MirrorType access out of bounds")
    while _pos < i {
      _pos.successor()
    }
    while _pos > i {
      _pos.prec()
    }
    return ("[\(_pos._intPos)]", reflect(_dict[_pos._dicPos]))
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
  public func getMirror() -> MirrorType {
    return _DictionaryMirror(self)
  }
}

/// Initializes a `Dictionary` from unique key-value pairs.
///
/// Using a builder can be faster than inserting key-value pairs into an empty
/// `Dictionary`.
public struct _DictionaryBuilder<Key : Hashable, Value> {
  var _result: [Key : Value]
  var _nativeStorage: _NativeDictionaryStorage<Key, Value>
  let _requestedCount: Int
  var _actualCount: Int

  public init(count: Int) {
    let requiredCapacity =
        _NativeDictionaryStorage<Key, Value>.getMinCapacity(
            count, _dictionaryDefaultMaxLoadFactorInverse)
    _result = [Key : Value](minimumCapacity: requiredCapacity)
    _nativeStorage = _result._variantStorage.native
    _requestedCount = count
    _actualCount = 0
  }

  public mutating func add(#key: Key, value: Value) {
    _nativeStorage.unsafeAddNew(key: key, value: value)
    _actualCount++
  }

  public mutating func take() -> [Key : Value] {
    _precondition(_actualCount >= 0,
        "can not take the result twice")
    _precondition(_actualCount == _requestedCount,
        "the number of key-value pairs added does not match the promised count")

    // Finish building the `Dictionary`.
    _nativeStorage.count = _requestedCount

    // Prevent taking the result twice.
    _actualCount = -1
    return _result
  }
}

//===--- Mocks of Cocoa types that we use ---------------------------------===//

import SwiftShims

@objc
public protocol _SwiftNSFastEnumerationType {
  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int
}

@objc
public protocol _SwiftNSEnumeratorType {
  init()
  func nextObject() -> AnyObject?
}

public typealias _SwiftNSZone = COpaquePointer

@objc
public protocol _SwiftNSCopyingType {
  func copyWithZone(zone: _SwiftNSZone) -> AnyObject
}

@unsafe_no_objc_tagged_pointer @objc
public protocol _SwiftNSArrayRequiredOverridesType :
    _SwiftNSCopyingType, _SwiftNSFastEnumerationType {

  func objectAtIndex(index: Int) -> AnyObject

  func getObjects(UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange)

  func countByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  var count: Int { get }
}

@unsafe_no_objc_tagged_pointer @objc
public protocol _SwiftNSArrayType : _SwiftNSArrayRequiredOverridesType {
  func indexOfObject(anObject: AnyObject) -> Int
}

@objc
public protocol _SwiftNSDictionaryRequiredOverridesType :
    _SwiftNSCopyingType, _SwiftNSFastEnumerationType {

  // The following methods should be overridden when implementing an
  // NSDictionary subclass.

  // The designated initializer of `NSDictionary`.
  init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafePointer<Void>, count: Int)

  var count: Int { get }
  func objectForKey(aKey: AnyObject?) -> AnyObject?
  func keyEnumerator() -> _SwiftNSEnumeratorType?

  // We also override the following methods for efficiency.

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int
}

@unsafe_no_objc_tagged_pointer @objc
public protocol _SwiftNSDictionaryType :
    _SwiftNSDictionaryRequiredOverridesType {
  func getObjects(objects: UnsafeMutablePointer<AnyObject>,
      andKeys keys: UnsafeMutablePointer<AnyObject>)
}

/// Call `[lhs isEqual: rhs]`.
///
/// This function is part of the runtime because `Bool` type is bridged to
/// `ObjCBool`, which is in Foundation overlay.
@asmname("swift_stdlib_NSObject_isEqual")
func _stdlib_NSObject_isEqual(lhs: AnyObject, rhs: AnyObject) -> Bool

/// Equivalent to `NSDictionary.allKeys`, but does not leave objects on the
/// autorelease pool.
func _stdlib_NSDictionary_allKeys(nsd: _SwiftNSDictionaryType)
    -> HeapBuffer<Int, AnyObject> {
  let count = nsd.count
  var buffer = HeapBuffer<Int, AnyObject>(
      HeapBufferStorage<Int, AnyObject>.self, count, count)
  nsd.getObjects(nil, andKeys: buffer.baseAddress)
  return buffer
}

//===--- Bridging ---------------------------------------------------------===//

extension Dictionary {
  public func _bridgeToObjectiveCImpl()
      -> _SwiftNSDictionaryRequiredOverridesType {
    switch _variantStorage {
    case .Native(let nativeOwner):
      _precondition(_isBridgedToObjectiveC(Key.self),
          "Key is not bridged to Objective-C")
      _precondition(_isBridgedToObjectiveC(Value.self),
          "Value is not bridged to Objective-C")

      // The `Dictionary` is backed by native storage, which is also a proper
      // `NSDictionary` subclass, that, if needed, performs bridging lazily.
      return nativeOwner as _NativeDictionaryStorageOwnerBase

    case .Cocoa(let cocoaStorage):
      // The `Dictionary` is already backed by `NSDictionary` of some kind.  Just
      // unwrap it.
      return cocoaStorage.cocoaDictionary
    }
  }

  public static func _bridgeFromObjectiveCAdoptingNativeStorage(
      d: AnyObject
  ) -> [Key : Value]? {
    if let nativeOwner =
        d as AnyObject as? _NativeDictionaryStorageOwner<Key, Value> {
      // If `NSDictionary` is actually native storage of `Dictionary` with key
      // and value types that the requested ones match exactly, then just
      // re-wrap the native storage.
      return [Key : Value](_nativeStorageOwner: nativeOwner)
    }
    // FIXME: what if `d` is native storage, but for different key/value type?
    return .None
  }
}

//===--- Compiler conversion/casting entry points -------------------------===//

/// Perform a non-bridged upcast that always succeeds.
///
/// Requires: `BaseKey` and `BaseValue` are base classes or base @objc
/// protocols (such as `AnyObject`) of `DerivedKey` and `DerivedValue`,
/// respectively.
public func _dictionaryUpCast<DerivedKey, DerivedValue, BaseKey, BaseValue>(
    source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  // FIXME: This crappy implementation is O(n) because it copies the
  // data; a proper implementation would be O(1).

  _sanityCheck(_isClassOrObjCExistential(BaseKey.self))
  _sanityCheck(_isClassOrObjCExistential(BaseValue.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedKey.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedValue.self))

  var result = Dictionary<BaseKey, BaseValue>(minimumCapacity: source.count)
  for (k, v) in source {
    result[unsafeBitCast(k, BaseKey.self)] = unsafeBitCast(v, BaseValue.self)
  }
  return result
}

/// Implements an unconditional upcast that involves bridging.
///
/// The cast can fail if bridging fails.
///
/// Precondition: `SwiftKey` and `SwiftValue` are bridged to Objective-C,
/// and at least one of them requires non-trivial bridging.
public func _dictionaryBridgeToObjectiveC<
    SwiftKey, SwiftValue, ObjCKey, ObjCValue
>(
    source: Dictionary<SwiftKey, SwiftValue>
) -> Dictionary<ObjCKey, ObjCValue> {
  _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(SwiftKey.self) ||
      !_isBridgedVerbatimToObjectiveC(SwiftValue.self))
  _sanityCheck(
      _isClassOrObjCExistential(ObjCKey.self) ||
      _isClassOrObjCExistential(ObjCValue.self))

  var result = Dictionary<ObjCKey, ObjCValue>(minimumCapacity: source.count)
  let keyBridgesDirectly =
      _isBridgedVerbatimToObjectiveC(SwiftKey.self) ==
          _isBridgedVerbatimToObjectiveC(ObjCKey.self)
  let valueBridgesDirectly =
      _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
          _isBridgedVerbatimToObjectiveC(ObjCValue.self)
  for (key, value) in source {
    // Bridge the key
    var bridgedKey: ObjCKey
    if keyBridgesDirectly {
      bridgedKey = unsafeBitCast(key, ObjCKey.self)
    } else {
      let bridged: AnyObject? = _bridgeToObjectiveC(key)
      _precondition(bridged != nil, "dictionary key cannot be bridged to Objective-C")
      bridgedKey = unsafeBitCast(bridged!, ObjCKey.self)
    }

    // Bridge the value
    var bridgedValue: ObjCValue
    if valueBridgesDirectly {
      bridgedValue = unsafeBitCast(value, ObjCValue.self)
    } else {
      let bridged: AnyObject? = _bridgeToObjectiveC(value)
      _precondition(bridged != nil,
          "dictionary value cannot be bridged to Objective-C")
      bridgedValue = unsafeBitCast(bridged!, ObjCValue.self)
    }

    result[bridgedKey] = bridgedValue
  }

  return result
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
/// a subtype of `BaseValue`, and all of these types are reference types.
public func _dictionaryDownCast<BaseKey, BaseValue, DerivedKey, DerivedValue>(
    source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue> {
  _sanityCheck(_isClassOrObjCExistential(BaseKey.self))
  _sanityCheck(_isClassOrObjCExistential(BaseValue.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedKey.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedValue.self))

  switch source._variantStorage {
  case .Native(let nativeOwner):
    // FIXME(performance): this introduces an indirection through Objective-C
    // runtime, even though we access native storage.  But we can not
    // unsafeBitCast the owner object, because that would change the generic
    // arguments.
    //
    // One way to solve this is to add a third, read-only, representation to
    // variant storage: like _NativeDictionaryStorageOwner, but it would
    // perform casts when accessing elements.
    //
    // Note: it is safe to treat the storage as immutable here because
    // Dictionary will not mutate storage with reference count greater than 1.
    return Dictionary(
      _immutableCocoaDictionary:
        unsafeBitCast(nativeOwner, _SwiftNSDictionaryType.self))

  case .Cocoa(let cocoaStorage):
    return Dictionary(
      _immutableCocoaDictionary:
        unsafeBitCast(cocoaStorage, _SwiftNSDictionaryType.self))
  }
}

/// Implements a conditional downcast.
///
/// If the cast fails, the function returns `.None`.  All checks should be
/// performed eagerly.
///
/// Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
/// a subtype of `BaseValue`, and all of these types are reference types.
public func _dictionaryDownCastConditional<
    BaseKey, BaseValue, DerivedKey, DerivedValue
>(
    source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue>? {
  _sanityCheck(_isClassOrObjCExistential(BaseKey.self))
  _sanityCheck(_isClassOrObjCExistential(BaseValue.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedKey.self))
  _sanityCheck(_isClassOrObjCExistential(DerivedValue.self))

  var result = Dictionary<DerivedKey, DerivedValue>()
  for (key, value) in source {
    if let derivedKey = key as? DerivedKey {
      if let derivedValue = value as? DerivedValue {
        result[derivedKey] = derivedValue
        continue
      }
    }

    // Either the key or the value wasn't of the appropriate derived
    // type. Fail.
    return nil
  }
  return result
}

/// Implements an unconditional downcast that involves bridging.
///
/// Precondition: at least one of `SwiftKey` or `SwiftValue` is a bridged value
/// type, and the corresponding `ObjCKey` or `ObjCValue` is a reference type.
public func _dictionaryBridgeFromObjectiveC<
    ObjCKey, ObjCValue, SwiftKey, SwiftValue
>(
    source: Dictionary<ObjCKey, ObjCValue>
) -> Dictionary<SwiftKey, SwiftValue> {
  let result: Dictionary<SwiftKey, SwiftValue>? =
      _dictionaryBridgeFromObjectiveCConditional(source)
  _precondition(result != nil, "dictionary cannot be bridged from Objective-C")
  return result!
}

/// Implements a conditional downcast that involves bridging.
///
/// If the cast fails, the function returns `.None`.  All checks should be
/// performed eagerly.
///
/// Precondition: at least one of `SwiftKey` or `SwiftValue` is a bridged value
/// type, and the corresponding `ObjCKey` or `ObjCValue` is a reference type.
public func _dictionaryBridgeFromObjectiveCConditional<
    ObjCKey, ObjCValue, SwiftKey, SwiftValue
>(
    source: Dictionary<ObjCKey, ObjCValue>
) -> Dictionary<SwiftKey, SwiftValue>? {
  _sanityCheck(
      _isClassOrObjCExistential(ObjCKey.self) ||
      _isClassOrObjCExistential(ObjCValue.self))
  _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(SwiftKey.self) ||
      !_isBridgedVerbatimToObjectiveC(SwiftValue.self))

  let keyBridgesDirectly =
      _isBridgedVerbatimToObjectiveC(SwiftKey.self) ==
          _isBridgedVerbatimToObjectiveC(ObjCKey.self)
  let valueBridgesDirectly =
      _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
          _isBridgedVerbatimToObjectiveC(ObjCValue.self)

  var result = Dictionary<SwiftKey, SwiftValue>()
  for (key, value) in source {
    // Downcast the key.
    var resultKey: SwiftKey
    if keyBridgesDirectly {
      if let bridgedKey = key as? SwiftKey {
        resultKey = bridgedKey
      } else {
        return nil
      }
    } else {
      if let bridgedKey = _conditionallyBridgeFromObjectiveC(
          _reinterpretCastToAnyObject(key), SwiftKey.self) {
        resultKey = bridgedKey
      } else {
        return nil
      }
    }

    // Downcast the value.
    var resultValue: SwiftValue
    if valueBridgesDirectly {
      if let bridgedValue = value as? SwiftValue {
        resultValue = bridgedValue
      } else {
        return nil
      }
    } else {
      if let bridgedValue = _conditionallyBridgeFromObjectiveC(
          _reinterpretCastToAnyObject(value), SwiftValue.self) {
        resultValue = bridgedValue
      } else {
        return nil
      }
    }

    result[resultKey] = resultValue
  }
  return result
}


//===--- Hacks and workarounds --------------------------------------------===//

/// Like `UnsafeMutablePointer<Unmanaged<AnyObject>>`, or `id
/// __unsafe_unretained *` in Objective-C ARC.
struct _UnmanagedAnyObjectArray {
  // `UnsafeMutablePointer<Unmanaged<AnyObject>>` fails because of:
  // <rdar://problem/16836348> IRGen: Couldn't find conformance

  /// Underlying pointer, typed as an integer to escape from reference
  /// counting.
  var value: UnsafeMutablePointer<Word>

  init(_ up: UnsafeMutablePointer<AnyObject>) {
    self.value = UnsafeMutablePointer(up)
  }

  subscript(i: Int) -> AnyObject {
    get {
      return _reinterpretCastToAnyObject(value[i])
    }
    nonmutating set(newValue) {
      value[i] = unsafeBitCast(newValue, Word.self)
    }
  }
}

