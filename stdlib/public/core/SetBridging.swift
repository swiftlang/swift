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

#if _runtime(_ObjC)

import SwiftShims

/// Equivalent to `NSSet.allObjects`, but does not leave objects on the
/// autorelease pool.
internal func _stdlib_NSSet_allObjects(_ object: AnyObject) -> _BridgingBuffer {
  let nss = unsafeBitCast(object, to: _NSSet.self)
  let count = nss.count
  let storage = _BridgingBuffer(count)
  nss.getObjects(storage.baseAddress, count: count)
  return storage
}

extension _NativeSet { // Bridging
  @usableFromInline
  internal __consuming func bridged() -> AnyObject {
    _connectOrphanedFoundationSubclassesIfNeeded()
    
    // We can zero-cost bridge if our keys are verbatim
    // or if we're the empty singleton.

    // Temporary var for SOME type safety.
    let nsSet: _NSSetCore

    if _storage === __RawSetStorage.empty || count == 0 {
      nsSet = __RawSetStorage.empty
    } else if _isBridgedVerbatimToObjectiveC(Element.self) {
      nsSet = unsafeDowncast(_storage, to: _SetStorage<Element>.self)
    } else {
      nsSet = _SwiftDeferredNSSet(self)
    }

    // Cast from "minimal NSSet" to "NSSet"
    // Note that if you actually ask Swift for this cast, it will fail.
    // Never trust a shadow protocol!
    return nsSet
  }
}

/// An NSEnumerator that works with any _NativeSet of verbatim bridgeable
/// elements. Used by the various NSSet impls.
final internal class _SwiftSetNSEnumerator<Element: Hashable>
  : __SwiftNativeNSEnumerator, _NSEnumerator {

  @nonobjc internal var base: _NativeSet<Element>
  @nonobjc internal var bridgedElements: __BridgingHashBuffer?
  @nonobjc internal var nextBucket: _NativeSet<Element>.Bucket
  @nonobjc internal var endBucket: _NativeSet<Element>.Bucket

  @objc
  internal override required init() {
    _internalInvariantFailure("don't call this designated initializer")
  }

  internal init(_ base: __owned _NativeSet<Element>) {
    _internalInvariant(_isBridgedVerbatimToObjectiveC(Element.self))
    _internalInvariant(_orphanedFoundationSubclassesReparented)
    self.base = base
    self.bridgedElements = nil
    self.nextBucket = base.hashTable.startBucket
    self.endBucket = base.hashTable.endBucket
    super.init()
  }

  @nonobjc
  internal init(_ deferred: __owned _SwiftDeferredNSSet<Element>) {
    _internalInvariant(!_isBridgedVerbatimToObjectiveC(Element.self))
    _internalInvariant(_orphanedFoundationSubclassesReparented)
    self.base = deferred.native
    self.bridgedElements = deferred.bridgeElements()
    self.nextBucket = base.hashTable.startBucket
    self.endBucket = base.hashTable.endBucket
    super.init()
  }

  private func bridgedElement(at bucket: _HashTable.Bucket) -> AnyObject {
    _internalInvariant(base.hashTable.isOccupied(bucket))
    if let bridgedElements = self.bridgedElements {
      return bridgedElements[bucket]
    }
    return _bridgeAnythingToObjectiveC(base.uncheckedElement(at: bucket))
  }

  //
  // NSEnumerator implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  @objc
  internal func nextObject() -> AnyObject? {
    if nextBucket == endBucket {
      return nil
    }
    let bucket = nextBucket
    nextBucket = base.hashTable.occupiedBucket(after: nextBucket)
    return self.bridgedElement(at: bucket)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>,
    count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }

    if nextBucket == endBucket {
      state.pointee = theState
      return 0
    }

    // Return only a single element so that code can start iterating via fast
    // enumeration, terminate it, and continue via NSEnumerator.
    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = self.bridgedElement(at: nextBucket)
    nextBucket = base.hashTable.occupiedBucket(after: nextBucket)
    state.pointee = theState
    return 1
  }
}

/// This class exists for Objective-C bridging. It holds a reference to a
/// _NativeSet, and can be upcast to NSSelf when bridging is necessary.  This is
/// the fallback implementation for situations where toll-free bridging isn't
/// possible. On first access, a _NativeSet of AnyObject will be constructed
/// containing all the bridged elements.
final internal class _SwiftDeferredNSSet<Element: Hashable>
  : __SwiftNativeNSSet, _NSSetCore {

  // This stored property must be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  private var _bridgedElements_DoNotUse: AnyObject?

  /// The unbridged elements.
  internal var native: _NativeSet<Element>

  internal init(_ native: __owned _NativeSet<Element>) {
    _internalInvariant(native.count > 0)
    _internalInvariant(!_isBridgedVerbatimToObjectiveC(Element.self))
    self.native = native
    super.init()
  }

  /// Returns the pointer to the stored property, which contains bridged
  /// Set elements.
  @nonobjc
  private var _bridgedElementsPtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self)
      .assumingMemoryBound(to: Optional<AnyObject>.self)
  }

  /// The buffer for bridged Set elements, if present.
  @nonobjc
  private var _bridgedElements: __BridgingHashBuffer? {
    guard let ref = _stdlib_atomicLoadARCRef(object: _bridgedElementsPtr) else {
      return nil
    }
    return unsafeDowncast(ref, to: __BridgingHashBuffer.self)
  }

  /// Attach a buffer for bridged Set elements.
  @nonobjc
  private func _initializeBridgedElements(_ storage: __BridgingHashBuffer) {
    _stdlib_atomicInitializeARCRef(
      object: _bridgedElementsPtr,
      desired: storage)
  }

  @nonobjc
  internal func bridgeElements() -> __BridgingHashBuffer {
    if let bridgedElements = _bridgedElements { return bridgedElements }

    // Allocate and initialize heap storage for bridged objects.
    let bridged = __BridgingHashBuffer.allocate(
      owner: native._storage,
      hashTable: native.hashTable)
    for bucket in native.hashTable {
      let object = _bridgeAnythingToObjectiveC(
        native.uncheckedElement(at: bucket))
      bridged.initialize(at: bucket, to: object)
    }

    // Atomically put the bridged elements in place.
    _initializeBridgedElements(bridged)
    return _bridgedElements!
  }

  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _internalInvariantFailure("don't call this designated initializer")
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSSet` type, which is immutable.
    return self
  }

  @objc(member:)
  internal func member(_ object: AnyObject) -> AnyObject? {
    guard let element = _conditionallyBridgeFromObjectiveC(object, Element.self)
    else { return nil }

    let (bucket, found) = native.find(element)
    guard found else { return nil }
    let bridged = bridgeElements()
    return bridged[bucket]
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    return _SwiftSetNSEnumerator<Element>(self)
  }

  @objc
  internal var count: Int {
    return native.count
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?,
    count: Int
  ) -> Int {
    defer { _fixLifetime(self) }
    let hashTable = native.hashTable

    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(hashTable.startBucket.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var bucket = _HashTable.Bucket(offset: Int(theState.extra.0))
    let endBucket = hashTable.endBucket
    _precondition(bucket == endBucket || hashTable.isOccupied(bucket),
      "Invalid fast enumeration state")

    // Only need to bridge once, so we can hoist it out of the loop.
    let bridgedElements = bridgeElements()

    var stored = 0
    for i in 0..<count {
      if bucket == endBucket { break }
      unmanagedObjects[i] = bridgedElements[bucket]
      stored += 1
      bucket = hashTable.occupiedBucket(after: bucket)
    }
    theState.extra.0 = CUnsignedLong(bucket.offset)
    state.pointee = theState
    return stored
  }
}

// NOTE: older overlays called this struct _CocoaSet. The two
// must coexist without conflicting ObjC class names from the nested
// classes, so it was renamed. The old names must not be used in the new
// runtime.
@usableFromInline
@frozen
internal struct __CocoaSet {
  @usableFromInline
  internal let object: AnyObject

  @inlinable
  internal init(_ object: __owned AnyObject) {
    self.object = object
  }
}

extension __CocoaSet {
  @usableFromInline
  @_effects(releasenone)
  internal func member(for index: Index) -> AnyObject {
    return index.element
  }

  @usableFromInline
  internal func member(for element: AnyObject) -> AnyObject? {
    let nss = unsafeBitCast(object, to: _NSSet.self)
    return nss.member(element)
  }
}

extension __CocoaSet {
  @usableFromInline
  internal func isEqual(to other: __CocoaSet) -> Bool {
    return _stdlib_NSObject_isEqual(self.object, other.object)
  }
}

extension __CocoaSet: _SetBuffer {
  @usableFromInline
  internal typealias Element = AnyObject

  @usableFromInline // FIXME(cocoa-index): Should be inlinable
  internal var startIndex: Index {
    @_effects(releasenone)
    get {
      let allKeys = _stdlib_NSSet_allObjects(self.object)
      return Index(Index.Storage(self, allKeys), offset: 0)
    }
  }

  @usableFromInline // FIXME(cocoa-index): Should be inlinable
  internal var endIndex: Index {
    @_effects(releasenone)
    get {
      let allKeys = _stdlib_NSSet_allObjects(self.object)
      return Index(Index.Storage(self, allKeys), offset: allKeys.count)
    }
  }

  @usableFromInline // FIXME(cocoa-index): Should be inlinable
  @_effects(releasenone)
  internal func index(after index: Index) -> Index {
    validate(index)
    var result = index
    result._offset += 1
    return result
  }

  internal func validate(_ index: Index) {
    _precondition(index.storage.base.object === self.object,
      "Invalid index")
    _precondition(index._offset < index.storage.allKeys.count,
      "Attempt to access endIndex")
  }

  @usableFromInline // FIXME(cocoa-index): Should be inlinable
  internal func formIndex(after index: inout Index, isUnique: Bool) {
    validate(index)
    index._offset += 1
  }

  @usableFromInline // FIXME(cocoa-index): Should be inlinable
  @_effects(releasenone)
  internal func index(for element: AnyObject) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if !contains(element) {
      return nil
    }

    let allKeys = _stdlib_NSSet_allObjects(object)
    for i in 0..<allKeys.count {
      if _stdlib_NSObject_isEqual(element, allKeys[i]) {
        return Index(Index.Storage(self, allKeys), offset: i)
      }
    }
    _internalInvariantFailure(
      "An NSSet member wasn't listed amongst its enumerated contents")
  }

  @usableFromInline
  internal var count: Int {
    let nss = unsafeBitCast(object, to: _NSSet.self)
    return nss.count
  }

  @usableFromInline
  internal func contains(_ element: AnyObject) -> Bool {
    let nss = unsafeBitCast(object, to: _NSSet.self)
    return nss.member(element) != nil
  }

  @usableFromInline // FIXME(cocoa-index): Make inlinable
  @_effects(releasenone)
  internal func element(at i: Index) -> AnyObject {
    let element: AnyObject? = i.element
    _internalInvariant(element != nil, "Item not found in underlying NSSet")
    return element!
  }
}

extension __CocoaSet {
  @frozen
  @usableFromInline
  internal struct Index {
    internal var _storage: Builtin.BridgeObject
    internal var _offset: Int

    internal var storage: Storage {
      @inline(__always)
      get {
        let storage = _bridgeObject(toNative: _storage)
        return unsafeDowncast(storage, to: Storage.self)
      }
    }

    internal init(_ storage: __owned Storage, offset: Int) {
      self._storage = _bridgeObject(fromNative: storage)
      self._offset = offset
    }
  }
}

extension __CocoaSet.Index {
  // FIXME(cocoa-index): Try using an NSEnumerator to speed this up.
  internal class Storage {
    // Assumption: we rely on NSDictionary.getObjects when being
    // repeatedly called on the same NSDictionary, returning items in the same
    // order every time.
    // Similarly, the same assumption holds for NSSet.allObjects.

    /// A reference to the NSSet, which owns members in `allObjects`,
    /// or `allKeys`, for NSSet and NSDictionary respectively.
    internal let base: __CocoaSet
    // FIXME: swift-3-indexing-model: try to remove the cocoa reference, but
    // make sure that we have a safety check for accessing `allKeys`.  Maybe
    // move both into the dictionary/set itself.

    /// An unowned array of keys.
    internal var allKeys: _BridgingBuffer

    internal init(
      _ base: __owned __CocoaSet,
      _ allKeys: __owned _BridgingBuffer
    ) {
      self.base = base
      self.allKeys = allKeys
    }
  }
}

extension __CocoaSet.Index {
  @usableFromInline
  internal var handleBitPattern: UInt {
    @_effects(readonly)
    get {
      return unsafeBitCast(storage, to: UInt.self)
    }
  }
}

extension __CocoaSet.Index {
  @usableFromInline // FIXME(cocoa-index): Make inlinable
  @nonobjc
  internal var element: AnyObject {
    @_effects(readonly)
    get {
      _precondition(_offset < storage.allKeys.count,
        "Attempting to access Set elements using an invalid index")
      return storage.allKeys[_offset]
    }
  }

  @usableFromInline // FIXME(cocoa-index): Make inlinable
  @nonobjc
  internal var age: Int32 {
    @_effects(releasenone)
    get {
      return _HashTable.age(for: storage.base.object)
    }
  }
}

extension __CocoaSet.Index: Equatable {
  @usableFromInline // FIXME(cocoa-index): Make inlinable
  @_effects(readonly)
  internal static func == (lhs: __CocoaSet.Index, rhs: __CocoaSet.Index) -> Bool {
    _precondition(lhs.storage.base.object === rhs.storage.base.object,
      "Comparing indexes from different sets")
    return lhs._offset == rhs._offset
  }
}

extension __CocoaSet.Index: Comparable {
  @usableFromInline // FIXME(cocoa-index): Make inlinable
  @_effects(readonly)
  internal static func < (lhs: __CocoaSet.Index, rhs: __CocoaSet.Index) -> Bool {
    _precondition(lhs.storage.base.object === rhs.storage.base.object,
      "Comparing indexes from different sets")
    return lhs._offset < rhs._offset
  }
}

extension __CocoaSet: Sequence {
  @usableFromInline
  final internal class Iterator {
    // Cocoa Set iterator has to be a class, otherwise we cannot
    // guarantee that the fast enumeration struct is pinned to a certain memory
    // location.

    // This stored property should be stored at offset zero.  There's code below
    // relying on this.
    internal var _fastEnumerationState: _SwiftNSFastEnumerationState =
      _makeSwiftNSFastEnumerationState()

    // This stored property should be stored right after
    // `_fastEnumerationState`.  There's code below relying on this.
    internal var _fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

    internal let base: __CocoaSet

    internal var _fastEnumerationStatePtr:
      UnsafeMutablePointer<_SwiftNSFastEnumerationState> {
      return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
        to: _SwiftNSFastEnumerationState.self)
    }

    internal var _fastEnumerationStackBufPtr:
      UnsafeMutablePointer<_CocoaFastEnumerationStackBuf> {
      return UnsafeMutableRawPointer(_fastEnumerationStatePtr + 1)
        .assumingMemoryBound(to: _CocoaFastEnumerationStackBuf.self)
    }

    // These members have to be word-sized integers, they cannot be limited to
    // Int8 just because our storage holds 16 elements: fast enumeration is
    // allowed to return inner pointers to the container, which can be much
    // larger.
    internal var itemIndex: Int = 0
    internal var itemCount: Int = 0

    internal init(_ base: __owned __CocoaSet) {
      self.base = base
    }
  }

  @usableFromInline
  internal __consuming func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension __CocoaSet.Iterator: IteratorProtocol {
  @usableFromInline
  internal typealias Element = AnyObject

  @usableFromInline
  internal func next() -> Element? {
    if itemIndex < 0 {
      return nil
    }
    let base = self.base
    if itemIndex == itemCount {
      let stackBufCount = _fastEnumerationStackBuf.count
      // We can't use `withUnsafeMutablePointer` here to get pointers to
      // properties, because doing so might introduce a writeback storage, but
      // fast enumeration relies on the pointer identity of the enumeration
      // state struct.
      itemCount = base.object.countByEnumerating(
        with: _fastEnumerationStatePtr,
        objects: UnsafeMutableRawPointer(_fastEnumerationStackBufPtr)
          .assumingMemoryBound(to: AnyObject.self),
        count: stackBufCount)
      if itemCount == 0 {
        itemIndex = -1
        return nil
      }
      itemIndex = 0
    }
    let itemsPtrUP =
    UnsafeMutableRawPointer(_fastEnumerationState.itemsPtr!)
      .assumingMemoryBound(to: AnyObject.self)
    let itemsPtr = _UnmanagedAnyObjectArray(itemsPtrUP)
    let key: AnyObject = itemsPtr[itemIndex]
    itemIndex += 1
    return key
  }
}

//===--- Bridging ---------------------------------------------------------===//

extension Set {
  @inlinable
  public __consuming func _bridgeToObjectiveCImpl() -> AnyObject {
    guard _variant.isNative else {
      return _variant.asCocoa.object
    }
    return _variant.asNative.bridged()
  }

  /// Returns the native Dictionary hidden inside this NSDictionary;
  /// returns nil otherwise.
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ s: __owned AnyObject
  ) -> Set<Element>? {

    // Try all three NSSet impls that we currently provide.

    if let deferred = s as? _SwiftDeferredNSSet<Element> {
      return Set(_native: deferred.native)
    }

    if let nativeStorage = s as? _SetStorage<Element> {
      return Set(_native: _NativeSet(nativeStorage))
    }

    if s === __RawSetStorage.empty {
      return Set()
    }

    // FIXME: what if `s` is native storage, but for different key/value type?
    return nil
  }
}

#endif // _runtime(_ObjC)
