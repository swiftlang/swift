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

import SwiftShims

/// An instance of this class has all `Dictionary` data tail-allocated.
/// Enough bytes are allocated to hold the bitmap for marking valid entries,
/// keys, and values. The data layout starts with the bitmap, followed by the
/// keys, followed by the values.
//
// See the docs at the top of the file for more details on this type
//
// NOTE: The precise layout of this type is relied on in the runtime
// to provide a statically allocated empty singleton.
// See stdlib/public/stubs/GlobalObjects.cpp for details.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
@_objc_non_lazy_realization
internal class _RawDictionaryStorage: __SwiftNativeNSDictionary {
  /// The current number of occupied entries in this dictionary.
  @usableFromInline
  @nonobjc
  internal final var _count: Int

  /// The maximum number of elements that can be inserted into this set without
  /// exceeding the hash table's maximum load factor.
  @usableFromInline
  @nonobjc
  internal final var _capacity: Int

  /// The scale of this dictionary. The number of buckets is 2 raised to the
  /// power of `scale`.
  @usableFromInline
  @nonobjc
  internal final var _scale: Int

  @usableFromInline
  internal final var _seed: Int

  @usableFromInline
  @nonobjc
  internal final var _rawKeys: UnsafeMutableRawPointer

  @usableFromInline
  @nonobjc
  internal final var _rawValues: UnsafeMutableRawPointer

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

  @inlinable
  @nonobjc
  internal final var _bucketCount: Int {
    @inline(__always) get { return 1 &<< _scale }
  }

  @inlinable
  @nonobjc
  internal final var _metadata: UnsafeMutablePointer<_HashTable.Word> {
    @inline(__always) get {
      let address = Builtin.projectTailElems(self, _HashTable.Word.self)
      return UnsafeMutablePointer(address)
    }
  }

  // The _HashTable struct contains pointers into tail-allocated storage, so
  // this is unsafe and needs `_fixLifetime` calls in the caller.
  @inlinable
  @nonobjc
  internal final var _hashTable: _HashTable {
    @inline(__always) get {
      return _HashTable(words: _metadata, bucketCount: _bucketCount)
    }
  }
}

/// The storage class for the singleton empty set.
/// The single instance of this class is created by the runtime.
@_fixed_layout
@usableFromInline
internal class _EmptyDictionarySingleton: _RawDictionaryStorage {
  @nonobjc
  internal override init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }
#endif
}

#if _runtime(_ObjC)
extension _EmptyDictionarySingleton: _NSDictionaryCore {
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal var count: Int {
    return 0
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    // Even though we never do anything in here, we need to update the
    // state so that callers know we actually ran.

    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }
    state.pointee = theState

    return 0
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    return nil
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return _SwiftEmptyNSEnumerator()
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    // Do nothing, we're empty
  }
}
#endif

extension _RawDictionaryStorage {
  /// The empty singleton that is used for every single Dictionary that is
  /// created without any elements. The contents of the storage should never
  /// be mutated.
  @inlinable
  @nonobjc
  internal static var empty: _EmptyDictionarySingleton {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyDictionarySingleton))
  }
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
final internal class _DictionaryStorage<Key: Hashable, Value>
  : _RawDictionaryStorage, _NSDictionaryCore {
  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }
#endif

  deinit {
    guard _count > 0 else { return }
    if !_isPOD(Key.self) {
      let keys = self._keys
      for index in _hashTable {
        (keys + index.bucket).deinitialize(count: 1)
      }
    }
    if !_isPOD(Value.self) {
      let values = self._values
      for index in _hashTable {
        (values + index.bucket).deinitialize(count: 1)
      }
    }
    _count = 0
    _fixLifetime(self)
  }

  @inlinable
  final internal var _keys: UnsafeMutablePointer<Key> {
    @inline(__always)
    get {
      return self._rawKeys.assumingMemoryBound(to: Key.self)
    }
  }

  @inlinable
  final internal var _values: UnsafeMutablePointer<Value> {
    @inline(__always)
    get {
      return self._rawValues.assumingMemoryBound(to: Value.self)
    }
  }

  internal var asNative: _NativeDictionary<Key, Value> {
    return _NativeDictionary(self)
  }

  @usableFromInline
  @_effects(releasenone)
  internal static func reallocate(
    original: _RawDictionaryStorage,
    capacity: Int
  ) -> (storage: _DictionaryStorage, rehash: Bool) {
    _sanityCheck(capacity >= original._count)
    let scale = _HashTable.scale(forCapacity: capacity)
    let rehash = (scale != original._scale)
    let newStorage = _DictionaryStorage<Key, Value>.allocate(scale: scale)
    return (newStorage, rehash)
  }

  @usableFromInline
  @_effects(releasenone)
  static internal func allocate(capacity: Int) -> _DictionaryStorage {
    let scale = _HashTable.scale(forCapacity: capacity)
    return allocate(scale: scale)
  }

  static internal func allocate(scale: Int) -> _DictionaryStorage {
    // The entry count must be representable by an Int value; hence the scale's
    // peculiar upper bound.
    _sanityCheck(scale >= 0 && scale < Int.bitWidth - 1)

    let bucketCount = 1 &<< scale
    let wordCount = _UnsafeBitset.wordCount(forCapacity: bucketCount)
    let storage = Builtin.allocWithTailElems_3(
      _DictionaryStorage<Key, Value>.self,
      wordCount._builtinWordValue, _HashTable.Word.self,
      bucketCount._builtinWordValue, Key.self,
      bucketCount._builtinWordValue, Value.self)

    let metadataAddr = Builtin.projectTailElems(storage, _HashTable.Word.self)
    let keysAddr = Builtin.getTailAddr_Word(
      metadataAddr, wordCount._builtinWordValue, _HashTable.Word.self,
      Key.self)
    let valuesAddr = Builtin.getTailAddr_Word(
      keysAddr, bucketCount._builtinWordValue, Key.self,
      Value.self)
    storage._count = 0
    storage._capacity = _HashTable.capacity(forScale: scale)
    storage._scale = scale
    storage._rawKeys = UnsafeMutableRawPointer(keysAddr)
    storage._rawValues = UnsafeMutableRawPointer(valuesAddr)

    // We use a slightly different hash seed whenever we change the size of the
    // hash table, so that we avoid certain copy operations becoming quadratic,
    // without breaking value semantics. (For background details, see
    // https://bugs.swift.org/browse/SR-3268)

    // FIXME: Use true per-instance seeding instead. Per-capacity seeding still
    // leaves hash values the same in same-sized tables, which may affect
    // operations on two tables at once. (E.g., union.)
    storage._seed = scale

    // Initialize hash table metadata.
    storage._hashTable.clear()
    return storage
  }

#if _runtime(_ObjC)
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal var count: Int {
    return _count
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return _SwiftDictionaryNSEnumerator<Key, Value>(asNative)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(asNative.startIndex.bucket)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var index = _HashTable.Index(bucket: Int(theState.extra.0))
    let endIndex = asNative.endIndex
    _precondition(index == endIndex || _hashTable.isOccupied(index))
    var stored = 0
    for i in 0..<count {
      if index == endIndex { break }

      let key = asNative.uncheckedKey(at: index)
      unmanagedObjects[i] = _bridgeAnythingToObjectiveC(key)

      stored += 1
      index = asNative.index(after: index)
    }
    theState.extra.0 = CUnsignedLong(index.bucket)
    state.pointee = theState
    return stored
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (index, found) = asNative.find(nativeKey)
    guard found else { return nil }
    let value = asNative.uncheckedValue(at: index)
    return _bridgeAnythingToObjectiveC(value)
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    _precondition(count >= 0, "Invalid count")
    guard count > 0 else { return }
    var i = 0 // Current position in the output buffers
    switch (_UnmanagedAnyObjectArray(keys), _UnmanagedAnyObjectArray(objects)) {
    case (let unmanagedKeys?, let unmanagedObjects?):
      for (key, value) in asNative {
        unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
        unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (let unmanagedKeys?, nil):
      for (key, _) in asNative {
        unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (nil, let unmanagedObjects?):
      for (_, value) in asNative {
        unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
        i += 1
        guard i < count else { break }
      }
    case (nil, nil):
      // Do nothing.
      break
    }
  }
#endif
}

