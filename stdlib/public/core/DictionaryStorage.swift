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
// NOTE: older runtimes called this class _RawDictionaryStorage. The two
// must coexist without a conflicting ObjC class name, so it was
// renamed. The old name must not be used in the new runtime.
@_fixed_layout
@usableFromInline
@_objc_non_lazy_realization
@unsafe
internal class __RawDictionaryStorage: __SwiftNativeNSDictionary {
  // NOTE: The precise layout of this type is relied on in the runtime to
  // provide a statically allocated empty singleton.  See
  // stdlib/public/stubs/GlobalObjects.cpp for details.

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
  internal final var _scale: Int8

  /// The scale corresponding to the highest `reserveCapacity(_:)` call so far,
  /// or 0 if there were none. This may be used later to allow removals to
  /// resize storage.
  ///
  /// FIXME: <rdar://problem/18114559> Shrink storage on deletion
  @usableFromInline
  @nonobjc
  internal final var _reservedScale: Int8

  // Currently unused, set to zero.
  @nonobjc
  internal final var _extra: Int16

  /// A mutation count, enabling stricter index validation.
  @usableFromInline
  @nonobjc
  internal final var _age: Int32

  /// The hash seed used to hash elements in this dictionary instance.
  @usableFromInline
  internal final var _seed: Int

  /// A raw pointer to the start of the tail-allocated hash buffer holding keys.
  @usableFromInline
  @nonobjc
  internal final var _rawKeys: UnsafeMutableRawPointer

  /// A raw pointer to the start of the tail-allocated hash buffer holding
  /// values.
  @usableFromInline
  @nonobjc
  internal final var _rawValues: UnsafeMutableRawPointer

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _internalInvariantFailure("This class cannot be directly initialized")
  }

  @inlinable
  @nonobjc
  internal final var _bucketCount: Int {
    @inline(__always) get { return unsafe 1 &<< _scale }
  }

  @inlinable
  @nonobjc
  internal final var _metadata: UnsafeMutablePointer<_HashTable.Word> {
    @inline(__always) get {
      let address = unsafe Builtin.projectTailElems(self, _HashTable.Word.self)
      return unsafe UnsafeMutablePointer(address)
    }
  }

  // The _HashTable struct contains pointers into tail-allocated storage, so
  // this is unsafe and needs `_fixLifetime` calls in the caller.
  @inlinable
  @nonobjc
  internal final var _hashTable: _HashTable {
    @inline(__always) get {
      return unsafe _HashTable(words: _metadata, bucketCount: _bucketCount)
    }
  }
}

/// The storage class for the singleton empty set.
/// The single instance of this class is created by the runtime.
// NOTE: older runtimes called this class _EmptyDictionarySingleton.
// The two must coexist without a conflicting ObjC class name, so it was
// renamed. The old name must not be used in the new runtime.
@unsafe @_fixed_layout
@usableFromInline
@_objc_non_lazy_realization
internal class __EmptyDictionarySingleton: __RawDictionaryStorage {
  @nonobjc
  internal override init(_doNotCallMe: ()) {
    _internalInvariantFailure("This class cannot be directly initialized")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _internalInvariantFailure("This class cannot be directly initialized")
  }
#endif
}

#if _runtime(_ObjC)
extension __EmptyDictionarySingleton: @unsafe _NSDictionaryCore {
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return unsafe self
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

    var theState = unsafe state.pointee
    if unsafe theState.state == 0 {
      unsafe theState.state = 1 // Arbitrary non-zero value.
      unsafe theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      unsafe theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }
    unsafe state.pointee = theState

    return 0
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    return nil
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return __SwiftEmptyNSEnumerator()
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

#if $Embedded
// In embedded Swift, the stdlib is a .swiftmodule only without any .o/.a files,
// to allow consuming it by clients with different LLVM codegen setting (-mcpu
// flags, etc.), which means we cannot declare the singleton in a C/C++ file.
//
// TODO: We should figure out how to make this a constant so that it's placed in
// non-writable memory (can't be a let, Builtin.addressof below requires a var).
@unsafe
public var _swiftEmptyDictionarySingleton: (Int, Int, Int, Int, UInt8, UInt8, UInt16, UInt32, Int, Int, Int, Int) =
    (
      /*isa*/0, /*refcount*/-1, // HeapObject header
      /*count*/0,
      /*capacity*/0,
      /*scale*/0,
      /*reservedScale*/0,
      /*extra*/0,
      /*age*/0,
      /*seed*/0,
      /*rawKeys*/1,
      /*rawValues*/1,
      /*metadata*/~1
    )
#endif

extension __RawDictionaryStorage {
  /// The empty singleton that is used for every single Dictionary that is
  /// created without any elements. The contents of the storage should never
  /// be mutated.
  @inlinable
  @nonobjc
  internal static var empty: __EmptyDictionarySingleton {
    return unsafe Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyDictionarySingleton))
  }
  
  @_alwaysEmitIntoClient
  @inline(__always)
  internal final func uncheckedKey<Key: Hashable>(at bucket: _HashTable.Bucket) -> Key {
    defer { unsafe _fixLifetime(self) }
    unsafe _internalInvariant(_hashTable.isOccupied(bucket))
    let keys = unsafe _rawKeys.assumingMemoryBound(to: Key.self)
    return unsafe keys[bucket.offset]
  }

  @safe
  @_alwaysEmitIntoClient
  @inline(never)
  internal final func find<Key: Hashable>(_ key: Key) -> (bucket: _HashTable.Bucket, found: Bool) {
    return unsafe find(key, hashValue: key._rawHashValue(seed: _seed))
  }

  @safe
  @_alwaysEmitIntoClient
  @inline(never)
  internal final func find<Key: Hashable>(_ key: Key, hashValue: Int) -> (bucket: _HashTable.Bucket, found: Bool) {
      let hashTable = unsafe _hashTable
      var bucket = unsafe hashTable.idealBucket(forHashValue: hashValue)
      while unsafe hashTable._isOccupied(bucket) {
        if unsafe uncheckedKey(at: bucket) == key {
          return (bucket, true)
        }
        bucket = unsafe hashTable.bucket(wrappedAfter: bucket)
      }
      return (bucket, false)
  }
}

@unsafe @usableFromInline
final internal class _DictionaryStorage<Key: Hashable, Value>
  : __RawDictionaryStorage, @unsafe _NSDictionaryCore {
  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _internalInvariantFailure("This class cannot be directly initialized")
  }

  deinit {
    guard unsafe _count > 0 else { return }
    if !_isPOD(Key.self) {
      let keys = unsafe self._keys
      for unsafe bucket in unsafe _hashTable {
        unsafe (keys + bucket.offset).deinitialize(count: 1)
      }
    }
    if !_isPOD(Value.self) {
      let values = unsafe self._values
      for unsafe bucket in unsafe _hashTable {
        unsafe (values + bucket.offset).deinitialize(count: 1)
      }
    }
    unsafe _count = 0
    unsafe _fixLifetime(self)
  }

  @inlinable
  final internal var _keys: UnsafeMutablePointer<Key> {
    @inline(__always)
    get {
      return unsafe self._rawKeys.assumingMemoryBound(to: Key.self)
    }
  }

  @inlinable
  final internal var _values: UnsafeMutablePointer<Value> {
    @inline(__always)
    get {
      return unsafe self._rawValues.assumingMemoryBound(to: Value.self)
    }
  }

  internal var asNative: _NativeDictionary<Key, Value> {
    return unsafe _NativeDictionary(self)
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _internalInvariantFailure("This class cannot be directly initialized")
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return unsafe self
  }

  @objc
  internal var count: Int {
    return unsafe _count
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return unsafe _SwiftDictionaryNSEnumerator<Key, Value>(asNative)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    defer { unsafe _fixLifetime(self) }
    let hashTable = unsafe _hashTable

    var theState = unsafe state.pointee
    if unsafe theState.state == 0 {
      unsafe theState.state = 1 // Arbitrary non-zero value.
      unsafe theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      unsafe theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      unsafe theState.extra.0 = CUnsignedLong(hashTable.startBucket.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if unsafe _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = unsafe _UnmanagedAnyObjectArray(objects!)
    var bucket = unsafe _HashTable.Bucket(offset: Int(theState.extra.0))
    let endBucket = unsafe hashTable.endBucket
    unsafe _precondition(bucket == endBucket || hashTable.isOccupied(bucket),
      "Invalid fast enumeration state")
    var stored = 0
    for i in 0..<count {
      if bucket == endBucket { break }

      let key = unsafe _keys[bucket.offset]
      unsafe unmanagedObjects[i] = _bridgeAnythingToObjectiveC(key)

      stored += 1
      bucket = unsafe hashTable.occupiedBucket(after: bucket)
    }
    unsafe theState.extra.0 = CUnsignedLong(bucket.offset)
    unsafe state.pointee = theState
    return stored
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (bucket, found) = unsafe asNative.find(nativeKey)
    guard found else { return nil }
    let value = unsafe asNative.uncheckedValue(at: bucket)
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
    switch unsafe (_UnmanagedAnyObjectArray(keys), _UnmanagedAnyObjectArray(objects)) {
    case (let unmanagedKeys?, let unmanagedObjects?):
      for (key, value) in unsafe asNative {
        unsafe unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
        unsafe unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (let unmanagedKeys?, nil):
      for (key, _) in unsafe asNative {
        unsafe unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (nil, let unmanagedObjects?):
      for (_, value) in unsafe asNative {
        unsafe unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
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

extension _DictionaryStorage {
  @usableFromInline
  @_effects(releasenone)
  internal static func copy(
    original: __RawDictionaryStorage
  ) -> _DictionaryStorage {
    return unsafe allocate(
      scale: original._scale,
      age: original._age,
      seed: original._seed)
  }

  @usableFromInline
  @_effects(releasenone)
  static internal func resize(
    original: __RawDictionaryStorage,
    capacity: Int,
    move: Bool
  ) -> _DictionaryStorage {
    let scale = unsafe _HashTable.scale(forCapacity: capacity)
    return unsafe allocate(scale: scale, age: nil, seed: nil)
  }

  @usableFromInline
  @_effects(releasenone)
  static internal func allocate(capacity: Int) -> _DictionaryStorage {
    let scale = unsafe _HashTable.scale(forCapacity: capacity)
    return unsafe allocate(scale: scale, age: nil, seed: nil)
  }

#if _runtime(_ObjC)
  @usableFromInline
  @_effects(releasenone)
  static internal func convert(
    _ cocoa: __CocoaDictionary,
    capacity: Int
  ) -> _DictionaryStorage {
    let scale = unsafe _HashTable.scale(forCapacity: capacity)
    let age = unsafe _HashTable.age(for: cocoa.object)
    return unsafe allocate(scale: scale, age: age, seed: nil)
  }
#endif

  static internal func allocate(
    scale: Int8,
    age: Int32?,
    seed: Int?
  ) -> _DictionaryStorage {
    // The entry count must be representable by an Int value; hence the scale's
    // peculiar upper bound.
    _internalInvariant(scale >= 0 && scale < Int.bitWidth - 1)

    let bucketCount = (1 as Int) &<< scale
    let wordCount = unsafe _UnsafeBitset.wordCount(forCapacity: bucketCount)
    let storage = unsafe Builtin.allocWithTailElems_3(
      _DictionaryStorage<Key, Value>.self,
      wordCount._builtinWordValue, _HashTable.Word.self,
      bucketCount._builtinWordValue, Key.self,
      bucketCount._builtinWordValue, Value.self)

    let metadataAddr = unsafe Builtin.projectTailElems(storage, _HashTable.Word.self)
    let keysAddr = Builtin.getTailAddr_Word(
      metadataAddr, wordCount._builtinWordValue, _HashTable.Word.self,
      Key.self)
    let valuesAddr = Builtin.getTailAddr_Word(
      keysAddr, bucketCount._builtinWordValue, Key.self,
      Value.self)
    unsafe storage._count = 0
    unsafe storage._capacity = unsafe _HashTable.capacity(forScale: scale)
    unsafe storage._scale = scale
    unsafe storage._reservedScale = 0
    unsafe storage._extra = 0

    if let age = age {
      unsafe storage._age = age
    } else {
      // The default mutation count is simply a scrambled version of the storage
      // address.
      unsafe storage._age = Int32(
        truncatingIfNeeded: ObjectIdentifier(storage).hashValue)
    }

    unsafe storage._seed = unsafe seed ?? _HashTable.hashSeed(for: Builtin.castToNativeObject(storage), scale: scale)
    unsafe storage._rawKeys = UnsafeMutableRawPointer(keysAddr)
    unsafe storage._rawValues = UnsafeMutableRawPointer(valuesAddr)

    // Initialize hash table metadata.
    unsafe storage._hashTable.clear()
    return unsafe storage
  }
}
