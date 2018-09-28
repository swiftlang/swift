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

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _DictionaryBuffer {
  associatedtype Key
  associatedtype Value
  associatedtype Index

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func index(forKey key: Key) -> Index?
  var count: Int { get }

  func contains(_ key: Key) -> Bool
  func lookup(_ key: Key) -> Value?
  func lookup(_ index: Index) -> (key: Key, value: Value)
  func key(at index: Index) -> Key
  func value(at index: Index) -> Value
}

extension Dictionary {
  @usableFromInline
  @_frozen
  internal enum _Variant {
    case native(_NativeDictionary<Key, Value>)
#if _runtime(_ObjC)
    case cocoa(_CocoaDictionary)
#endif
  }
}

extension Dictionary._Variant {
#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  // Allow the optimizer to consider the surrounding code unreachable if Element
  // is guaranteed to be native.
  @usableFromInline @_transparent
  internal func cocoaPath() {
    if guaranteedNative {
      _conditionallyUnreachable()
    }
  }
#endif

  @inlinable
  internal mutating func isUniquelyReferenced() -> Bool {
#if _runtime(_ObjC)
    guard isNative else {
      // Don't consider Cocoa a buffer mutable, even if it is mutable and it is
      // uniquely referenced.
      return false
    }
#endif
    // Note that &self drills down through .native(_NativeDictionary) to the
    // first property in _NativeDictionary, which is the reference to the
    // storage.
    return _isUnique_native(&self)
  }

#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var isNative: Bool {
    switch self {
    case .native:
      return true
    case .cocoa:
      cocoaPath()
      return false
    }
  }
#endif

  @inlinable
  internal var asNative: _NativeDictionary<Key, Value> {
    @inline(__always)
    get {
      switch self {
      case .native(let native):
        return native
#if _runtime(_ObjC)
      case .cocoa:
        _sanityCheckFailure("internal error: not backed by native buffer")
#endif
      }
    }
    @inline(__always)
    set {
      self = .native(newValue)
    }
  }

#if _runtime(_ObjC)
  @inlinable
  internal var asCocoa: _CocoaDictionary {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
    case .cocoa(let cocoa):
      return cocoa
    }
  }
#endif

  /// Reserves enough space for the specified number of elements to be stored
  /// without reallocating additional storage.
  internal mutating func reserveCapacity(_ capacity: Int) {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoa = asCocoa
      let capacity = Swift.max(cocoa.count, capacity)
      self = .native(_NativeDictionary(cocoa, capacity: capacity))
      return
    }
#endif
    let isUnique = isUniquelyReferenced()
    asNative.reserveCapacity(capacity, isUnique: isUnique)
  }

  /// The number of elements that can be stored without expanding the current
  /// storage.
  ///
  /// For bridged storage, this is equal to the current count of the
  /// collection, since any addition will trigger a copy of the elements into
  /// newly allocated storage. For native storage, this is the element count
  /// at which adding any more elements will exceed the load factor.
  @inlinable
  internal var capacity: Int {
#if _runtime(_ObjC)
    guard isNative else {
      return asCocoa.count
    }
#endif
    return asNative.capacity
  }
}

extension Dictionary._Variant: _DictionaryBuffer {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)
  @usableFromInline
  internal typealias Index = Dictionary<Key, Value>.Index

  @inlinable
  internal var startIndex: Index {
#if _runtime(_ObjC)
    guard isNative else {
      return Index(_cocoa: asCocoa.startIndex)
    }
#endif
    return asNative.startIndex
  }

  @inlinable
  internal var endIndex: Index {
#if _runtime(_ObjC)
    guard isNative else {
      return Index(_cocoa: asCocoa.endIndex)
    }
#endif
    return asNative.endIndex
  }

  @inlinable
  internal func index(after index: Index) -> Index {
#if _runtime(_ObjC)
    guard isNative else {
      return Index(_cocoa: asCocoa.index(after: index._asCocoa))
    }
#endif
    return asNative.index(after: index)
  }

  @inlinable
  @inline(__always)
  internal func index(forKey key: Key) -> Index? {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let index = asCocoa.index(forKey: cocoaKey) else { return nil }
      return Index(_cocoa: index)
    }
#endif
    return asNative.index(forKey: key)
  }

  @inlinable
  internal var count: Int {
    @inline(__always)
    get {
#if _runtime(_ObjC)
      guard isNative else {
        return asCocoa.count
      }
#endif
      return asNative.count
    }
  }

  @inlinable
  @inline(__always)
  func contains(_ key: Key) -> Bool {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      return asCocoa.contains(cocoaKey)
    }
#endif
    return asNative.contains(key)
  }

  @inlinable
  @inline(__always)
  func lookup(_ key: Key) -> Value? {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let cocoaValue = asCocoa.lookup(cocoaKey) else { return nil }
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
    }
#endif
    return asNative.lookup(key)
  }

  @inlinable
  @inline(__always)
  func lookup(_ index: Index) -> (key: Key, value: Value) {
#if _runtime(_ObjC)
    guard isNative else {
      let (cocoaKey, cocoaValue) = asCocoa.lookup(index._asCocoa)
      let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      return (nativeKey, nativeValue)
    }
#endif
    return asNative.lookup(index)
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaKey = asCocoa.key(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaKey, Key.self)
    }
#endif
    return asNative.key(at: index)
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaValue = asCocoa.value(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
    }
#endif
    return asNative.value(at: index)
  }
}

extension Dictionary._Variant {
  /// Same as find(_:), except assume a corresponding key/value pair will be
  /// inserted if it doesn't already exist, and mutated if it does exist. When
  /// this function returns, the storage is guaranteed to be native, uniquely
  /// held, and with enough capacity for a single insertion (if the key isn't
  /// already in the dictionary.)
  @inlinable
  @inline(__always)
  internal mutating func mutatingFind(
    _ key: Key
  ) -> (bucket: _NativeDictionary<Key, Value>.Bucket, found: Bool) {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoa = asCocoa
      var native = _NativeDictionary<Key, Value>(
        cocoa, capacity: cocoa.count + 1)
      let result = native.mutatingFind(key, isUnique: true)
      self = .native(native)
      return result
    }
#endif
    let isUnique = isUniquelyReferenced()
    return asNative.mutatingFind(key, isUnique: isUnique)
  }

  @inlinable
  @inline(__always)
  internal mutating func ensureUniqueNative() -> _NativeDictionary<Key, Value> {
#if _runtime(_ObjC)
    guard isNative else {
      cocoaPath()
      let native = _NativeDictionary<Key, Value>(asCocoa)
      self = .native(native)
      return native
    }
#endif
    let isUnique = isUniquelyReferenced()
    if !isUnique {
      asNative.copy()
    }
    return asNative
  }

  @inlinable
  internal mutating func updateValue(
    _ value: __owned Value,
    forKey key: Key
  ) -> Value? {
#if _runtime(_ObjC)
    guard isNative else {
      // Make sure we have space for an extra element.
      let cocoa = asCocoa
      var native = _NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1)
      let result = native.updateValue(value, forKey: key, isUnique: true)
      self = .native(native)
      return result
    }
#endif
    let isUnique = self.isUniquelyReferenced()
    return asNative.updateValue(value, forKey: key, isUnique: isUnique)
  }

  @inlinable
  internal mutating func setValue(_ value: __owned Value, forKey key: Key) {
#if _runtime(_ObjC)
    if !isNative {
      // Make sure we have space for an extra element.
      let cocoa = asCocoa
      self = .native(_NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1))
    }
#endif
    let isUnique = self.isUniquelyReferenced()
    asNative.setValue(value, forKey: key, isUnique: isUnique)
  }

  @inlinable
  internal mutating func remove(at index: Index) -> Element {
    // FIXME(performance): fuse data migration and element deletion into one
    // operation.
    let native = ensureUniqueNative()
    let bucket = native.validatedBucket(for: index)
    return asNative.uncheckedRemove(at: bucket, isUnique: true)
  }

  @inlinable
  internal mutating func removeValue(forKey key: Key) -> Value? {
#if _runtime(_ObjC)
    guard isNative else {
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      let cocoa = asCocoa
      guard cocoa.lookup(cocoaKey) != nil else { return nil }
      var native = _NativeDictionary<Key, Value>(cocoa)
      let (bucket, found) = native.find(key)
      _precondition(found, "Bridging did not preserve equality")
      let old = native.uncheckedRemove(at: bucket, isUnique: true).value
      self = .native(native)
      return old
    }
#endif
    let (bucket, found) = asNative.find(key)
    guard found else { return nil }
    let isUnique = isUniquelyReferenced()
    return asNative.uncheckedRemove(at: bucket, isUnique: isUnique).value
  }

  @inlinable
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if !keepCapacity {
      self = .native(_NativeDictionary())
      return
    }
    guard count > 0 else { return }

#if _runtime(_ObjC)
    guard isNative else {
      self = .native(_NativeDictionary(capacity: asCocoa.count))
      return
    }
#endif
    let isUnique = isUniquelyReferenced()
    asNative.removeAll(isUnique: isUnique)
  }
}

extension Dictionary._Variant {
  /// Returns an iterator over the `(Key, Value)` pairs.
  ///
  /// - Complexity: O(1).
  @inlinable
  @inline(__always)
  __consuming internal func makeIterator() -> Dictionary<Key, Value>.Iterator {
#if _runtime(_ObjC)
    guard isNative else {
      return Dictionary.Iterator(_cocoa: asCocoa.makeIterator())
    }
#endif
    return Dictionary.Iterator(_native: asNative.makeIterator())
  }
}

extension Dictionary._Variant {
  @inlinable
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
#if _runtime(_ObjC)
    guard isNative else {
      return try asCocoa.mapValues(transform)
    }
#endif
    return try asNative.mapValues(transform)
  }

  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: __owned S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
#if _runtime(_ObjC)
    guard isNative else {
      var native = _NativeDictionary<Key, Value>(asCocoa)
      try native.merge(
        keysAndValues,
        isUnique: true,
        uniquingKeysWith: combine)
      self = .native(native)
      return
    }
#endif
    let isUnique = isUniquelyReferenced()
    try asNative.merge(
      keysAndValues,
      isUnique: isUnique,
      uniquingKeysWith: combine)
  }
}

