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
    switch self {
    case .native:
      // Note that &self drills down through .native(_NativeDictionary) to the
      // first property in _NativeDictionary, which is the reference to the
      // storage.
      return _isUnique_native(&self)
#if _runtime(_ObjC)
    case .cocoa:
      cocoaPath()
      // Don't consider Cocoa buffer mutable, even if it is mutable and is
      // uniquely referenced.
      return false
#endif
    }
  }

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
  @inlinable
  internal mutating func reserveCapacity(_ capacity: Int) {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.reserveCapacity(capacity, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let capacity = Swift.max(cocoa.count, capacity)
      self = .native(_NativeDictionary(cocoa, capacity: capacity))
#endif
    }
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
    switch self {
    case .native:
      return asNative.capacity
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return cocoa.count
#endif
    }
  }
}

extension Dictionary._Variant: _DictionaryBuffer {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)
  @usableFromInline
  internal typealias Index = Dictionary<Key, Value>.Index

  @inlinable
  internal var startIndex: Index {
    switch self {
    case .native(let native):
      return native.startIndex
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.startIndex)
#endif
    }
  }

  @inlinable
  internal var endIndex: Index {
    switch self {
    case .native(let native):
      return native.endIndex
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.endIndex)
#endif
    }
  }

  @inlinable
  internal func index(after index: Index) -> Index {
    switch self {
    case .native(let native):
      return native.index(after: index)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.index(after: index._asCocoa))
#endif
    }
  }

  @inlinable
  @inline(__always)
  internal func index(forKey key: Key) -> Index? {
    switch self {
    case .native(let native):
      return native.index(forKey: key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let index = cocoa.index(forKey: cocoaKey) else { return nil }
      return Index(_cocoa: index)
#endif
    }
  }

  @inlinable
  internal var count: Int {
    @inline(__always)
    get {
      switch self {
      case .native(let native):
        return native.count
#if _runtime(_ObjC)
      case .cocoa(let cocoa):
        cocoaPath()
        return cocoa.count
#endif
      }
    }
  }

  @inlinable
  @inline(__always)
  func contains(_ key: Key) -> Bool {
    switch self {
    case .native(let native):
      return native.contains(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      return cocoa.contains(cocoaKey)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func lookup(_ key: Key) -> Value? {
    switch self {
    case .native(let native):
      return native.lookup(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let cocoaValue = cocoa.lookup(cocoaKey) else { return nil }
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func lookup(_ index: Index) -> (key: Key, value: Value) {
    switch self {
    case .native(let native):
      return native.lookup(index)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let (cocoaKey, cocoaValue) = cocoa.lookup(index._asCocoa)
      let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      return (nativeKey, nativeValue)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
    switch self {
    case .native(let native):
      return native.key(at: index)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = cocoa.key(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaKey, Key.self)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
    switch self {
    case .native(let native):
      return native.value(at: index)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaValue = cocoa.value(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
#endif
    }
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
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      return asNative.mutatingFind(key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      var native = _NativeDictionary<Key, Value>(
        cocoa, capacity: cocoa.count + 1)
      let result = native.mutatingFind(key, isUnique: true)
      self = .native(native)
      return result
#endif
    }
  }

  /// Ensure uniquely held native storage, while preserving the given index.
  /// (If the variant had bridged storage, then the returned index will be the
  /// corresponding native representation. Otherwise it's kept the same.)
  @inlinable
  @inline(__always)
  internal mutating func ensureUniqueNative() -> _NativeDictionary<Key, Value> {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      if !isUnique {
        let rehashed = asNative.copy(capacity: asNative.capacity)
        _sanityCheck(!rehashed)
      }
      return asNative
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let native = _NativeDictionary<Key, Value>(cocoa)
      self = .native(native)
      return native
#endif
    }
  }

  @inlinable
  internal mutating func updateValue(
    _ value: __owned Value,
    forKey key: Key
  ) -> Value? {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      return asNative.updateValue(value, forKey: key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      // Make sure we have space for an extra element.
      var native = _NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1)
      let result = native.updateValue(value, forKey: key, isUnique: true)
      self = .native(native)
      return result
#endif
    }
  }

  @inlinable
  internal mutating func setValue(_ value: __owned Value, forKey key: Key) {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      asNative.setValue(value, forKey: key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      // Make sure we have space for an extra element.
      var native = _NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1)
      native.setValue(value, forKey: key, isUnique: true)
      self = .native(native)
#endif
    }
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
    switch self {
    case .native:
      let (bucket, found) = asNative.find(key)
      guard found else { return nil }
      let isUnique = isUniquelyReferenced()
      return asNative.uncheckedRemove(at: bucket, isUnique: isUnique).value
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard cocoa.lookup(cocoaKey) != nil else { return nil }
      var native = _NativeDictionary<Key, Value>(cocoa)
      let (bucket, found) = native.find(key)
      _precondition(found, "Bridging did not preserve equality")
      let old = native.uncheckedRemove(at: bucket, isUnique: true).value
      self = .native(native)
      return old
#endif
    }
  }

  @inlinable
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if !keepCapacity {
      self = .native(_NativeDictionary())
      return
    }
    guard count > 0 else { return }

    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.removeAll(isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      self = .native(_NativeDictionary(capacity: cocoa.count))
#endif
    }
  }
}

extension Dictionary._Variant {
  /// Returns an iterator over the `(Key, Value)` pairs.
  ///
  /// - Complexity: O(1).
  @inlinable
  @inline(__always)
  __consuming internal func makeIterator() -> Dictionary<Key, Value>.Iterator {
    switch self {
    case .native(let native):
      return Dictionary.Iterator(_native: native.makeIterator())
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Dictionary.Iterator(_cocoa: cocoa.makeIterator())
#endif
    }
  }
}

extension Dictionary._Variant {
  @inlinable
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    switch self {
    case .native(let native):
      return try native.mapValues(transform)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return try cocoa.mapValues(transform)
#endif
    }
  }

  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: __owned S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      try asNative.merge(
        keysAndValues,
        isUnique: isUnique,
        uniquingKeysWith: combine)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      var native = _NativeDictionary<Key, Value>(cocoa)
      try native.merge(
        keysAndValues,
        isUnique: true,
        uniquingKeysWith: combine)
      self = .native(native)
#endif
    }
  }
}

