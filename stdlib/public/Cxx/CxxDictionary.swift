//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A C++ type that represents a dictionary.
///
/// C++ standard library types such as `std::map` and `std::unordered_map`
/// conform to this protocol.
public protocol CxxDictionary<Key, Value>: ExpressibleByDictionaryLiteral {
  override associatedtype Key
  override associatedtype Value
  associatedtype Element: CxxPair<Key, Value>
  associatedtype RawIterator: UnsafeCxxInputIterator
    where RawIterator.Pointee == Element
  associatedtype RawMutableIterator: UnsafeCxxMutableInputIterator
    where RawMutableIterator.Pointee == Element
  associatedtype Size: BinaryInteger
  associatedtype InsertionResult

  init()

  /// Do not implement this function manually in Swift.
  func __findUnsafe(_ key: Key) -> RawIterator
  /// Do not implement this function manually in Swift.
  mutating func __findMutatingUnsafe(_ key: Key) -> RawMutableIterator

  /// Do not implement this function manually in Swift.
  @discardableResult
  mutating func __insertUnsafe(_ element: Element) -> InsertionResult

  /// Do not implement this function manually in Swift.
  @discardableResult
  mutating func erase(_ key: Key) -> Size

  /// Do not implement this function manually in Swift.
  @discardableResult
  mutating func __eraseUnsafe(_ iter: RawMutableIterator) -> RawMutableIterator

  /// Do not implement this function manually in Swift.
  func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  func __endUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  mutating func __endMutatingUnsafe() -> RawMutableIterator
}

extension CxxDictionary {
  /// Creates a C++ map containing the elements of a Swift Dictionary.
  ///
  /// This initializes the map by copying every key and value of the dictionary.
  ///
  /// - Complexity: O(*n*), where *n* is the number of entries in the Swift
  ///   dictionary
  @inlinable
  public init(_ dictionary: Dictionary<Key, Value>) where Key: Hashable {
    self.init()
    for (key, value) in dictionary {
      self[key] = value
    }
  }

  @inlinable
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init()
    for (key, value) in elements {
      self[key] = value
    }
  }

  @inlinable
  public init<S: Sequence>(
    grouping values: __owned S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value: CxxVector<S.Element> {
    self.init()
    for value in values {
      let key = try keyForValue(value)
      var iter = __findMutatingUnsafe(key)
      if iter != __endMutatingUnsafe() {
        iter.pointee.second.push_back(value)
      } else {
        var vector = Value()
        vector.push_back(value)
        self[key] = vector
      }
    }
  }

  @inlinable
  public subscript(key: Key) -> Value? {
    get {
      let iter = __findUnsafe(key)
      guard iter != __endUnsafe() else {
        return nil
      }
      return iter.pointee.second
    }
    set(newValue) {
      guard let newValue = newValue else {
        self.erase(key)
        return
      }
      var iter = self.__findMutatingUnsafe(key)
      if iter != self.__endMutatingUnsafe() {
        // This key already exists in the dictionary.
        iter.pointee.second = newValue
      } else {
        // Create a std::pair<key_type, mapped_type>.
        let keyValuePair = Element(first: key, second: newValue)
        self.__insertUnsafe(keyValuePair)
      }
    }
  }

  @inlinable
  public subscript(
    key: Key, default defaultValue: @autoclosure () -> Value
  ) -> Value {
    get {
      let iter = __findUnsafe(key)
      guard iter != __endUnsafe() else {
        return defaultValue()
      }
      return iter.pointee.second
    }
    set(newValue) {
      var iter = self.__findMutatingUnsafe(key)
      if iter != self.__endMutatingUnsafe() {
        iter.pointee.second = newValue
      } else {
        let keyValuePair = Element(first: key, second: newValue)
        self.__insertUnsafe(keyValuePair)
      }
    }
  }

  @inlinable
  public func mapValues<R, E>(
    _ transform: (Value) throws(E) -> R.Value
  ) throws(E) -> R where R: CxxDictionary, R.Key == Key {
    var result = R.init()

    var iterator = __beginUnsafe()
    let endIterator = __endUnsafe()

    while iterator != endIterator {
      let pair = iterator.pointee
      try result.__insertUnsafe(R.Element(first: pair.first, second: transform(pair.second)))
      iterator = iterator.successor()
    }

    return result
  }

  @inlinable
  @_disfavoredOverload
  public func mapValues<E>(
    _ transform: (Value) throws(E) -> Value
  ) throws(E) -> Self {
    return try mapValues(transform) as Self
  }

  @inlinable
  public func compactMapValues<R, E>(
    _ transform: (Value) throws(E) -> R.Value?
  ) throws(E) -> R where R: CxxDictionary, R.Key == Key {
    var result = R.init()

    var iterator = __beginUnsafe()
    let endIterator = __endUnsafe()

    while iterator != endIterator {
      let pair = iterator.pointee
      if let value = try transform(pair.second) {
          result.__insertUnsafe(R.Element(first: pair.first, second: value))
      }
      iterator = iterator.successor()
    }

    return result
  }

  @inlinable
  @_disfavoredOverload
  public func compactMapValues<E>(
    _ transform: (Value) throws(E) -> Value?
  ) throws(E) -> Self {
    return try compactMapValues(transform) as Self
  }

  public func filter(_ isIncluded: (_ key: Key, _ value: Value) throws -> Bool) rethrows -> Self {
    var filteredDictionary = Self.init()
    var iterator = __beginUnsafe()
    let endIterator = __endUnsafe()

    while iterator != endIterator {
      let pair = iterator.pointee

      if try isIncluded(pair.first, pair.second) {
        filteredDictionary.__insertUnsafe(pair)
      }

      iterator = iterator.successor()
    }

    return filteredDictionary
  }

  @inlinable
  @discardableResult
  public mutating func removeValue(forKey key: Key) -> Value? {
    let iter = self.__findMutatingUnsafe(key)
    guard iter != self.__endMutatingUnsafe() else { return nil }

    let value = iter.pointee.second
    self.__eraseUnsafe(iter)
    return value
  }

  @inlinable
  public mutating func merge<S: Sequence, E>(
    _ other: __owned S,
    uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) where S.Element == (Key, Value) {
    for (key, value) in other {
      var iter = self.__findMutatingUnsafe(key)
      if iter != self.__endMutatingUnsafe() {
        iter.pointee.second = try combine(iter.pointee.second, value)
      } else {
        let keyValuePair = Element(first: key, second: value)
        self.__insertUnsafe(keyValuePair)
      }
    }
  }

  @inlinable
  public mutating func merge<E>(
    _ other: __owned Dictionary<Key, Value>,
    uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) where Key: Hashable {
    for (key, value) in other {
      var iter = self.__findMutatingUnsafe(key)
      if iter != self.__endMutatingUnsafe() {
        iter.pointee.second = try combine(iter.pointee.second, value)
      } else {
        let keyValuePair = Element(first: key, second: value)
        self.__insertUnsafe(keyValuePair)
      }
    }
  }

  @inlinable
  public mutating func merge<T: CxxDictionary, E>(
    _ other: __owned T,
    uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) where T.Key == Key, T.Value == Value {
    var iterator = other.__beginUnsafe()
    while iterator != other.__endUnsafe() {
      var iter = self.__findMutatingUnsafe(iterator.pointee.first)
      if iter != self.__endMutatingUnsafe() {
        iter.pointee.second = try combine(iter.pointee.second, iterator.pointee.second)
      } else {
        let keyValuePair = Element(first: iterator.pointee.first, second: iterator.pointee.second)
        self.__insertUnsafe(keyValuePair)
      }
      iterator = iterator.successor()
    }
  }

  @inlinable
  public __consuming func merging<S: Sequence, E>(
    _ other: __owned S,
    uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) -> Self where S.Element == (Key, Value) {
    var result = self
    try result.merge(other, uniquingKeysWith: combine)
    return result
  }

  @inlinable
  public __consuming func merging<E>(
      _ other: __owned Dictionary<Key, Value>,
      uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) -> Self where Key: Hashable {
    var result = self
    try result.merge(other, uniquingKeysWith: combine)
    return result
  }

  @inlinable
  public __consuming func merging<T: CxxDictionary, E>(
    _ other: __owned T,
    uniquingKeysWith combine: (Value, Value) throws(E) -> Value
  ) throws(E) -> Self where T.Key == Key, T.Value == Value {
    var result = self
    try result.merge(other, uniquingKeysWith: combine)
    return result
  }
}
