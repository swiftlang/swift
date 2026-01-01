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

/// A C++ type that represents a set of values, which might be repeating.
///
/// C++ standard library types such as `std::set`, `std::unordered_set` and
/// `std::multiset` conform to this protocol.
///
/// - SeeAlso: `CxxUniqueSet`
public protocol CxxSet<Element>: ExpressibleByArrayLiteral {
  associatedtype Element
  associatedtype Size: BinaryInteger
  associatedtype RawIterator: UnsafeCxxInputIterator
    where RawIterator.Pointee == Element
  associatedtype RawMutableIterator: UnsafeCxxInputIterator
    where RawMutableIterator.Pointee == Element

  // std::pair<iterator, bool> for std::set and std::unordered_set
  // iterator for std::multiset
  associatedtype InsertionResult

  init()

  func __beginUnsafe() -> RawIterator
  func __endUnsafe() -> RawIterator
  func __findUnsafe(_ value: Element) -> RawIterator

  @discardableResult
  mutating func __insertUnsafe(_ element: Element) -> InsertionResult

  func count(_ element: Element) -> Size
  func size() -> Size
  func empty() -> CBool

  @discardableResult
  mutating func erase(_ key: Element) -> Size
}

extension CxxSet {
  @inlinable
  public var isEmpty: Bool {
    empty()
  }

  @inlinable
  public var count: Int {
    Int(size())
  }

  /// Creates a C++ set containing the elements of a Swift Sequence.
  ///
  /// This initializes the set by copying every element of the sequence.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the Swift
  ///   sequence
  @inlinable
  public init<S: Sequence>(_ sequence: __shared S) where S.Element == Element {
    self.init()
    for item in sequence {
      self.__insertUnsafe(item)
    }
  }

  @inlinable
  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }

  /// Returns a Boolean value that indicates whether the given element exists
  /// in the set.
  @inlinable
  public func contains(_ element: Element) -> Bool {
    return self.__findUnsafe(element) != self.__endUnsafe()
  }
}

/// A C++ type that represents a set of unique values.
///
/// C++ standard library types such as `std::set` and `std::unordered_set`
/// conform to this protocol.
public protocol CxxUniqueSet<Element>: CxxSet {
  override associatedtype InsertionResult
    where InsertionResult: CxxPair<RawMutableIterator, Bool>

  @discardableResult
  mutating func __eraseUnsafe(_ iter: RawIterator) -> RawMutableIterator
}

extension CxxUniqueSet {
  /// Inserts the given element in the set if it is not already present.
  ///
  /// If an element equal to `newMember` is already contained in the set, this
  /// method has no effect.
  ///
  /// - Parameter newMember: An element to insert into the set.
  /// - Returns: `(true, newMember)` if `newMember` was not contained in the
  ///   set. If an element equal to `newMember` was already contained in the
  ///   set, the method returns `(false, oldMember)`, where `oldMember` is the
  ///   element that was equal to `newMember`.
  @inlinable
  @discardableResult
  public mutating func insert(
    _ newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    let insertionResult = self.__insertUnsafe(newMember)
    let rawIterator: RawMutableIterator = insertionResult.first
    let inserted: Bool = insertionResult.second
    return (inserted, rawIterator.pointee)
  }

  /// Removes the given element from the set.
  ///
  /// - Parameter member: An element to remove from the set.
  @discardableResult
  @inlinable
  public mutating func remove(_ member: Element) -> Element? {
    let iter = self.__findUnsafe(member)
    guard iter != self.__endUnsafe() else {
      return nil
    }
    let value = iter.pointee
    self.__eraseUnsafe(iter)
    return value
  }

  @inlinable
  public func filter(_ isIncluded: (_ element: Element) throws -> Bool) rethrows -> Self {
    var filteredSet = Self.init()
    var iterator = self.__beginUnsafe()
    let endIterator = self.__endUnsafe()

    while iterator != endIterator {
      let element = iterator.pointee
      if try isIncluded(element) {
        filteredSet.__insertUnsafe(element)
      }
      iterator = iterator.successor()
    }
    return filteredSet
  }

  @inlinable
  public __consuming func union(_ other: Self) -> Self {
    var result = self
    result.formUnion(other)
    return result
  }

  @inlinable
  public __consuming func union<S: Sequence>(_ other: __owned S) -> Self
  where S.Element == Element {
    var result = self
    result.formUnion(other)
    return result
  }

  @inlinable
  public mutating func formUnion(_ other: __owned Self) {
    var iterator = other.__beginUnsafe()
    let endIterator = other.__endUnsafe()
    while iterator != endIterator {
      self.__insertUnsafe(iterator.pointee)
      iterator = iterator.successor()
    }
  }

  @inlinable
  public mutating func formUnion<S: Sequence>(_ other: __owned S)
  where S.Element == Element {
    for item in other {
      self.__insertUnsafe(item)
    }
  }

  @inlinable
  public __consuming func intersection(_ other: Self) -> Self {
    var result = Self()
    if self.size() <= other.size() {
      var iterator = self.__beginUnsafe()
      let endIterator = self.__endUnsafe()
      while iterator != endIterator {
        let element = iterator.pointee
        if other.contains(element) {
          result.__insertUnsafe(element)
        }
        iterator = iterator.successor()
      }
    } else {
      var iterator = other.__beginUnsafe()
      let endIterator = other.__endUnsafe()
      while iterator != endIterator {
        let element = iterator.pointee
        if self.contains(element) {
          result.__insertUnsafe(element)
        }
        iterator = iterator.successor()
      }
    }
    return result
  }

  @inlinable
  public __consuming func intersection<S: Sequence>(_ other: S) -> Self
  where S.Element == Element {
    var result = Self()
    for item in other where self.contains(item) {
      result.__insertUnsafe(item)
    }
    return result
  }

    @inlinable
  public mutating func formIntersection(_ other: Self) {
    self = self.intersection(other)
  }

  @inlinable
  public mutating func formIntersection<S: Sequence>(_ other: S)
  where S.Element == Element {
    self = self.intersection(other)
  }

    @inlinable
  public __consuming func subtracting(_ other: Self) -> Self {
    var result = self
    result.subtract(other)
    return result
  }

  @inlinable
  public __consuming func subtracting<S: Sequence>(_ other: S) -> Self
  where S.Element == Element {
    var result = self
    result.subtract(other)
    return result
  }

  @inlinable
  public mutating func subtract(_ other: Self) {
    var iterator = other.__beginUnsafe()
    let endIterator = other.__endUnsafe()
    while iterator != endIterator {
      let value = iterator.pointee
      self.erase(value)
      iterator = iterator.successor()
    }
  }

  @inlinable
  public mutating func subtract<S: Sequence>(_ other: S)
  where S.Element == Element {
    for item in other {
      self.erase(item)
    }
  }

  @inlinable
  public func isSubset(of other: Self) -> Bool {
    guard self.size() <= other.size() else { return false }

    var iterator = self.__beginUnsafe()
    let endIterator = self.__endUnsafe()

    while iterator != endIterator {
      guard other.contains(iterator.pointee) else {
        return false
      }
      iterator = iterator.successor()
    }
    return true
  }

  @inlinable
  public func isSubset<S: Sequence>(of other: S) -> Bool
  where S.Element == Element {
    guard !isEmpty else { return true }
    var seen = Self()
    var seenCount = 0

    for element in other {
      guard self.contains(element) else { continue }
      let inserted = seen.__insertUnsafe(element)
      if inserted.second {
        seenCount += 1
        if seenCount == self.size() {
          return true
        }
      }
    }
    return false
  }

  @inlinable
  public func isStrictSubset(of other: Self) -> Bool {
    return self.size() < other.size() && self.isSubset(of: other)
  }

  @inlinable
  public func isStrictSubset<S: Sequence>(of other: S) -> Bool
  where S.Element == Element {
    var seen = Self()
    var seenCount = 0
    var isStrict = false
    for element in other {
      guard self.contains(element) else {
        if !isStrict {
          isStrict = true
          if seenCount == self.size() { return true }
        }
        continue
      }
      let inserted = seen.__insertUnsafe(element)
      if inserted.second {
        seenCount += 1
        if seenCount == self.size(), isStrict {
          return true
        }
      }
    }
    return false
  }

  @inlinable
  public func isSuperset(of other: Self) -> Bool {
    return other.isSubset(of: self)
  }

  @inlinable
  public func isSuperset<S: Sequence>(of other: __owned S) -> Bool
  where S.Element == Element {
    return other.allSatisfy { self.contains($0) }
  }

  @inlinable
  public func isStrictSuperset(of other: Self) -> Bool {
    return self.size() > other.size() && other.isSubset(of: self)
  }

  @inlinable
  public func isStrictSuperset<S: Sequence>(of other: S) -> Bool
  where S.Element == Element {
    var seen = Self()
    var seenCount = 0
    for element in other {
      guard self.contains(element) else { return false }
      let inserted = seen.__insertUnsafe(element)
      if inserted.second {
        seenCount += 1
        if seenCount == self.size() {
          return false
        }
      }
    }

    return seenCount < self.size()
  }

  @inlinable
  public func isDisjoint(with other: Self) -> Bool {
    guard !self.isEmpty && !other.isEmpty else { return true }
    let (smaller, larger) =
      self.size() < other.size() ? (self, other) : (other, self)

    var iterator = smaller.__beginUnsafe()

    while iterator != smaller.__endUnsafe() {
      if larger.contains(iterator.pointee) {
        return false
      }
      iterator = iterator.successor()
    }
    return true
  }

  @inlinable
  public func isDisjoint<S: Sequence>(with other: S) -> Bool
  where S.Element == Self.Element {
    guard !self.isEmpty else { return true }

    return other.allSatisfy { !self.contains($0) }
  }

  @inlinable
  public __consuming func symmetricDifference(_ other: __owned Self) -> Self {
    var newSet = self
    newSet.formSymmetricDifference(other)
    return newSet
  }

  @inlinable
  public __consuming func symmetricDifference<S: Sequence>(
    _ other: __owned S
  ) -> Self
  where S.Element == Self.Element {
    var newSet = self
    newSet.formSymmetricDifference(other)
    return newSet
  }

  @inlinable
  public mutating func formSymmetricDifference(_ other: __owned Self) {
    if self.isEmpty {
      self.formUnion(other)
    } else {
      var iterator = other.__beginUnsafe()
      let endIterator = other.__endUnsafe()
      while iterator != endIterator {
        let member = iterator.pointee
        let seen = self.__findUnsafe(member)
        if seen != self.__endUnsafe() {
          self.__eraseUnsafe(seen)
        } else {
          self.__insertUnsafe(member)
        }

        iterator = iterator.successor()
      }
    }
  }

  @inlinable
  public mutating func formSymmetricDifference<S: Sequence>(
    _ other: __owned S
  ) where S.Element == Self.Element {
    if self.isEmpty {
      self.formUnion(other)
    } else {
      var seenElements = Self()
      for member in other where seenElements.insert(member).inserted {
        let seen = self.__findUnsafe(member)
        if seen != self.__endUnsafe() {
          self.__eraseUnsafe(seen)
        } else {
          self.__insertUnsafe(member)
        }
      }
    }
  }
}
