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

//===--- APIs unique to Set<Element> --------------------------------------===//

/// An unordered collection of unique elements.
///
/// You use a set instead of an array when you need to test efficiently for
/// membership and you aren't concerned with the order of the elements in the
/// collection, or when you need to ensure that each element appears only once
/// in a collection.
///
/// You can create a set with any element type that conforms to the `Hashable`
/// protocol. By default, most types in the standard library are hashable,
/// including strings, numeric and Boolean types, enumeration cases without
/// associated values, and even sets themselves.
///
/// Swift makes it as easy to create a new set as to create a new array. Simply
/// assign an array literal to a variable or constant with the `Set` type
/// specified.
///
///     let ingredients: Set = ["cocoa beans", "sugar", "cocoa butter", "salt"]
///     if ingredients.contains("sugar") {
///         print("No thanks, too sweet.")
///     }
///     // Prints "No thanks, too sweet."
///
/// Set Operations
/// ==============
///
/// Sets provide a suite of mathematical set operations. For example, you can
/// efficiently test a set for membership of an element or check its
/// intersection with another set:
///
/// - Use the `contains(_:)` method to test whether a set contains a specific
///   element.
/// - Use the "equal to" operator (`==`) to test whether two sets contain the
///   same elements.
/// - Use the `isSubset(of:)` method to test whether a set contains all the
///   elements of another set or sequence.
/// - Use the `isSuperset(of:)` method to test whether all elements of a set
///   are contained in another set or sequence.
/// - Use the `isStrictSubset(of:)` and `isStrictSuperset(of:)` methods to test
///   whether a set is a subset or superset of, but not equal to, another set.
/// - Use the `isDisjoint(with:)` method to test whether a set has any elements
///   in common with another set.
///
/// You can also combine, exclude, or subtract the elements of two sets:
///
/// - Use the `union(_:)` method to create a new set with the elements of a set
///   and another set or sequence.
/// - Use the `intersection(_:)` method to create a new set with only the
///   elements common to a set and another set or sequence.
/// - Use the `symmetricDifference(_:)` method to create a new set with the
///   elements that are in either a set or another set or sequence, but not in
///   both.
/// - Use the `subtracting(_:)` method to create a new set with the elements of
///   a set that are not also in another set or sequence.
///
/// You can modify a set in place by using these methods' mutating
/// counterparts: `formUnion(_:)`, `formIntersection(_:)`,
/// `formSymmetricDifference(_:)`, and `subtract(_:)`.
///
/// Set operations are not limited to use with other sets. Instead, you can
/// perform set operations with another set, an array, or any other sequence
/// type.
///
///     var primes: Set = [2, 3, 5, 7]
///
///     // Tests whether primes is a subset of a Range<Int>
///     print(primes.isSubset(of: 0..<10))
///     // Prints "true"
///
///     // Performs an intersection with an Array<Int>
///     let favoriteNumbers = [5, 7, 15, 21]
///     print(primes.intersection(favoriteNumbers))
///     // Prints "[5, 7]"
///
/// Sequence and Collection Operations
/// ==================================
///
/// In addition to the `Set` type's set operations, you can use any nonmutating
/// sequence or collection methods with a set.
///
///     if primes.isEmpty {
///         print("No primes!")
///     } else {
///         print("We have \(primes.count) primes.")
///     }
///     // Prints "We have 4 primes."
///
///     let primesSum = primes.reduce(0, +)
///     // 'primesSum' == 17
///
///     let primeStrings = primes.sorted().map(String.init)
///     // 'primeStrings' == ["2", "3", "5", "7"]
///
/// You can iterate through a set's unordered elements with a `for`-`in` loop.
///
///     for number in primes {
///         print(number)
///     }
///     // Prints "5"
///     // Prints "7"
///     // Prints "2"
///     // Prints "3"
///
/// Many sequence and collection operations return an array or a type-erasing
/// collection wrapper instead of a set. To restore efficient set operations,
/// create a new set from the result.
///
///     let morePrimes = primes.union([11, 13, 17, 19])
///
///     let laterPrimes = morePrimes.filter { $0 > 10 }
///     // 'laterPrimes' is of type Array<Int>
///
///     let laterPrimesSet = Set(morePrimes.filter { $0 > 10 })
///     // 'laterPrimesSet' is of type Set<Int>
///
/// Bridging Between Set and NSSet
/// ==============================
///
/// You can bridge between `Set` and `NSSet` using the `as` operator. For
/// bridging to be possible, the `Element` type of a set must be a class, an
/// `@objc` protocol (a protocol imported from Objective-C or marked with the
/// `@objc` attribute), or a type that bridges to a Foundation type.
///
/// Bridging from `Set` to `NSSet` always takes O(1) time and space. When the
/// set's `Element` type is neither a class nor an `@objc` protocol, any
/// required bridging of elements occurs at the first access of each element,
/// so the first operation that uses the contents of the set (for example, a
/// membership test) can take O(*n*).
///
/// Bridging from `NSSet` to `Set` first calls the `copy(with:)` method
/// (`- copyWithZone:` in Objective-C) on the set to get an immutable copy and
/// then performs additional Swift bookkeeping work that takes O(1) time. For
/// instances of `NSSet` that are already immutable, `copy(with:)` returns the
/// same set in constant time; otherwise, the copying performance is
/// unspecified. The instances of `NSSet` and `Set` share buffer using the
/// same copy-on-write optimization that is used when two instances of `Set`
/// share buffer.
@_fixed_layout
public struct Set<Element: Hashable> {
  @usableFromInline
  internal var _variant: _Variant
}

extension Set {
  /// Creates an empty set with preallocated space for at least the specified
  /// number of elements.
  ///
  /// Use this initializer to avoid intermediate reallocations of a set's
  /// storage buffer when you know how many elements you'll insert into the set
  /// after creation.
  ///
  /// - Parameter minimumCapacity: The minimum number of elements that the
  ///   newly created set should be able to store without reallocating its
  ///   storage buffer.
  @inlinable
  public init(minimumCapacity: Int) {
    _variant = .native(_NativeSet(capacity: minimumCapacity))
  }

  /// Private initializer.
  @inlinable
  internal init(_native: _NativeSet<Element>) {
    _variant = .native(_native)
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_cocoa: _CocoaSet) {
    _variant = .cocoa(_cocoa)
  }
#endif

  //
  // All APIs below should dispatch to `_variant`, without doing any
  // additional processing.
  //

#if _runtime(_ObjC)
  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  ///
  /// * it is statically known that the given `NSSet` is immutable;
  /// * `Element` is bridged verbatim to Objective-C (i.e.,
  ///   is a reference type).
  @inlinable
  public // SPI(Foundation)
  init(_immutableCocoaSet: _NSSet) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self),
      "Set can be backed by NSSet only when the member type can be bridged verbatim to Objective-C")
    self.init(_cocoa: _CocoaSet(_immutableCocoaSet))
  }
#endif
}

extension Set: ExpressibleByArrayLiteral {
  //
  // `ExpressibleByArrayLiteral` conformance
  //
  /// Creates a set containing the elements of the given array literal.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you use an array literal. Instead, create a new set using an array
  /// literal as its value by enclosing a comma-separated list of values in
  /// square brackets. You can use an array literal anywhere a set is expected
  /// by the type context.
  ///
  /// Here, a set of strings is created from an array literal holding only
  /// strings.
  ///
  ///     let ingredients: Set = ["cocoa beans", "sugar", "cocoa butter", "salt"]
  ///     if ingredients.isSuperset(of: ["sugar", "salt"]) {
  ///         print("Whatever it is, it's bound to be delicious!")
  ///     }
  ///     // Prints "Whatever it is, it's bound to be delicious!"
  ///
  /// - Parameter elements: A variadic list of elements of the new set.
  @inlinable
  public init(arrayLiteral elements: Element...) {
    if elements.isEmpty {
      self.init()
      return
    }
    var native = _NativeSet<Element>(capacity: elements.count)
    for element in elements {
      let inserted = native._insert(element, isUnique: true).inserted
      if !inserted {
        // FIXME: Shouldn't this trap?
      }
    }
    self.init(_native: native)
  }
}

extension Set: Sequence {
  /// Returns an iterator over the members of the set.
  @inlinable
  @inline(__always)
  public func makeIterator() -> Iterator {
    return _variant.makeIterator()
  }

  /// Returns a Boolean value that indicates whether the given element exists
  /// in the set.
  ///
  /// This example uses the `contains(_:)` method to test whether an integer is
  /// a member of a set of prime numbers.
  ///
  ///     let primes: Set = [2, 3, 5, 7]
  ///     let x = 5
  ///     if primes.contains(x) {
  ///         print("\(x) is prime!")
  ///     } else {
  ///         print("\(x). Not prime.")
  ///     }
  ///     // Prints "5 is prime!"
  ///
  /// - Parameter member: An element to look for in the set.
  /// - Returns: `true` if `member` exists in the set; otherwise, `false`.
  ///
  /// - Complexity: O(1)
  @inlinable
  public func contains(_ member: Element) -> Bool {
    return _variant.contains(member)
  }

  @inlinable
  public func _customContainsEquatableElement(_ member: Element) -> Bool? {
    return contains(member)
  }
}

// This is not quite Sequence.filter, because that returns [Element], not Self
// (RangeReplaceableCollection.filter returns Self, but Set isn't an RRC)
extension Set {
  /// Returns a new set containing the elements of the set that satisfy the
  /// given predicate.
  ///
  /// In this example, `filter(_:)` is used to include only names shorter than
  /// five characters.
  ///
  ///     let cast: Set = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let shortNames = cast.filter { $0.count < 5 }
  ///
  ///     shortNames.isSubset(of: cast)
  ///     // true
  ///     shortNames.contains("Vivien")
  ///     // false
  ///
  /// - Parameter isIncluded: A closure that takes an element as its argument
  ///   and returns a Boolean value indicating whether the element should be
  ///   included in the returned set.
  /// - Returns: A set of the elements that `isIncluded` allows.
  @inlinable
  @available(swift, introduced: 4.0)
  public func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> Set {
    var result = _NativeSet<Element>()
    for element in self {
      if try isIncluded(element) {
        result.insertNew(element)
      }
    }
    return Set(_native: result)
  }
}

extension Set: Collection {
  /// The starting position for iterating members of the set.
  ///
  /// If the set is empty, `startIndex` is equal to `endIndex`.
  @inlinable
  public var startIndex: Index {
    return _variant.startIndex
  }

  /// The "past the end" position for the set---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the set is empty, `endIndex` is equal to `startIndex`.
  @inlinable
  public var endIndex: Index {
    return _variant.endIndex
  }

  /// Accesses the member at the given position.
  @inlinable
  public subscript(position: Index) -> Element {
    return _variant.element(at: position)
  }

  @inlinable
  public func index(after i: Index) -> Index {
    return _variant.index(after: i)
  }

  // APINAMING: complexity docs are broadly missing in this file.

  /// Returns the index of the given element in the set, or `nil` if the
  /// element is not a member of the set.
  ///
  /// - Parameter member: An element to search for in the set.
  /// - Returns: The index of `member` if it exists in the set; otherwise,
  ///   `nil`.
  ///
  /// - Complexity: O(1)
  @inlinable
  public func firstIndex(of member: Element) -> Index? {
    return _variant.index(for: member)
  }

  @inlinable
  public func _customIndexOfEquatableElement(
     _ member: Element
    ) -> Index?? {
    return Optional(firstIndex(of: member))
  }

  @inlinable
  public func _customLastIndexOfEquatableElement(
     _ member: Element
    ) -> Index?? {
    // The first and last elements are the same because each element is unique.
    return _customIndexOfEquatableElement(member)
  }

  /// The number of elements in the set.
  ///
  /// - Complexity: O(1).
  @inlinable
  public var count: Int {
    return _variant.count
  }

  /// A Boolean value that indicates whether the set is empty.
  @inlinable
  public var isEmpty: Bool {
    return count == 0
  }

  /// The first element of the set.
  ///
  /// The first element of the set is not necessarily the first element added
  /// to the set. Don't expect any particular ordering of set elements.
  ///
  /// If the set is empty, the value of this property is `nil`.
  @inlinable
  public var first: Element? {
    // FIXME: It'd better to use an iterator than to subscript with startIndex,
    // because startIndex is currently O(n) in bridged sets. However,
    // enumerators aren't guaranteed to have the same element order as allKeys.
    return count > 0 ? self[startIndex] : nil
  }
}

// FIXME: rdar://problem/23549059 (Optimize == for Set)
// Look into initially trying to compare the two sets by directly comparing the
// contents of both buffers in order. If they happen to have the exact same
// ordering we can get the `true` response without ever hashing. If the two
// buffers' contents differ at all then we have to fall back to hashing the
// rest of the elements (but we don't need to hash any prefix that did match).
extension Set: Equatable {
  /// Returns a Boolean value indicating whether two sets have equal elements.
  ///
  /// - Parameters:
  ///   - lhs: A set.
  ///   - rhs: Another set.
  /// - Returns: `true` if the `lhs` and `rhs` have the same elements; otherwise,
  ///   `false`.
  @inlinable
  public static func == (lhs: Set<Element>, rhs: Set<Element>) -> Bool {
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):

      if lhsNative._storage === rhsNative._storage {
        return true
      }

      if lhsNative.count != rhsNative.count {
        return false
      }

      for member in lhsNative {
        guard rhsNative.find(member).found else {
          return false
        }
      }
      return true

  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa

    case (.native(let lhsNative), .cocoa(let rhsCocoa)):
      if lhsNative.count != rhsCocoa.count {
        return false
      }

      for i in lhsNative.indices {
        let key = lhsNative.element(at: i)
        let bridgedKey = _bridgeAnythingToObjectiveC(key)
        if rhsCocoa.contains(bridgedKey) {
          continue
        }
        return false
      }
      return true

    case (.cocoa, .native):
      return rhs == lhs
  #endif
    }
  }
}

extension Set: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    // FIXME: <rdar://problem/18915294> Cache Set<T> hashValue
    var hash = 0
    let seed = hasher._generateSeed()
    for member in self {
      hash ^= member._rawHashValue(seed: seed)
    }
    hasher.combine(hash)
  }
}

extension Set: _HasCustomAnyHashableRepresentation {
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _SetAnyHashableBox(self))
  }
}

internal struct _SetAnyHashableBox<Element: Hashable>: _AnyHashableBox {
  internal let _value: Set<Element>
  internal let _canonical: Set<AnyHashable>

  internal init(_ value: Set<Element>) {
    self._value = value
    self._canonical = value as Set<AnyHashable>
  }

  internal var _base: Any {
    return _value
  }

  internal var _canonicalBox: _AnyHashableBox {
    return _SetAnyHashableBox<AnyHashable>(_canonical)
  }

  internal func _isEqual(to other: _AnyHashableBox) -> Bool? {
    guard let other = other as? _SetAnyHashableBox<AnyHashable> else {
      return nil
    }
    return _canonical == other._value
  }

  internal var _hashValue: Int {
    return _canonical.hashValue
  }

  internal func _hash(into hasher: inout Hasher) {
    _canonical.hash(into: &hasher)
  }

  func _rawHashValue(_seed: (UInt64, UInt64)) -> Int {
    return _canonical._rawHashValue(seed: _seed)
  }

  internal func _unbox<T: Hashable>() -> T? {
    return _value as? T
  }

  internal func _downCastConditional<T>(
    into result: UnsafeMutablePointer<T>
  ) -> Bool {
    guard let value = _value as? T else { return false }
    result.initialize(to: value)
    return true
  }
}

extension Set: SetAlgebra {

  /// Inserts the given element in the set if it is not already present.
  ///
  /// If an element equal to `newMember` is already contained in the set, this
  /// method has no effect. In the following example, a new element is
  /// inserted into `classDays`, a set of days of the week. When an existing
  /// element is inserted, the `classDays` set does not change.
  ///
  ///     enum DayOfTheWeek: Int {
  ///         case sunday, monday, tuesday, wednesday, thursday,
  ///             friday, saturday
  ///     }
  ///
  ///     var classDays: Set<DayOfTheWeek> = [.wednesday, .friday]
  ///     print(classDays.insert(.monday))
  ///     // Prints "(true, .monday)"
  ///     print(classDays)
  ///     // Prints "[.friday, .wednesday, .monday]"
  ///
  ///     print(classDays.insert(.friday))
  ///     // Prints "(false, .friday)"
  ///     print(classDays)
  ///     // Prints "[.friday, .wednesday, .monday]"
  ///
  /// - Parameter newMember: An element to insert into the set.
  /// - Returns: `(true, newMember)` if `newMember` was not contained in the
  ///   set. If an element equal to `newMember` was already contained in the
  ///   set, the method returns `(false, oldMember)`, where `oldMember` is the
  ///   element that was equal to `newMember`. In some cases, `oldMember` may
  ///   be distinguishable from `newMember` by identity comparison or some
  ///   other means.
  @inlinable
  @discardableResult
  public mutating func insert(
    _ newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    return _variant.insert(newMember)
  }

  /// Inserts the given element into the set unconditionally.
  ///
  /// If an element equal to `newMember` is already contained in the set,
  /// `newMember` replaces the existing element. In this example, an existing
  /// element is inserted into `classDays`, a set of days of the week.
  ///
  ///     enum DayOfTheWeek: Int {
  ///         case sunday, monday, tuesday, wednesday, thursday,
  ///             friday, saturday
  ///     }
  ///
  ///     var classDays: Set<DayOfTheWeek> = [.monday, .wednesday, .friday]
  ///     print(classDays.update(with: .monday))
  ///     // Prints "Optional(.monday)"
  ///
  /// - Parameter newMember: An element to insert into the set.
  /// - Returns: An element equal to `newMember` if the set already contained
  ///   such a member; otherwise, `nil`. In some cases, the returned element
  ///   may be distinguishable from `newMember` by identity comparison or some
  ///   other means.
  @inlinable
  @discardableResult
  public mutating func update(with newMember: Element) -> Element? {
    return _variant.update(with: newMember)
  }

  /// Removes the specified element from the set.
  ///
  /// This example removes the element `"sugar"` from a set of ingredients.
  ///
  ///     var ingredients: Set = ["cocoa beans", "sugar", "cocoa butter", "salt"]
  ///     let toRemove = "sugar"
  ///     if let removed = ingredients.remove(toRemove) {
  ///         print("The recipe is now \(removed)-free.")
  ///     }
  ///     // Prints "The recipe is now sugar-free."
  ///
  /// - Parameter member: The element to remove from the set.
  /// - Returns: The value of the `member` parameter if it was a member of the
  ///   set; otherwise, `nil`.
  @inlinable
  @discardableResult
  public mutating func remove(_ member: Element) -> Element? {
    return _variant.remove(member)
  }

  /// Removes the element at the given index of the set.
  ///
  /// - Parameter position: The index of the member to remove. `position` must
  ///   be a valid index of the set, and must not be equal to the set's end
  ///   index.
  /// - Returns: The element that was removed from the set.
  @inlinable
  @discardableResult
  public mutating func remove(at position: Index) -> Element {
    return _variant.remove(at: position)
  }

  /// Removes all members from the set.
  ///
  /// - Parameter keepingCapacity: If `true`, the set's buffer capacity is
  ///   preserved; if `false`, the underlying buffer is released. The
  ///   default is `false`.
  @inlinable
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _variant.removeAll(keepingCapacity: keepCapacity)
  }

  /// Removes the first element of the set.
  ///
  /// Because a set is not an ordered collection, the "first" element may not
  /// be the first element that was added to the set. The set must not be
  /// empty.
  ///
  /// - Complexity: Amortized O(1) if the set does not wrap a bridged `NSSet`.
  ///   If the set wraps a bridged `NSSet`, the performance is unspecified.
  ///
  /// - Returns: A member of the set.
  @inlinable
  @discardableResult
  public mutating func removeFirst() -> Element {
    _precondition(!isEmpty, "Can't removeFirst from an empty Set")
    return remove(at: startIndex)
  }

  //
  // APIs below this comment should be implemented strictly in terms of
  // *public* APIs above.  `_variant` should not be accessed directly.
  //
  // This separates concerns for testing.  Tests for the following APIs need
  // not to concern themselves with testing correctness of behavior of
  // underlying buffer (and different variants of it), only correctness of the
  // API itself.
  //

  /// Creates an empty set.
  ///
  /// This is equivalent to initializing with an empty array literal. For
  /// example:
  ///
  ///     var emptySet = Set<Int>()
  ///     print(emptySet.isEmpty)
  ///     // Prints "true"
  ///
  ///     emptySet = []
  ///     print(emptySet.isEmpty)
  ///     // Prints "true"
  @inlinable
  public init() {
    self = Set<Element>(_native: _NativeSet())
  }

  /// Creates a new set from a finite sequence of items.
  ///
  /// Use this initializer to create a new set from an existing sequence, for
  /// example, an array or a range.
  ///
  ///     let validIndices = Set(0..<7).subtracting([2, 4, 5])
  ///     print(validIndices)
  ///     // Prints "[6, 0, 1, 3]"
  ///
  /// This initializer can also be used to restore set methods after performing
  /// sequence operations such as `filter(_:)` or `map(_:)` on a set. For
  /// example, after filtering a set of prime numbers to remove any below 10,
  /// you can create a new set by using this initializer.
  ///
  ///     let primes: Set = [2, 3, 5, 7, 11, 13, 17, 19, 23]
  ///     let laterPrimes = Set(primes.lazy.filter { $0 > 10 })
  ///     print(laterPrimes)
  ///     // Prints "[17, 19, 23, 11, 13]"
  ///
  /// - Parameter sequence: The elements to use as members of the new set.
  @inlinable
  public init<Source: Sequence>(_ sequence: Source)
    where Source.Element == Element {
    if let s = sequence as? Set<Element> {
      // If this sequence is actually a native `Set`, then we can quickly
      // adopt its native buffer and let COW handle uniquing only
      // if necessary.
      self._variant = s._variant
      return
    }
    var native = _NativeSet<Element>(capacity: sequence.underestimatedCount)
    for element in sequence {
      native._insert(element, isUnique: true)
    }
    self._variant = .native(native)
  }

  /// Returns a Boolean value that indicates whether the set is a subset of the
  /// given sequence.
  ///
  /// Set *A* is a subset of another set *B* if every member of *A* is also a
  /// member of *B*.
  ///
  ///     let employees = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isSubset(of: employees))
  ///     // Prints "true"
  ///
  /// - Parameter possibleSuperset: A sequence of elements. `possibleSuperset`
  ///   must be finite.
  /// - Returns: `true` if the set is a subset of `possibleSuperset`;
  ///   otherwise, `false`.
  @inlinable
  public func isSubset<S: Sequence>(of possibleSuperset: S) -> Bool
    where S.Element == Element {
    if self.isEmpty { return true }
    if self.count == 1 { return possibleSuperset.contains(self.first!) }
    if let s = possibleSuperset as? Set<Element> {
      return isSubset(of: s)
    }
    switch self._variant {
    case .native(let native):
      return native.isSubset(of: possibleSuperset)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return _NativeSet<Element>(cocoa).isSubset(of: possibleSuperset)
#endif
    }
  }

  /// Returns a Boolean value that indicates whether the set is a strict subset
  /// of the given sequence.
  ///
  /// Set *A* is a strict subset of another set *B* if every member of *A* is
  /// also a member of *B* and *B* contains at least one element that is not a
  /// member of *A*.
  ///
  ///     let employees = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isStrictSubset(of: employees))
  ///     // Prints "true"
  ///
  ///     // A set is never a strict subset of itself:
  ///     print(attendees.isStrictSubset(of: attendees))
  ///     // Prints "false"
  ///
  /// - Parameter possibleStrictSuperset: A sequence of elements.
  ///   `possibleStrictSuperset` must be finite.
  /// - Returns: `true` is the set is strict subset of
  ///   `possibleStrictSuperset`; otherwise, `false`.
  @inlinable
  public func isStrictSubset<S: Sequence>(of possibleStrictSuperset: S) -> Bool
    where S.Element == Element {
    if let s = possibleStrictSuperset as? Set<Element> {
      return isStrictSubset(of: s)
    }
    switch self._variant {
    case .native(let native):
      return native.isStrictSubset(of: possibleStrictSuperset)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return _NativeSet<Element>(cocoa)
        .isStrictSubset(of: possibleStrictSuperset)
#endif
    }
  }

  /// Returns a Boolean value that indicates whether the set is a superset of
  /// the given sequence.
  ///
  /// Set *A* is a superset of another set *B* if every member of *B* is also a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isSuperset(of: attendees))
  ///     // Prints "true"
  ///
  /// - Parameter possibleSubset: A sequence of elements. `possibleSubset` must
  ///   be finite.
  /// - Returns: `true` if the set is a superset of `possibleSubset`;
  ///   otherwise, `false`.
  @inlinable
  public func isSuperset<S: Sequence>(of possibleSubset: S) -> Bool
    where S.Element == Element {
    if possibleSubset.underestimatedCount > self.count {
      return false
    }
    if let s = possibleSubset as? Set<Element> {
      return isSuperset(of: s)
    }
    for element in possibleSubset {
      guard self.contains(element) else { return false }
    }
    return true
  }

  /// Returns a Boolean value that indicates whether the set is a strict
  /// superset of the given sequence.
  ///
  /// Set *A* is a strict superset of another set *B* if every member of *B* is
  /// also a member of *A* and *A* contains at least one element that is *not*
  /// a member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isStrictSuperset(of: attendees))
  ///     // Prints "true"
  ///     print(employees.isStrictSuperset(of: employees))
  ///     // Prints "false"
  ///
  /// - Parameter possibleStrictSubset: A sequence of elements.
  ///   `possibleStrictSubset` must be finite.
  /// - Returns: `true` if the set is a strict superset of
  ///   `possibleStrictSubset`; otherwise, `false`.
  @inlinable
  public func isStrictSuperset<S: Sequence>(of possibleStrictSubset: S) -> Bool
    where S.Element == Element {
    if isEmpty { return false }
    if let s = possibleStrictSubset as? Set<Element> {
      return isStrictSuperset(of: s)
    }
    guard case .native(let native) = self._variant else {
      return isStrictSuperset(of: Set(possibleStrictSubset))
    }
    return native.isStrictSuperset(of: possibleStrictSubset)
  }

  /// Returns a Boolean value that indicates whether the set has no members in
  /// common with the given sequence.
  ///
  /// In the following example, the `employees` set is disjoint with the
  /// elements of the `visitors` array because no name appears in both.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let visitors = ["Marcia", "Nathaniel", "Olivia"]
  ///     print(employees.isDisjoint(with: visitors))
  ///     // Prints "true"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  /// - Returns: `true` if the set has no elements in common with `other`;
  ///   otherwise, `false`.
  @inlinable
  public func isDisjoint<S: Sequence>(with other: S) -> Bool
    where S.Element == Element {
    return _isDisjoint(with: other)
  }

  @inlinable
  internal func _isDisjoint<S: Sequence>(with other: S) -> Bool
    where S.Element == Element {
    for element in other {
      if self.contains(element) { return false }
    }
    return true
  }

  /// Returns a new set with the elements of both this set and the given
  /// sequence.
  ///
  /// In the following example, the `attendeesAndVisitors` set is made up
  /// of the elements of the `attendees` set and the `visitors` array:
  ///
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     let visitors = ["Marcia", "Nathaniel"]
  ///     let attendeesAndVisitors = attendees.union(visitors)
  ///     print(attendeesAndVisitors)
  ///     // Prints "["Diana", "Nathaniel", "Bethany", "Alicia", "Marcia"]"
  ///
  /// If the set already contains one or more elements that are also in
  /// `other`, the existing members are kept. If `other` contains multiple
  /// instances of equivalent elements, only the first instance is kept.
  ///
  ///     let initialIndices = Set(0..<5)
  ///     let expandedIndices = initialIndices.union([2, 3, 6, 6, 7, 7])
  ///     print(expandedIndices)
  ///     // Prints "[2, 4, 6, 7, 0, 1, 3]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  /// - Returns: A new set with the unique elements of this set and `other`.
  @inlinable
  public func union<S: Sequence>(_ other: S) -> Set<Element>
    where S.Element == Element {
    var newSet = self
    newSet.formUnion(other)
    return newSet
  }

  /// Inserts the elements of the given sequence into the set.
  ///
  /// If the set already contains one or more elements that are also in
  /// `other`, the existing members are kept. If `other` contains multiple
  /// instances of equivalent elements, only the first instance is kept.
  ///
  ///     var attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     let visitors = ["Diana", "Marcia", "Nathaniel"]
  ///     attendees.formUnion(visitors)
  ///     print(attendees)
  ///     // Prints "["Diana", "Nathaniel", "Bethany", "Alicia", "Marcia"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  @inlinable
  public mutating func formUnion<S: Sequence>(_ other: S)
    where S.Element == Element {
    _variant.formUnion(other)
  }

  /// Returns a new set containing the elements of this set that do not occur
  /// in the given sequence.
  ///
  /// In the following example, the `nonNeighbors` set is made up of the
  /// elements of the `employees` set that are not elements of `neighbors`:
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let nonNeighbors = employees.subtracting(neighbors)
  ///     print(nonNeighbors)
  ///     // Prints "["Chris", "Diana", "Alicia"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  /// - Returns: A new set.
  @inlinable
  public func subtracting<S: Sequence>(_ other: S) -> Set<Element>
    where S.Element == Element {
    return Set(_native: _variant.subtracting(other))
  }

  /// Removes the elements of the given sequence from the set.
  ///
  /// In the following example, the elements of the `employees` set that are
  /// also elements of the `neighbors` array are removed. In particular, the
  /// names `"Bethany"` and `"Eric"` are removed from `employees`.
  ///
  ///     var employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     employees.subtract(neighbors)
  ///     print(employees)
  ///     // Prints "["Chris", "Diana", "Alicia"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  @inlinable
  public mutating func subtract<S: Sequence>(_ other: S)
    where S.Element == Element {
    _subtract(other)
  }

  @inlinable
  internal mutating func _subtract<S: Sequence>(_ other: S)
  where S.Element == Element {
    guard _variant.isUniquelyReferenced() else {
      self = Set(_native: _variant.subtracting(other))
      return
    }
    for item in other {
      remove(item)
    }
  }

  /// Returns a new set with the elements that are common to both this set and
  /// the given sequence.
  ///
  /// In the following example, the `bothNeighborsAndEmployees` set is made up
  /// of the elements that are in *both* the `employees` and `neighbors` sets.
  /// Elements that are in only one or the other are left out of the result of
  /// the intersection.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let bothNeighborsAndEmployees = employees.intersection(neighbors)
  ///     print(bothNeighborsAndEmployees)
  ///     // Prints "["Bethany", "Eric"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  /// - Returns: A new set.
  @inlinable
  public func intersection<S: Sequence>(_ other: S) -> Set<Element>
    where S.Element == Element {
    return Set(_native: _variant.intersection(other))
  }

  /// Removes the elements of the set that aren't also in the given sequence.
  ///
  /// In the following example, the elements of the `employees` set that are
  /// not also members of the `neighbors` set are removed. In particular, the
  /// names `"Alicia"`, `"Chris"`, and `"Diana"` are removed.
  ///
  ///     var employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     employees.formIntersection(neighbors)
  ///     print(employees)
  ///     // Prints "["Bethany", "Eric"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  @inlinable
  public mutating func formIntersection<S: Sequence>(_ other: S)
    where S.Element == Element {
    self = self.intersection(other)
  }

  /// Returns a new set with the elements that are either in this set or in the
  /// given sequence, but not in both.
  ///
  /// In the following example, the `eitherNeighborsOrEmployees` set is made up
  /// of the elements of the `employees` and `neighbors` sets that are not in
  /// both `employees` *and* `neighbors`. In particular, the names `"Bethany"`
  /// and `"Eric"` do not appear in `eitherNeighborsOrEmployees`.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani"]
  ///     let eitherNeighborsOrEmployees = employees.symmetricDifference(neighbors)
  ///     print(eitherNeighborsOrEmployees)
  ///     // Prints "["Diana", "Forlani", "Alicia"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  /// - Returns: A new set.
  @inlinable
  public func symmetricDifference<S: Sequence>(_ other: S) -> Set<Element>
    where S.Element == Element {
    if let other = other as? Set<Element> {
      return Set(_native: _variant.symmetricDifference(other))
    }
    return Set(_native: _variant.symmetricDifference(other))
  }

  /// Replace this set with the elements contained in this set or the given
  /// set, but not both.
  ///
  /// In the following example, the elements of the `employees` set that are
  /// also members of `neighbors` are removed from `employees`, while the
  /// elements of `neighbors` that are not members of `employees` are added to
  /// `employees`. In particular, the names `"Bethany"` and `"Eric"` are
  /// removed from `employees` while the name `"Forlani"` is added.
  ///
  ///     var employees: Set = ["Alicia", "Bethany", "Diana", "Eric"]
  ///     let neighbors = ["Bethany", "Eric", "Forlani"]
  ///     employees.formSymmetricDifference(neighbors)
  ///     print(employees)
  ///     // Prints "["Diana", "Forlani", "Alicia"]"
  ///
  /// - Parameter other: A sequence of elements. `other` must be finite.
  @inlinable
  public mutating func formSymmetricDifference<S: Sequence>(_ other: S)
    where S.Element == Element {
    self = self.symmetricDifference(other)
  }
}

extension Set: CustomStringConvertible, CustomDebugStringConvertible {
  /// A string that represents the contents of the set.
  public var description: String {
    return _makeCollectionDescription(for: self, withTypeName: nil)
  }

  /// A string that represents the contents of the set, suitable for debugging.
  public var debugDescription: String {
    return _makeCollectionDescription(for: self, withTypeName: "Set")
  }
}

#if _runtime(_ObjC)
@_silgen_name("swift_stdlib_CFSetGetValues")
@usableFromInline
internal
func _stdlib_CFSetGetValues(_ nss: _NSSet, _: UnsafeMutablePointer<AnyObject>)

/// Equivalent to `NSSet.allObjects`, but does not leave objects on the
/// autorelease pool.
@inlinable
internal func _stdlib_NSSet_allObjects(
  _ nss: _NSSet
) -> _HeapBuffer<Int, AnyObject> {
  let count = nss.count
  let storage = _HeapBuffer<Int, AnyObject>(
    _HeapBufferStorage<Int, AnyObject>.self, count, count)
  _stdlib_CFSetGetValues(nss, storage.baseAddress)
  return storage
}
#endif

//===--- Compiler conversion/casting entry points for Set<Element> --------===//

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseValue` is a base class or base `@objc`
///   protocol (such as `AnyObject`) of `DerivedValue`.
@inlinable
public func _setUpCast<DerivedValue, BaseValue>(_ source: Set<DerivedValue>)
  -> Set<BaseValue> {
  var builder = _SetBuilder<BaseValue>(count: source.count)
  for x in source {
    builder.add(member: x as! BaseValue)
  }
  return builder.take()
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastIndirect")
internal func _setDownCastIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>) {
  target.initialize(to: _setDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
public func _setDownCast<BaseValue, DerivedValue>(_ source: Set<BaseValue>)
  -> Set<DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedValue.self) {
    switch source._variant {
    case .native(let nativeSet):
      return Set(_cocoa: _CocoaSet(nativeSet.bridged()))
    case .cocoa(let cocoaSet):
      return Set(_cocoa: cocoaSet)
    }
  }
#endif
  return _setDownCastConditional(source)!
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastConditionalIndirect")
internal func _setDownCastConditionalIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>
) -> Bool {
  if let result: Set<TargetValue> = _setDownCastConditional(source.pointee) {
    target.initialize(to: result)
    return true
  }
  return false
}

/// Implements a conditional downcast.
///
/// If the cast fails, the function returns `nil`.  All checks should be
/// performed eagerly.
///
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
public func _setDownCastConditional<BaseValue, DerivedValue>(
  _ source: Set<BaseValue>
) -> Set<DerivedValue>? {
  var result = _NativeSet<DerivedValue>(capacity: source.count)
  for member in source {
    guard let derivedMember = member as? DerivedValue else {
      return nil
    }
    result.insertNew(derivedMember)
  }
  return Set(_native: result)
}

extension Set {
  /// Removes the elements of the given set from this set.
  ///
  /// In the following example, the elements of the `employees` set that are
  /// also members of the `neighbors` set are removed. In particular, the
  /// names `"Bethany"` and `"Eric"` are removed from `employees`.
  ///
  ///     var employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     employees.subtract(neighbors)
  ///     print(employees)
  ///     // Prints "["Diana", "Chris", "Alicia"]"
  ///
  /// - Parameter other: Another set.
  @inlinable
  public mutating func subtract(_ other: Set<Element>) {
    _subtract(other)
  }

  /// Returns a Boolean value that indicates whether this set is a subset of
  /// the given set.
  ///
  /// Set *A* is a subset of another set *B* if every member of *A* is also a
  /// member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isSubset(of: employees))
  ///     // Prints "true"
  ///
  /// - Parameter other: Another set.
  /// - Returns: `true` if the set is a subset of `other`; otherwise, `false`.
  @inlinable
  public func isSubset(of other: Set<Element>) -> Bool {
    guard self.count > 0 else { return true }
    guard self.count <= other.count else { return false }
    for member in self {
      if !other.contains(member) {
        return false
      }
    }
    return true
  }

  /// Returns a Boolean value that indicates whether this set is a superset of
  /// the given set.
  ///
  /// Set *A* is a superset of another set *B* if every member of *B* is also a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isSuperset(of: attendees))
  ///     // Prints "true"
  ///
  /// - Parameter other: Another set.
  /// - Returns: `true` if the set is a superset of `other`; otherwise,
  ///   `false`.
  @inlinable
  public func isSuperset(of other: Set<Element>) -> Bool {
    return other.isSubset(of: self)
  }

  /// Returns a Boolean value that indicates whether this set has no members in
  /// common with the given set.
  ///
  /// In the following example, the `employees` set is disjoint with the
  /// `visitors` set because no name appears in both sets.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let visitors: Set = ["Marcia", "Nathaniel", "Olivia"]
  ///     print(employees.isDisjoint(with: visitors))
  ///     // Prints "true"
  ///
  /// - Parameter other: Another set.
  /// - Returns: `true` if the set has no elements in common with `other`;
  ///   otherwise, `false`.
  @inlinable
  public func isDisjoint(with other: Set<Element>) -> Bool {
    return _isDisjoint(with: other)
  }

  /// Returns a new set containing the elements of this set that do not occur
  /// in the given set.
  ///
  /// In the following example, the `nonNeighbors` set is made up of the
  /// elements of the `employees` set that are not elements of `neighbors`:
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let nonNeighbors = employees.subtracting(neighbors)
  ///     print(nonNeighbors)
  ///     // Prints "["Diana", "Chris", "Alicia"]"
  ///
  /// - Parameter other: Another set.
  /// - Returns: A new set.
  @inlinable
  public func subtracting(_ other: Set<Element>) -> Set<Element> {
    return Set(_native: _variant.subtracting(other))
  }

  /// Returns a Boolean value that indicates whether the set is a strict
  /// superset of the given sequence.
  ///
  /// Set *A* is a strict superset of another set *B* if every member of *B* is
  /// also a member of *A* and *A* contains at least one element that is *not*
  /// a member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isStrictSuperset(of: attendees))
  ///     // Prints "true"
  ///     print(employees.isStrictSuperset(of: employees))
  ///     // Prints "false"
  ///
  /// - Parameter other: Another set.
  /// - Returns: `true` if the set is a strict superset of
  ///   `other`; otherwise, `false`.
  @inlinable
  public func isStrictSuperset(of other: Set<Element>) -> Bool {
    return self.count > other.count && self.isSuperset(of: other)
  }

  /// Returns a Boolean value that indicates whether the set is a strict subset
  /// of the given sequence.
  ///
  /// Set *A* is a strict subset of another set *B* if every member of *A* is
  /// also a member of *B* and *B* contains at least one element that is not a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isStrictSubset(of: employees))
  ///     // Prints "true"
  ///
  ///     // A set is never a strict subset of itself:
  ///     print(attendees.isStrictSubset(of: attendees))
  ///     // Prints "false"
  ///
  /// - Parameter other: Another set.
  /// - Returns: `true` if the set is a strict subset of
  ///   `other`; otherwise, `false`.
  @inlinable
  public func isStrictSubset(of other: Set<Element>) -> Bool {
    return other.isStrictSuperset(of: self)
  }

  /// Returns a new set with the elements that are common to both this set and
  /// the given sequence.
  ///
  /// In the following example, the `bothNeighborsAndEmployees` set is made up
  /// of the elements that are in *both* the `employees` and `neighbors` sets.
  /// Elements that are in only one or the other are left out of the result of
  /// the intersection.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let bothNeighborsAndEmployees = employees.intersection(neighbors)
  ///     print(bothNeighborsAndEmployees)
  ///     // Prints "["Bethany", "Eric"]"
  ///
  /// - Parameter other: Another set.
  /// - Returns: A new set.
  @inlinable
  public func intersection(_ other: Set<Element>) -> Set<Element> {
    return Set(_native: _variant.intersection(other))
  }

  /// Removes the elements of the set that are also in the given sequence and
  /// adds the members of the sequence that are not already in the set.
  ///
  /// In the following example, the elements of the `employees` set that are
  /// also members of `neighbors` are removed from `employees`, while the
  /// elements of `neighbors` that are not members of `employees` are added to
  /// `employees`. In particular, the names `"Alicia"`, `"Chris"`, and
  /// `"Diana"` are removed from `employees` while the names `"Forlani"` and
  /// `"Greta"` are added.
  ///
  ///     var employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     employees.formSymmetricDifference(neighbors)
  ///     print(employees)
  ///     // Prints "["Diana", "Chris", "Forlani", "Alicia", "Greta"]"
  ///
  /// - Parameter other: Another set.
  @inlinable
  public mutating func formSymmetricDifference(_ other: Set<Element>) {
    self = self.symmetricDifference(other)
  }
}

/// This protocol is only used for compile-time checks that
/// every Set type implements all required operations.
internal protocol _SetBuffer {
  associatedtype Element
  associatedtype Index

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func formIndex(after i: inout Index)
  func index(for element: Element) -> Index?
  var count: Int { get }

  func contains(_ member: Element) -> Bool
  func element(at i: Index) -> Element
}

/// An instance of this class has all `Set` data tail-allocated.  Enough bytes
/// are allocated to hold the _HashTable's metadata entries as well of all
/// buckets.  The data layout starts with the metadata entries, followed by the
/// element-sized buckets.
///
/// **Note:** The precise layout of this type is relied on in the runtime
/// to provide a statically allocated empty singleton.
/// See stdlib/public/stubs/GlobalObjects.cpp for details.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
@_objc_non_lazy_realization
internal class _SwiftRawSetStorage: _SwiftNativeNSSet {
  // The _HashTable struct contains pointers into tail-allocated storage, so
  // this is unsafe and needs `_fixLifetime` calls in the caller.
  @usableFromInline
  @nonobjc
  internal final var hashTable: _HashTable

  @usableFromInline
  internal final var seed: Hasher._Seed

  @usableFromInline
  @nonobjc
  internal final var rawElements: UnsafeMutableRawPointer

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

  @usableFromInline
  internal func invalidate() {}
}

/// The storage class for the singleton empty set.
/// The single instance of this class is created by the runtime.
@usableFromInline
internal class _SwiftEmptySetStorage: _SwiftRawSetStorage {
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by using the `empty` singleton")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("Don't call this designated initializer")
  }
#endif
}

extension _SwiftRawSetStorage {
  /// The empty singleton that is used for every single Set that is created
  /// without any elements. The contents of the storage must never be mutated.
  @inlinable
  @nonobjc
  internal static var empty: _SwiftEmptySetStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptySetStorage))
  }
}


extension _SwiftEmptySetStorage: _NSSetCore {
#if _runtime(_ObjC)
  //
  // NSSet implementation, assuming Self is the empty singleton
  //
  @objc
  internal var count: Int {
    return 0
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal func member(_ object: AnyObject) -> AnyObject? {
    return nil
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    return _SwiftEmptyNSEnumerator()
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
#endif
}

@_fixed_layout
@usableFromInline
internal final class _SwiftNativeSetStorage<
  Element: Hashable
>: _SwiftRawSetStorage, _NSSetCore {

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

  deinit {
    let elements = self.elements

    if !_isPOD(Element.self) && hashTable.count > 0 {
      for index in hashTable.occupiedIndices {
        (elements + index.bucket).deinitialize(count: 1)
      }
      hashTable.count = -1
    }

    _fixLifetime(self)
  }

  @usableFromInline
  internal override func invalidate() {
    hashTable.count = -1
  }

  @inlinable
  static internal func allocate(
    capacity: Int,
    seed: Hasher._Seed
  ) -> _SwiftNativeSetStorage {
    let scale = _HashTable.scale(forCapacity: Swift.max(1 ,capacity))
    return allocate(scale: scale, seed: seed)
  }

  @usableFromInline
  @_effects(releasenone)
  static internal func allocate(
    scale: Int,
    seed: Hasher._Seed
  ) -> _SwiftNativeSetStorage {
    _sanityCheck(scale >= 0 && scale < Int.bitWidth - 1)
    let bucketCount = 1 &<< scale
    let storage = Builtin.allocWithTailElems_2(
      _SwiftNativeSetStorage<Element>.self,
      bucketCount._builtinWordValue, _HashTable.MapEntry.self,
      bucketCount._builtinWordValue, Element.self)

    let mapAddr = Builtin.projectTailElems(storage, _HashTable.MapEntry.self)
    let elementsAddr = Builtin.getTailAddr_Word(
      mapAddr,
      bucketCount._builtinWordValue,
      _HashTable.MapEntry.self,
      Element.self)
    storage.hashTable = _HashTable(
      scale: scale,
      map: UnsafeMutablePointer(mapAddr))
    storage.rawElements = UnsafeMutableRawPointer(elementsAddr)
    storage.seed = seed
    return storage
  }

  @inlinable
  final var elements: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return self.rawElements.assumingMemoryBound(to: Element.self)
    }
  }

  internal var asNative: _NativeSet<Element> {
    return _NativeSet(self)
  }

#if _runtime(_ObjC)
  // _NSSetCore implementation
  //
  // FIXME: These should be moved to an extension, but @objc members aren't
  // supported in extensions of generic classes.

  @objc
  internal init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("Don't call this designated initializer")
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal var count: Int {
    return hashTable.count
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    return _SwiftSetNSEnumerator(asNative)
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
    hashTable.checkOccupied(index)
    let endIndex = asNative.endIndex
    var stored = 0
    for i in 0..<count {
      guard index < endIndex else { break }

      let element = asNative.uncheckedElement(at: index)
      unmanagedObjects[i] = _bridgeAnythingToObjectiveC(element)

      stored += 1
      asNative.formIndex(after: &index)
    }
    theState.extra.0 = CUnsignedLong(index.bucket)
    state.pointee = theState
    return stored
  }

  @objc
  internal func member(_ object: AnyObject) -> AnyObject? {
    guard let native = _conditionallyBridgeFromObjectiveC(object, Element.self)
    else { return nil }

    let (index, found) = asNative.find(native)
    guard found else { return nil }
    return _bridgeAnythingToObjectiveC(asNative.uncheckedElement(at: index))
  }
#endif
}

/// A wrapper around _SwiftRawSetStorage that provides most of the
/// implementation of Set.
///
/// This type and most of its functionality doesn't require Hashable at all.
/// The reason for this is to support storing AnyObject for bridging
/// with _SwiftDeferredNSSet. What functionality actually relies on
/// Hashable can be found in an extension.
@usableFromInline
@_fixed_layout
internal struct _NativeSet<Element: Hashable> {
  /// See this comments on _SwiftRawSetStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline
  internal var _storage: _SwiftRawSetStorage

  /// Constructs an instance from the empty singleton.
  @inlinable
  internal init() {
    self._storage = _SwiftRawSetStorage.empty
  }

  /// Constructs a native set adopting the given storage.
  @inlinable
  internal init(_ storage: _SwiftRawSetStorage) {
    self._storage = storage
  }

  @inlinable
  internal init(capacity: Int) {
    self._storage = _SwiftNativeSetStorage<Element>.allocate(
      capacity: capacity,
      seed: Hasher._randomSeed())
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_ cocoa: _CocoaSet) {
    self.init(cocoa, capacity: cocoa.count)
  }

  @inlinable
  internal init(_ cocoa: _CocoaSet, capacity: Int) {
    _sanityCheck(cocoa.count <= capacity)
    self.init(capacity: capacity)
    for element in cocoa {
      insertNew(_forceBridgeFromObjectiveC(element, Element.self))
    }
  }
#endif
}

extension _NativeSet {
  @inlinable
  internal var bucketCount: Int {
    @inline(__always)
    get {
      return _assumeNonNegative(_storage.hashTable.bucketCount)
    }
  }

  @inlinable
  internal var capacity: Int {
    @inline(__always)
    get {
      return _assumeNonNegative(_storage.hashTable.capacity)
    }
  }

  @inlinable
  internal var indices: _HashTable.OccupiedIndices {
    @inline(__always)
    get {
      return _storage.hashTable.occupiedIndices
    }
  }

  @inlinable
  internal var elements: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return _storage.rawElements.assumingMemoryBound(to: Element.self)
    }
  }

  @inlinable
  @inline(__always)
  internal func uncheckedElement(at i: Index) -> Element {
    _sanityCheck(_storage.hashTable.isOccupied(i))
    defer { _fixLifetime(self) }
    return elements[i.bucket]
  }

  @usableFromInline @_transparent
  internal func uncheckedDestroyElement(at i: Index) {
    _sanityCheck(_storage.hashTable.isValid(i))
    defer { _fixLifetime(self) }
    (elements + i.bucket).deinitialize(count: 1)
  }

  @usableFromInline @_transparent
  internal func uncheckedInitializeElement(at i: Index, to element: Element) {
    _sanityCheck(_storage.hashTable.isValid(i))
    defer { _fixLifetime(self) }
    (elements + i.bucket).initialize(to: element)
  }

  @usableFromInline @_transparent
  internal func uncheckedMoveInitializeElement(
    from source: _NativeSet,
    at sourceIndex: Index,
    to targetIndex: Index
  ) {
    _sanityCheck(source._storage.hashTable.isOccupied(sourceIndex))
    _sanityCheck(_storage.hashTable.isValid(targetIndex))
    defer { _fixLifetime(self) }
    (elements + targetIndex.bucket).moveInitialize(
      from: source.elements + sourceIndex.bucket,
      count: 1)
  }
}

extension _NativeSet {
  @inlinable
  internal mutating func reallocate(
    isUnique: Bool,
    capacity: Int
  ) -> Bool {
    let scale = _HashTable.scale(forCapacity: capacity)
    let rehash = (scale != _storage.hashTable.scale)

    // We generate a unique hash seed whenever we change the size of the hash
    // table, so that we avoid certain copy operations becoming quadratic,
    // without breaking value semantics. (For background details, see
    // https://bugs.swift.org/browse/SR-3268)
    let seed = rehash ? Hasher._randomSeed() : _storage.seed

    var result = _NativeSet(
      _SwiftNativeSetStorage<Element>.allocate(scale: scale, seed: seed))
    switch (isUnique, rehash) {
    case (true, true): // Move & rehash elements
      for index in self.indices {
        result._unsafeMoveNew(from: self, at: index)
      }
      _storage.invalidate()
    case (true, false): // Move elements to same buckets in new storage
      result._storage.hashTable.copyContents(of: _storage.hashTable)
      for index in self.indices {
        result.uncheckedMoveInitializeElement(from: self, at: index, to: index)
      }
      _storage.invalidate()
    case (false, true): // Copy & rehash elements
      for index in self.indices {
        result.insertNew(self.uncheckedElement(at: index))
      }
    case (false, false): // Copy elements to same buckets in new storage
      result._storage.hashTable.copyContents(of: _storage.hashTable)
      for index in self.indices {
        let element = uncheckedElement(at: index)
        result.uncheckedInitializeElement(at: index, to: element)
      }
    }
    _storage = result._storage
    return rehash
  }

  @inlinable
  @inline(__always)
  internal mutating func ensureUnique(
    isUnique: Bool,
    capacity: Int
  ) -> (reallocated: Bool, rehashed: Bool) {
    if isUnique && capacity <= self.capacity {
      return (false, false)
    }
    return (true, reallocate(isUnique: isUnique, capacity: capacity))
  }
}

extension _NativeSet: _SetBuffer {
  @usableFromInline
  internal typealias Index = _HashTable.Index

  @inlinable
  internal var startIndex: Index {
    return _storage.hashTable.startIndex
  }

  @inlinable
  internal var endIndex: Index {
    return _storage.hashTable.endIndex
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    return _storage.hashTable.index(after: i)
  }

  @inlinable
  internal func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable
  @inline(__always)
  internal func index(for element: Element) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (i, found) = find(element)
    return found ? i : nil
  }

  @inlinable
  internal var count: Int {
    get {
      return _assumeNonNegative(_storage.hashTable.count)
    }
  }

  @inlinable
  @inline(__always)
  internal func contains(_ member: Element) -> Bool {
    if count == 0 { // Fast path that avoids computing the hash of the key.
      return false
    }
    return find(member).found
  }

  @inlinable
  @inline(__always)
  internal func element(at index: Index) -> Element {
    _storage.hashTable.checkOccupied(index)
    defer { _fixLifetime(self) }
    return elements[index.bucket]
  }
}

extension _NativeSet: _HashTableDelegate {
  internal func hashValue(at index: Index) -> Int {
    return uncheckedElement(at: index)._rawHashValue(seed: _storage.seed)
  }

  internal func moveEntry(from source: Index, to target: Index) {
    _sanityCheck(source != target)
    (elements + target.bucket).moveInitialize(
      from: elements + source.bucket,
      count: 1)
  }
}

extension _NativeSet {
#if _runtime(_ObjC)
  @usableFromInline
  internal func bridged() -> _NSSet {
    // We can zero-cost bridge if our keys are verbatim
    // or if we're the empty singleton.

    // Temporary var for SOME type safety before a cast.
    let nsSet: _NSSetCore

    if _storage === _SwiftRawSetStorage.empty {
      nsSet = _SwiftRawSetStorage.empty
    } else if _isBridgedVerbatimToObjectiveC(Element.self) {
      nsSet = unsafeDowncast(_storage, to: _SwiftNativeSetStorage<Element>.self)
    } else {
      nsSet = _SwiftDeferredNSSet(self)
    }

    // Cast from "minimal NSSet" to "NSSet"
    // Note that if you actually ask Swift for this cast, it will fail.
    // Never trust a shadow protocol!
    return unsafeBitCast(nsSet, to: _NSSet.self)
  }
#endif
}

extension _NativeSet {
  @inlinable
  @inline(__always)
  internal func hashValue(for element: Element) -> Int {
    return element._rawHashValue(seed: _storage.seed)
  }

  @inlinable
  @inline(__always)
  internal func find(_ element: Element) -> (index: Index, found: Bool) {
    return find(element, hashValue: self.hashValue(for: element))
  }

  /// Search for a given key starting from the specified bucket.
  ///
  /// If the key is not present, returns the position where it could be
  /// inserted.
  @inlinable
  @inline(__always)
  internal func find(
    _ element: Element,
    hashValue: Int
  ) -> (index: Index, found: Bool) {
    var (index, found) = _storage.hashTable.lookupFirst(hashValue: hashValue)
    if _fastPath(!found || uncheckedElement(at: index) == element) {
      return (index, found)
    }
    while true {
      (index, found) =
        _storage.hashTable.lookupNext(hashValue: hashValue, after: index)
      if !found || uncheckedElement(at: index) == element {
        return (index, found)
      }
    }
  }
}

extension _NativeSet {
  @inlinable
  internal func isSubset<S: Sequence>(of possibleSuperset: S) -> Bool
    where S.Element == Element {
    // Allocate a temporary bitmap to mark elements in self that we've seen in
    // possibleSuperset.
    var bitmap = _Bitmap(bitCount: self.bucketCount)
    var seen = 0
    for element in possibleSuperset {
      // Found a new element of self in possibleSuperset.
      let (index, found) = find(element)
      guard found else { continue }
      if bitmap._uncheckedInsert(index.bucket) {
        seen += 1
        if seen == self.count {
          return true
        }
      }
    }
    return false
  }

  @inlinable
  internal func isStrictSubset<S: Sequence>(of possibleSuperset: S) -> Bool
    where S.Element == Element {
    // Allocate a temporary bitmap to mark elements in self that we've seen in
    // possibleSuperset.
    var bitmap = _Bitmap(bitCount: self.bucketCount)
    var seen = 0
    var isStrict = false
    for element in possibleSuperset {
      let (index, found) = find(element)
      guard found else {
        if !isStrict {
          isStrict = true
          if seen == self.count { return true }
        }
        continue
      }
      if bitmap._uncheckedInsert(index.bucket) {
        // Found a new element of self in possibleSuperset.
        seen += 1
        if seen == self.count && isStrict {
          return true
        }
      }
    }
    return false
  }

  @inlinable
  internal func isStrictSuperset<S: Sequence>(of possibleSubset: S) -> Bool
    where S.Element == Element {
    // Allocate a temporary bitmap to mark elements in self that we've seen in
    // possibleStrictSubset.
    var bitmap = _Bitmap(bitCount: self.bucketCount)
    var seen = 0
    for element in possibleSubset {
      let (index, found) = find(element)
      guard found else { return false }
      if bitmap._uncheckedInsert(index.bucket) {
        // Found a new element of possibleSubset in self.
        seen += 1
        if seen == self.count {
          return false
        }
      }
    }
    return true
  }

  @inlinable
  internal func bitmap<S: Sequence>(
    markingElementsIn other: S
  ) -> (bitmap: _Bitmap, count: Int)
  where S.Element == Element {
    var bitmap = _Bitmap(bitCount: self.bucketCount)
    var count = 0
    for element in other {
      let (index, found) = find(element)
      if found, bitmap._uncheckedInsert(index.bucket) {
        count += 1
      }
    }
    return (bitmap, count)
  }

  @inlinable
  internal mutating func formUnion<S: Sequence>(_ other: S, isUnique: Bool)
    where S.Element == Element {
    var isUnique = isUnique
    if other is Set<Element> {
      // When `other` is also a set, then max(count, other.count) is a lower
      // bound on the result's element count.
      let c = other.underestimatedCount
      if c > count {
        isUnique = ensureUnique(isUnique: isUnique, capacity: c).reallocated
      }
    }
    for element in other {
      let (inserted, _) = _insert(element, isUnique: isUnique)
      isUnique = isUnique || inserted
    }
  }

  @inlinable
  internal func subtracting<S: Sequence>(_ other: S) -> _NativeSet
    where S.Element == Element {
    // Rather than directly creating a new set, mark common elements in a bitmap
    // first. This ensures we hash each element (in both sets) only once, and
    // that we'll have an exact count for the result set, preventing rehashings
    // during insertions.
    let (dupes, dupeCount) = bitmap(markingElementsIn: other)
    if dupeCount == 0 { return self }
    if dupeCount == self.count { return _NativeSet() }
    var result = _NativeSet(capacity: self.count - dupeCount)
    for index in self.indices where !dupes._uncheckedContains(index.bucket) {
      result.insertNew(self.uncheckedElement(at: index))
    }
    return result
  }

  @inlinable
  internal func intersection<S: Sequence>(_ other: S) -> _NativeSet<Element>
    where S.Element == Element {
    // Rather than directly creating a new set, mark common elements in a bitmap
    // first. This minimizes hashing, and ensures that we'll have an exact count
    // for the result set, preventing rehashings during insertions.
    let (dupes, dupeCount) = bitmap(markingElementsIn: other)
    if dupeCount == 0 { return _NativeSet() }
    if dupeCount == self.count { return self }
    if let other = other as? _NativeSet<Element>, dupeCount == other.count {
      return other
    }
    var result = _NativeSet(capacity: dupeCount)
    for bucket in dupes {
      result.insertNew(self.uncheckedElement(at: Index(bucket: bucket)))
    }
    _sanityCheck(result.count == dupeCount)
    return result
  }

  @inlinable
  internal func symmetricDifference<S: Sequence>(_ other: S) -> _NativeSet
    where S.Element == Element {
    // Rather than directly creating a new set, mark common elements from `self`
    // in a bitmap, and collect distinct elements from `other` in an array.
    // This minimizes hashing, and ensures that we'll have an exact count for
    // the result set, preventing rehashings during insertions.
    var dupes = _Bitmap(bitCount: self.bucketCount)
    var dupeCount = 0
    var distinctsInOther: [Element] = []
    for element in other {
      let (index, found) = find(element)
      if found {
        if dupes._uncheckedInsert(index.bucket) {
          dupeCount += 1
        }
      } else {
        distinctsInOther.append(element)
      }
    }
    var result = _NativeSet<Element>(
      capacity: count - dupeCount + distinctsInOther.count)
    for index in self.indices where !dupes._uncheckedContains(index.bucket) {
      result.insertNew(self.uncheckedElement(at: index))
    }
    for element in distinctsInOther {
      result.insertNew(element)
    }
    return result
  }

  @inlinable
  internal func symmetricDifference(_ other: _NativeSet) -> _NativeSet {
    if self.count < other.count {
      // Prefer to iterate over the smaller set.
      return other.symmetricDifference(self)
    }
    // Rather than directly creating a new set, mark common elements from `self`
    // and `other` in two bitmaps.  This minimizes hashing, and ensures that
    // we'll have an exact count for the result set, preventing rehashings
    // during insertions.
    var selfDupes = _Bitmap(bitCount: self.bucketCount)
    var otherDupes = _Bitmap(bitCount: other.bucketCount)
    var dupeCount = 0
    for otherIndex in other.indices {
      let (selfIndex, found) = self.find(other.uncheckedElement(at: otherIndex))
      if found {
        if !selfDupes._uncheckedInsert(selfIndex.bucket) ||
          !otherDupes._uncheckedInsert(otherIndex.bucket) {
          ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
        }
        dupeCount += 1
      }
    }
    var result = _NativeSet<Element>(
      capacity: self.count + other.count - 2 * dupeCount)
    for i in self.indices where !selfDupes._uncheckedContains(i.bucket) {
      result.insertNew(self.uncheckedElement(at: i))
    }
    for i in other.indices where !otherDupes._uncheckedContains(i.bucket) {
      result.insertNew(other.uncheckedElement(at: i))
    }
    return result
  }
}

// This function has a highly visible name to make it stand out in stack traces.
@usableFromInline
@inline(never)
internal func ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(
  _ elementType: Any.Type
) -> Never {
  fatalError("""
    Duplicate elements of type '\(elementType)' were found in a Set.
    This usually means that either the type violates Hashable's requirements, or
    that members of such a set were mutated after insertion.
    """)
}

extension _NativeSet {
  @inlinable
  internal func _unsafeMoveNew(from source: _NativeSet, at sourceIndex: Index) {
    _sanityCheck(source._storage.hashTable.isOccupied(sourceIndex))
    _sanityCheck(count + 1 <= capacity)
    let sourcePtr = source.elements + sourceIndex.bucket
    let hashValue = self.hashValue(for: sourcePtr.pointee)
    if _isDebugAssertConfiguration() {
      // In debug builds, perform a full lookup and trap if we detect duplicate
      // elements -- these imply that the Element type violates Hashable
      // requirements. This is generally more costly than a direct insertion,
      // because we'll need to compare elements in case of hash collisions.
      let (index, found) = find(sourcePtr.pointee, hashValue: hashValue)
      guard !found else {
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
      }
      _storage.hashTable.insert(hashValue: hashValue, at: index)
      uncheckedMoveInitializeElement(from: source, at: sourceIndex, to: index)
    } else {
      let index = _storage.hashTable.insertNew(hashValue: hashValue)
      uncheckedMoveInitializeElement(from: source, at: sourceIndex, to: index)
    }
  }

  /// Insert a new element into uniquely held storage.
  /// Storage must be uniquely referenced.
  /// The `element` must not be already present in the Set.
  @inlinable
  internal mutating func insertNew(_ element: Element) {
    _ = ensureUnique(isUnique: true, capacity: count + 1)
    let hashValue = self.hashValue(for: element)
    if _isDebugAssertConfiguration() {
      // In debug builds, perform a full lookup and trap if we detect duplicate
      // elements -- these imply that the Element type violates Hashable
      // requirements. This is generally more costly than a direct insertion,
      // because we'll need to compare elements in case of hash collisions.
      let (index, found) = find(element, hashValue: hashValue)
      guard !found else {
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
      }
      _storage.hashTable.insert(hashValue: hashValue, at: index)
      uncheckedInitializeElement(at: index, to: element)
    } else {
      let index = _storage.hashTable.insertNew(hashValue: hashValue)
      uncheckedInitializeElement(at: index, to: element)
    }
  }
}

extension _NativeSet {
  @inlinable
  internal mutating func reserveCapacity(_ capacity: Int, isUnique: Bool) {
    _ = ensureUnique(isUnique: isUnique, capacity: capacity)
  }

  @inlinable
  internal mutating func update(
    with element: Element,
    isUnique: Bool
  ) -> Element? {
    var hashValue = self.hashValue(for: element)
    var (index, found) = find(element, hashValue: hashValue)
    let (_, rehashed) = ensureUnique(
      isUnique: isUnique,
      capacity: count + (found ? 0 : 1))
    if rehashed {
      hashValue = self.hashValue(for: element)
      (index, found) = find(element, hashValue: hashValue)
    }
    if found {
      let old = (elements + index.bucket).move()
      uncheckedInitializeElement(at: index, to: element)
      return old
    }
    _storage.hashTable.insert(hashValue: hashValue, at: index)
    uncheckedInitializeElement(at: index, to: element)
    return nil
  }

  /// A variant of insert that returns the inserted index rather than the
  /// element stored in the corresponding bucket.
  @inlinable
  @discardableResult
  internal mutating func _insert(
    _ element: Element,
    isUnique: Bool
  ) -> (inserted: Bool, index: Index) {
    var hashValue = self.hashValue(for: element)
    var (index, found) = find(element, hashValue: hashValue)
    if found { return (false, index) }
    if ensureUnique(isUnique: isUnique, capacity: count + 1).rehashed {
      hashValue = self.hashValue(for: element)
      (index, found) = find(element, hashValue: hashValue)
      guard !found else {
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
      }
    }
    _storage.hashTable.insert(hashValue: hashValue, at: index)
    uncheckedInitializeElement(at: index, to: element)
    return (true, index)
  }

  @inlinable
  internal mutating func insert(
    _ element: Element,
    isUnique: Bool
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    let (inserted, index) = _insert(element, isUnique: isUnique)
    if inserted {
      return (true, element)
    }
    return (false, uncheckedElement(at: index))
  }

  @usableFromInline
  @_effects(releasenone)
  internal mutating func _delete(at index: Index, hashValue: Int) {
    _storage.hashTable.delete(at: index, hashValue: hashValue, with: self)
  }

  @inlinable
  internal mutating func remove(_ member: Element, isUnique: Bool) -> Element? {
    var hashValue = self.hashValue(for: member)
    var (index, found) = find(member, hashValue: hashValue)

    // Fast path: if the key is not present, we will not mutate the set, so
    // don't force unique buffer.
    if !found {
      return nil
    }

    if ensureUnique(isUnique: isUnique, capacity: capacity).rehashed {
      hashValue = self.hashValue(for: member)
      (index, found) = find(member, hashValue: hashValue)
      guard found else {
        ELEMENT_TYPE_OF_SET_VIOLATES_HASHABLE_REQUIREMENTS(Element.self)
      }
    }

    let old = (elements + index.bucket).move()
    _delete(at: index, hashValue: hashValue)
    return old
  }

  @inlinable
  internal mutating func remove(at index: Index, isUnique: Bool) -> Element {
    _precondition(_storage.hashTable.isOccupied(index), "Invalid index")
    let (_, rehashed) = ensureUnique(isUnique: isUnique, capacity: capacity)
    _sanityCheck(!rehashed)

    let old = (elements + index.bucket).move()
    _delete(at: index, hashValue: self.hashValue(for: old))
    return old
  }

  @usableFromInline
  internal mutating func removeAll(isUnique: Bool) {
    guard isUnique else {
      _storage = _SwiftNativeSetStorage<Element>.allocate(
        scale: self._storage.hashTable.scale,
        seed: Hasher._randomSeed())
      return
    }
    for index in indices {
      uncheckedDestroyElement(at: index)
    }
    _storage.hashTable.removeAll()
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator that works with any native Set. Non-verbatim bridged sets
/// need to supply a _SwiftDeferredNSSet instance to provide access to bridged
/// values. Used by the various NSSet implementations.
final internal class _SwiftSetNSEnumerator<Element: Hashable>
  : _SwiftNativeNSEnumerator, _NSEnumerator {
  internal var base: _NativeSet<Element>
  internal var deferred: _SwiftDeferredNSSet<Element>?
  internal var nextIndex: _HashTable.Index
  internal var endIndex: _HashTable.Index

  internal override required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  internal init(_ base: _NativeSet<Element>) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self))
    self.base = base
    self.deferred = nil
    self.nextIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  internal init(_ deferred: _SwiftDeferredNSSet<Element>) {
    _sanityCheck(!_isBridgedVerbatimToObjectiveC(Element.self))
    self.base = deferred.native
    self.deferred = deferred
    self.nextIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  private func bridgedElement(at index: _HashTable.Index) -> AnyObject {
    if let deferred = self.deferred {
      return deferred.bridgedElements[index]
    }
    return _bridgeAnythingToObjectiveC(base.uncheckedElement(at: index))
  }

  //
  // NSEnumerator implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  @objc
  internal func nextObject() -> AnyObject? {
    if nextIndex == endIndex {
      return nil
    }
    let index = nextIndex
    base.formIndex(after: &nextIndex)
    return self.bridgedElement(at: index)
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

    if nextIndex == endIndex {
      state.pointee = theState
      return 0
    }

    // Return only a single element so that code can start iterating via fast
    // enumeration, terminate it, and continue via NSEnumerator.
    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = self.bridgedElement(at: nextIndex)
    base.formIndex(after: &nextIndex)
    state.pointee = theState
    return 1
  }
}
#endif

#if _runtime(_ObjC)
/// This class exists for Objective-C bridging. It holds a reference to a
/// _NativeSet, and can be upcast to NSSelf when bridging is necessary.  This is
/// the fallback implementation for situations where toll-free bridging isn't
/// possible. On first access, a _NativeSet of AnyObject will be constructed
/// containing all the bridged elements.
final internal class _SwiftDeferredNSSet<Element: Hashable>
  : _SwiftNativeNSSet, _NSSetCore {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  private var _bridgedElements_DoNotUse: AnyObject? = nil

  /// The unbridged elements.
  internal var native: _NativeSet<Element>

  internal init(_ native: _NativeSet<Element>) {
    self.native = native
    super.init()
  }

  deinit {
    if native.count > 0, let bridged = _bridgedElements {
      bridged.invalidate(with: native.indices)
    }
  }

  /// Returns the pointer to the stored property, which contains bridged
  /// Set elements.
  @nonobjc
  private var _bridgedElementsPtr: UnsafeMutablePointer<AnyObject?>
  {
    return _getUnsafePointerToStoredProperties(self)
      .assumingMemoryBound(to: Optional<AnyObject>.self)
  }

  /// The buffer for bridged Set elements, if present.
  @nonobjc
  private var _bridgedElements: _BridgingHashBuffer? {
    if let ref = _stdlib_atomicLoadARCRef(object: _bridgedElementsPtr) {
      return unsafeDowncast(ref, to: _BridgingHashBuffer.self)
    }
    return nil
  }

  /// Attach a buffer for bridged Set elements.
  @nonobjc
  private func _initializeBridgedElements(
    _ storage: _BridgingHashBuffer
  ) -> Bool {
    return _stdlib_atomicInitializeARCRef(
      object: _bridgedElementsPtr,
      desired: storage)
  }

  /// Returns the bridged Set values.
  @nonobjc
  internal var bridgedElements: _BridgingHashBuffer {
    return _bridgedElements!
  }

  @nonobjc
  internal func bridgeEverything() {
    if _fastPath(_bridgedElements != nil) {
      return
    }

    // Allocate and initialize heap storage for bridged objects.
    let bridged = _BridgingHashBuffer.create(bucketCount: native.bucketCount)
    for index in native.indices {
      let object = _bridgeAnythingToObjectiveC(native.element(at: index))
      bridged.initialize(at: index, to: object)
    }

    // Atomically put the bridged elements in place.
    if !_initializeBridgedElements(bridged) {
      // Lost the race.
      bridged.invalidate(with: native.indices)
    }
  }

  //
  // NSSet implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `base`.
  //

  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSSet` type, which is immutable.
    return self
  }

  @objc
  internal var count: Int {
    return native.count
  }

  @objc
  internal func member(_ object: AnyObject) -> AnyObject? {
    guard let element =
      _conditionallyBridgeFromObjectiveC(object, Element.self)
    else { return nil }

    let (index, found) = native.find(element)
    guard found else { return nil }
    bridgeEverything()
    return bridgedElements[index]
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    bridgeEverything()
    return _SwiftSetNSEnumerator(self)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?,
    count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(native.startIndex.bucket)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeSet<Element>.Index(bucket: Int(theState.extra.0))
    let endIndex = native.endIndex
    var stored = 0

    // Only need to bridge once, so we can hoist it out of the loop.
    if (currIndex != endIndex) {
      bridgeEverything()
    }

    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      let bridgedKey = bridgedElements[currIndex]
      unmanagedObjects[i] = bridgedKey
      stored += 1
      native.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.bucket)
    state.pointee = theState
    return stored
  }
}
#endif

#if _runtime(_ObjC)
@usableFromInline
@_fixed_layout
internal struct _CocoaSet {
  @usableFromInline
  internal let object: _NSSet

  @inlinable
  internal init(_ object: _NSSet) {
    self.object = object
  }
}

extension _CocoaSet: Equatable {
  @usableFromInline
  internal static func ==(lhs: _CocoaSet, rhs: _CocoaSet) -> Bool {
    return _stdlib_NSObject_isEqual(lhs.object, rhs.object)
  }
}

extension _CocoaSet: _SetBuffer {
  @usableFromInline
  internal typealias Element = AnyObject

  @inlinable
  internal var startIndex: Index {
    return Index(self, startIndex: ())
  }

  @inlinable
  internal var endIndex: Index {
    return Index(self, endIndex: ())
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    var i = i
    formIndex(after: &i)
    return i
  }

  @usableFromInline
  @_effects(releasenone)
  internal func formIndex(after i: inout Index) {
    _precondition(i.base.object === self.object, "Invalid index")
    _precondition(i.currentKeyIndex < i.allKeys.value,
      "Cannot increment endIndex")
    i.currentKeyIndex += 1
  }

  @usableFromInline
  internal func index(for element: AnyObject) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if !contains(element) {
      return nil
    }

    let allKeys = _stdlib_NSSet_allObjects(object)
    var index = -1
    for i in 0..<allKeys.value {
      if _stdlib_NSObject_isEqual(element, allKeys[i]) {
        index = i
        break
      }
    }
    _sanityCheck(index >= 0,
      "Element was found in fast path, but not found later?")
    return Index(self, allKeys, index)
  }

  @inlinable
  internal var count: Int {
    return object.count
  }

  @inlinable
  internal func contains(_ element: AnyObject) -> Bool {
    return object.member(element) != nil
  }

  @usableFromInline
  internal func element(at i: Index) -> AnyObject {
    let value: AnyObject? = i.allKeys[i.currentKeyIndex]
    _sanityCheck(value != nil, "Item not found in underlying NSSet")
    return value!
  }
}
#endif

extension Set {
  @usableFromInline
  @_frozen
  internal enum _Variant {
    case native(_NativeSet<Element>)
#if _runtime(_ObjC)
    case cocoa(_CocoaSet)
#endif
  }
}

extension Set._Variant {
  @inlinable
  @inline(__always)
  internal mutating func isUniquelyReferenced() -> Bool {
    switch self {
    case .native:
      // Note that `&self` drills down through `.native(_NativeSet)` to the
      // first property in `_NativeSet`, which is the reference to the storage.
      return _isUnique_native(&self)
#if _runtime(_ObjC)
    case .cocoa:
      // Don't consider Cocoa buffer mutable, even if it is mutable and is
      // uniquely referenced.
      return false
#endif
    }
  }

  @inlinable
  internal var asNative: _NativeSet<Element> {
    @inline(__always)
    get {
      switch self {
      case .native(let nativeSet):
        return nativeSet
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
  internal var asCocoa: _CocoaSet {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSSet")
    case .cocoa(let cocoaSet):
      return cocoaSet
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
      self = .native(_NativeSet(cocoa, capacity: capacity))
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
    case .cocoa(let cocoaSet):
      return cocoaSet.count
#endif
    }
  }
}

extension Set._Variant: _SetBuffer {
  @usableFromInline
  internal typealias Index = Set<Element>.Index

  @inlinable
  internal var startIndex: Index {
    switch self {
    case .native:
      return Index(_native: asNative.startIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      return Index(_cocoa: cocoaSet.startIndex)
#endif
    }
  }

  @inlinable
  internal var endIndex: Index {
    switch self {
    case .native:
      return Index(_native: asNative.endIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      return Index(_cocoa: cocoaSet.endIndex)
#endif
    }
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    switch self {
    case .native:
      return Index(_native: asNative.index(after: i._asNative))
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      return Index(_cocoa: cocoaSet.index(after: i._asCocoa))
#endif
    }
  }

  @inlinable
  internal func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable
  @inline(__always)
  internal func index(for element: Element) -> Index? {
    switch self {
    case .native:
      if let index = asNative.index(for: element) {
        return Index(_native: index)
      }
      return nil
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      let cocoaElement = _bridgeAnythingToObjectiveC(element)
      if let index = cocoa.index(for: cocoaElement) {
        return Index(_cocoa: index)
      }
      return nil
#endif
    }
  }

  @inlinable
  internal var count: Int {
    switch self {
    case .native:
      return asNative.count
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      return cocoaSet.count
#endif
    }
  }

  @inlinable
  @inline(__always)
  internal func contains(_ member: Element) -> Bool {
    switch self {
    case .native:
      return asNative.contains(member)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return cocoa.contains(_bridgeAnythingToObjectiveC(member))
#endif
    }
  }

  @inlinable
  internal func element(at i: Index) -> Element {
    switch self {
    case .native:
      return asNative.element(at: i._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      let cocoaMember = cocoa.element(at: i._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaMember, Element.self)
#endif
    }
  }
}

extension Set._Variant {
  @inlinable
  internal mutating func update(with value: Element) -> Element? {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      return asNative.update(with: value, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      // Make sure we have space for an extra element.
      var native = _NativeSet<Element>(cocoa, capacity: cocoa.count + 1)
      let old = native.update(with: value, isUnique: true)
      self = .native(native)
      return old
#endif
    }
  }

  @inlinable
  internal mutating func insert(
    _ element: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      return asNative.insert(element, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      // Make sure we have space for an extra element.
      var native = _NativeSet<Element>(cocoa, capacity: cocoa.count + 1)
      let result = native.insert(element, isUnique: true)
      self = .native(native)
      return result
#endif
    }
  }

  @inlinable
  @discardableResult
  internal mutating func remove(at index: Index) -> Element {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      return asNative.remove(at: index._asNative, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      //
      // FIXME(performance): fuse data migration and element deletion into one
      // operation.
      let index = index._asCocoa
      let cocoaMember = index.allKeys[index.currentKeyIndex]
      var native = _NativeSet<Element>(cocoaSet)
      let nativeMember = _forceBridgeFromObjectiveC(cocoaMember, Element.self)
      let old = native.remove(nativeMember, isUnique: true)
      _sanityCheck(nativeMember == old, "bridging did not preserve equality")
      self = .native(native)
      return nativeMember
#endif
    }
  }

  @inlinable
  @discardableResult
  internal mutating func remove(_ member: Element) -> Element? {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      return asNative.remove(member, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      let cocoaMember = _bridgeAnythingToObjectiveC(member)
      if !cocoa.contains(cocoaMember) {
        return nil
      }
      var native = _NativeSet<Element>(cocoa)
      let old = native.remove(member, isUnique: true)
      self = .native(native)
      _sanityCheck(old != nil, "bridging did not preserve equality")
      return old
#endif
    }
  }

  @inlinable
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if !keepCapacity {
      self = .native(_NativeSet<Element>())
      return
    }
    if count == 0 {
      return
    }

    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.removeAll(isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      self = .native(_NativeSet(capacity: cocoaSet.count))
#endif
    }
  }
}

extension Set._Variant {
  @inlinable
  internal mutating func formUnion<S: Sequence>(_ other: S)
    where S.Element == Element {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.formUnion(other, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let this):
      if let other = other as? Set<Element>,
        case .native(var that) = other._variant {
        // When given a choice, form the union on the native set.
        that.formUnion(Set(_cocoa: this), isUnique: false)
        self = .native(that)
        return
      }
      var native = _NativeSet<Element>(this)
      native.formUnion(other, isUnique: true)
      self = .native(native)
#endif
    }
  }

  @inlinable
  internal func subtracting<S: Sequence>(_ other: S) -> _NativeSet<Element>
  where S.Element == Element {
    switch self {
    case .native(let native):
      return native.subtracting(other)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return _NativeSet(cocoa).subtracting(other)
#endif
    }
  }

  @inlinable
  internal func intersection<S: Sequence>(_ other: S) -> _NativeSet<Element>
    where S.Element == Element {
    if let other = other as? Set<Element> {
      return self.intersection(other)
    }
    switch self {
    case .native(let native):
      return native.intersection(other)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return _NativeSet(cocoa).intersection(other)
#endif
    }
  }

  @inlinable
  internal func intersection(_ other: Set<Element>) -> _NativeSet<Element> {
    switch (self, other._variant) {
    case (.native(let this), .native(let that)):
      // Prefer to iterate over the smaller set.
      if this.count < that.count {
        return that.intersection(this)
      } else {
        return this.intersection(that)
      }
#if _runtime(_ObjC)
    case (.cocoa(let this), .native(let that)):
      return that.intersection(Set<Element>(_cocoa: this))
    case (.native(let this), .cocoa(_)):
      return this.intersection(other)
    case (.cocoa(let this), .cocoa(_)):
      return _NativeSet<Element>(this).intersection(other)
#endif
    }
  }

  @inlinable
  internal func symmetricDifference<S: Sequence>(
    _ other: S
  ) -> _NativeSet<Element> where S.Element == Element {
    switch self {
    case .native(let native):
      return native.symmetricDifference(other)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return _NativeSet<Element>(cocoa).symmetricDifference(other)
#endif
    }
  }

  @inlinable
  internal func symmetricDifference(
    _ other: Set<Element>
  ) -> _NativeSet<Element> {
    switch (self, other._variant) {
    case (.native(let this), .native(let that)):
      return this.symmetricDifference(that)
#if _runtime(_ObjC)
    case (.native(let this), .cocoa(_)):
      return this.symmetricDifference(other)
    case (.cocoa(let this), .native(let that)):
      return that.symmetricDifference(Set<Element>(_cocoa: this))
    case (.cocoa(let this), .cocoa(_)):
      return _NativeSet<Element>(this).symmetricDifference(other)
#endif
    }
  }
}

extension Set._Variant {
  /// Returns an iterator over the elements.
  ///
  /// - Complexity: O(1).
  @inlinable
  @inline(__always)
  internal func makeIterator() -> Set<Element>.Iterator {
    switch self {
    case .native(let nativeSet):
      return ._native(nativeSet.makeIterator())
#if _runtime(_ObjC)
    case .cocoa(let cocoaSet):
      return ._cocoa(cocoaSet.makeIterator())
#endif
    }
  }
}

#if _runtime(_ObjC)
extension _CocoaSet {
  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline
  internal struct Index {
    // Assumption: we rely on NSDictionary.getObjects when being
    // repeatedly called on the same NSDictionary, returning items in the same
    // order every time.
    // Similarly, the same assumption holds for NSSet.allObjects.

    /// A reference to the NSSet, which owns members in `allObjects`,
    /// or `allKeys`, for NSSet and NSDictionary respectively.
    @usableFromInline // FIXME(sil-serialize-all)
    internal let base: _CocoaSet
    // FIXME: swift-3-indexing-model: try to remove the cocoa reference, but
    // make sure that we have a safety check for accessing `allKeys`.  Maybe
    // move both into the dictionary/set itself.

    /// An unowned array of keys.
    @usableFromInline // FIXME(sil-serialize-all)
    internal var allKeys: _HeapBuffer<Int, AnyObject>

    /// Index into `allKeys`
    @usableFromInline // FIXME(sil-serialize-all)
    internal var currentKeyIndex: Int

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ base: _CocoaSet, startIndex: ()) {
      self.base = base
      self.allKeys = _stdlib_NSSet_allObjects(base.object)
      self.currentKeyIndex = 0
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ base: _CocoaSet, endIndex: ()) {
      self.base = base
      self.allKeys = _stdlib_NSSet_allObjects(base.object)
      self.currentKeyIndex = allKeys.value
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ base: _CocoaSet,
      _ allKeys: _HeapBuffer<Int, AnyObject>,
      _ currentKeyIndex: Int
    ) {
      self.base = base
      self.allKeys = allKeys
      self.currentKeyIndex = currentKeyIndex
    }
  }
}

extension _CocoaSet.Index: Equatable {
  @inlinable // FIXME(sil-serialize-all)
  internal static func == (lhs: _CocoaSet.Index, rhs: _CocoaSet.Index) -> Bool {
    return lhs.currentKeyIndex == rhs.currentKeyIndex
  }
}

extension _CocoaSet.Index: Comparable {
  @inlinable // FIXME(sil-serialize-all)
  internal static func < (lhs: _CocoaSet.Index, rhs: _CocoaSet.Index) -> Bool {
    return lhs.currentKeyIndex < rhs.currentKeyIndex
  }
}
#endif

extension Set {
  /// The position of an element in a set.
  @_fixed_layout
  public struct Index {
    // Index for native buffer is efficient.  Index for bridged NSSet is
    // not, because neither NSEnumerator nor fast enumeration support moving
    // backwards.  Even if they did, there is another issue: NSEnumerator does
    // not support NSCopying, and fast enumeration does not document that it is
    // safe to copy the state.  So, we cannot implement Index that is a value
    // type for bridged NSSet in terms of Cocoa enumeration facilities.

    @_frozen
    @usableFromInline
    internal enum _Variant {
      case native(_NativeSet<Element>.Index)
#if _runtime(_ObjC)
      case cocoa(_CocoaSet.Index)
#endif
    }

    @usableFromInline
    internal var _variant: _Variant

    @inlinable
    internal init(_variant: _Variant) {
      self._variant = _variant
    }
  }
}

extension Set.Index {
  @inlinable
  internal init(_native index: _NativeSet<Element>.Index) {
    self.init(_variant: .native(index))
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_cocoa index: _CocoaSet.Index) {
    self.init(_variant: .cocoa(index))
  }
#endif

  @usableFromInline @_transparent
  internal var _asNative: _NativeSet<Element>.Index {
    switch _variant {
    case .native(let nativeIndex):
      return nativeIndex
#if _runtime(_ObjC)
    case .cocoa:
      _sanityCheckFailure("internal error: does not contain a native index")
#endif
    }
  }

#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var _asCocoa: _CocoaSet.Index {
    switch _variant {
    case .native:
      _sanityCheckFailure("internal error: does not contain a Cocoa index")
    case .cocoa(let cocoaIndex):
      return cocoaIndex
    }
  }
#endif
}

extension Set.Index: Equatable {
  @inlinable
  public static func == (
    lhs: Set<Element>.Index,
    rhs: Set<Element>.Index
  ) -> Bool {
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative == rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }
}

extension Set.Index: Comparable {
  @inlinable
  public static func < (
    lhs: Set<Element>.Index,
    rhs: Set<Element>.Index
  ) -> Bool {
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative < rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa < rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }
}

extension Set.Index: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
  #if _runtime(_ObjC)
    switch _variant {
    case .native(let nativeIndex):
      hasher.combine(0 as UInt8)
      hasher.combine(nativeIndex.bucket)
    case .cocoa(let cocoaIndex):
      hasher.combine(1 as UInt8)
      hasher.combine(cocoaIndex.currentKeyIndex)
    }
  #else
    hasher.combine(_asNative.bucket)
  #endif
  }
}

extension _NativeSet: Sequence {
  @usableFromInline
  @_fixed_layout
  internal struct Iterator {
    // Iterator is iterating over a frozen view of the collection
    // state, so it should keep its own reference to the buffer.
    @usableFromInline
    internal var index: Index
    @usableFromInline
    internal var endIndex: Index
    @usableFromInline
    internal let base: _NativeSet

    @inlinable
    init(_ base: _NativeSet) {
      self.index = base.startIndex
      self.endIndex = base.endIndex
      self.base = base
    }
  }

  @inlinable
  internal func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _NativeSet.Iterator: IteratorProtocol {
  @inlinable
  internal mutating func next() -> Element? {
    guard index != endIndex else { return nil }
    let result = base.element(at: index)
    base.formIndex(after: &index)
    return result
  }
}

#if _runtime(_ObjC)
extension _CocoaSet: Sequence {
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

    internal let base: _CocoaSet

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

    internal init(_ base: _CocoaSet) {
      self.base = base
    }
  }

  @usableFromInline
  @_effects(releasenone)
  internal func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _CocoaSet.Iterator: IteratorProtocol {
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
#endif

extension Set {
  /// An iterator over the members of a `Set<Element>`.
  @_fixed_layout
  public struct Iterator {
    // Set has a separate IteratorProtocol and Index because of efficiency
    // and implementability reasons.
    //
    // Native sets have efficient indices.  Bridged NSSet instances don't.
    //
    // Even though fast enumeration is not suitable for implementing
    // Index, which is multi-pass, it is suitable for implementing a
    // IteratorProtocol, which is being consumed as iteration proceeds.

    @usableFromInline
    @_frozen
    internal enum _Variant {
      case native(_NativeSet<Element>.Iterator)
#if _runtime(_ObjC)
      case cocoa(_CocoaSet.Iterator)
#endif
    }

    @usableFromInline
    internal var _variant: _Variant

    @inlinable
    internal init(_variant: _Variant) {
      self._variant = _variant
    }
  }
}

extension Set.Iterator {
  @inlinable
  internal static func _native(
    _ iterator: _NativeSet<Element>.Iterator
  ) -> Set.Iterator {
    return Set.Iterator(_variant: .native(iterator))
  }

#if _runtime(_ObjC)
  @usableFromInline
  internal static func _cocoa(_ iterator: _CocoaSet.Iterator) -> Set.Iterator {
    return Set.Iterator(_variant: .cocoa(iterator))
  }
#endif

  @usableFromInline @_transparent
  internal var _asNative: _NativeSet<Element>.Iterator {
    get {
      switch _variant {
      case .native(let nativeIterator):
        return nativeIterator
#if _runtime(_ObjC)
      case .cocoa:
        _sanityCheckFailure("internal error: does not contain a native index")
#endif
      }
    }
    set {
      self._variant = .native(newValue)
    }
  }
}

extension Set.Iterator: IteratorProtocol {
  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable
  @inline(__always)
  public mutating func next() -> Element? {
    switch _variant {
    case .native:
      return _asNative.next()
#if _runtime(_ObjC)
    case .cocoa(let cocoaIterator):
      if let cocoaElement = cocoaIterator.next() {
        return _forceBridgeFromObjectiveC(cocoaElement, Element.self)
      }
      return nil
#endif
    }
  }
}

extension Set.Iterator: CustomReflectable {
  /// A mirror that reflects the iterator.
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: EmptyCollection<(label: String?, value: Any)>())
  }
}

extension Set: CustomReflectable {
  /// A mirror that reflects the set.
  public var customMirror: Mirror {
    let style = Mirror.DisplayStyle.`set`
    return Mirror(self, unlabeledChildren: self, displayStyle: style)
  }
}

/// Initializes a `Set` from unique members.
///
/// Using a builder can be faster than inserting members into an empty
/// `Set`.
@_fixed_layout
public struct _SetBuilder<Element: Hashable> {
  @usableFromInline
  internal var _target: _NativeSet<Element>
  @usableFromInline
  internal let _requestedCount: Int

  @inlinable
  public init(count: Int) {
    _target = _NativeSet(capacity: count)
    _requestedCount = count
  }

  @inlinable
  public mutating func add(member: Element) {
    _precondition(_target.count < _requestedCount,
      "Can't add more members than promised")
    _target.insertNew(member)
  }

  @inlinable
  public mutating func take() -> Set<Element> {
    _precondition(_target.capacity > 0 || _requestedCount == 0,
      "Cannot take the result twice")
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")

    // Prevent taking the result twice.
    var result = _NativeSet<Element>()
    swap(&result, &_target)
    return Set(_native: result)
  }
}

extension Set {
  /// Removes and returns the first element of the set.
  ///
  /// Because a set is not an ordered collection, the "first" element may not
  /// be the first element that was added to the set.
  ///
  /// - Returns: A member of the set. If the set is empty, returns `nil`.
  @inlinable
  public mutating func popFirst() -> Element? {
    guard !isEmpty else { return nil }
    return remove(at: startIndex)
  }

  /// The total number of elements that the set can contain without
  /// allocating new storage.
  @inlinable
  public var capacity: Int {
    return _variant.capacity
  }

  /// Reserves enough space to store the specified number of elements.
  ///
  /// If you are adding a known number of elements to a set, use this
  /// method to avoid multiple reallocations. This method ensures that the
  /// set has unique, mutable, contiguous storage, with space allocated
  /// for at least the requested number of elements.
  ///
  /// Calling the `reserveCapacity(_:)` method on a set with bridged
  /// storage triggers a copy to contiguous storage even if the existing
  /// storage has room to store `minimumCapacity` elements.
  ///
  /// - Parameter minimumCapacity: The requested number of elements to
  ///   store.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func reserveCapacity(_ minimumCapacity: Int) {
    _variant.reserveCapacity(minimumCapacity)
    _sanityCheck(self.capacity >= minimumCapacity)
  }
}

//===--- Bridging ---------------------------------------------------------===//

#if _runtime(_ObjC)
extension Set {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveCImpl() -> _NSSetCore {
    switch _variant {
    case .native(let nativeSet):
      return nativeSet.bridged()
    case .cocoa(let cocoaSet):
      return cocoaSet.object
    }
  }

  /// Returns the native `Set` hidden inside the given `NSSet` instance; returns
  /// `nil` otherwise.
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ s: AnyObject
  ) -> Set<Element>? {
    // Try all three NSSet impls that we currently provide.
    if let deferred = s as? _SwiftDeferredNSSet<Element> {
      return Set(_native: deferred.native)
    }
    if let nativeStorage = s as? _SwiftNativeSetStorage<Element> {
      return Set(_native: _NativeSet(nativeStorage))
    }
    if s === _SwiftRawSetStorage.empty {
      return Set()
    }
    // FIXME: what if `s` is native storage, but for different element type?
    return nil
  }
}
#endif

public typealias SetIndex<Element: Hashable> = Set<Element>.Index
public typealias SetIterator<Element: Hashable> = Set<Element>.Iterator

extension Set {
  public // @testable performance metrics
  var _stats: (maxLookups: Int, averageLookups: Double)? {
    guard case .native(let native) = _variant else { return nil }
    defer { _fixLifetime(self) }
    var maxChainLength = 0
    var sumChainLength = 0
    for i in native.indices {
      let delta = native.displacement(ofElementAt: i)
      maxChainLength = Swift.max(maxChainLength, delta + 1)
      sumChainLength += delta + 1
    }
    return (maxChainLength, Double(sumChainLength) / Double(count))
  }
}

extension _NativeSet {
  internal func displacement(ofElementAt index: Index) -> Int {
    let element = uncheckedElement(at: index)
    let hashValue = self.hashValue(for: element)
    let ideal = hashValue & _storage.hashTable.bucketMask
    let delta = ideal <= index.bucket
      ? index.bucket - ideal
      : index.bucket - ideal + bucketCount
    return delta
  }
}

extension _NativeSet {
  // FIXME: Remove
  internal func _dump() -> String {
    var result = ""
    defer { _fixLifetime(self) }
    let hashTable = _storage.hashTable
    let bits = String(unsafeBitCast(self, to: UInt.self), radix: 16)
    result += "Native set of \(Element.self) at 0x\(bits)\n"
    result += "  count: \(hashTable.count)\n"
    result += "  capacity: \(hashTable.capacity)\n"
    result += "  scale: \(hashTable.scale)\n"
    result += "  bucketCount: \(hashTable.bucketCount)\n"
    for i in indices {
      let element = uncheckedElement(at: i)
      let hashValue = self.hashValue(for: element)
      let delta = self.displacement(ofElementAt: i)
      let h = String(UInt(bitPattern: hashValue), radix: 16)
      let hl = String(hashTable.map[i.bucket].payload << 1, radix: 16)
      result += "  <bucket \(i.bucket)> "
      result += "delta: \(delta), "
      result += "hash: \(hl)/\(h): "
      result += "\(element)\n"
    }
    return result
  }
}

#if _runtime(_ObjC)
extension _CocoaSet {
  // FIXME: Remove
  internal func _dump() -> String {
    let bits = String(unsafeBitCast(self, to: UInt.self), radix: 16)
    return "Cocoa set of \(Element.self) at 0x\(bits)\n"
  }
}
#endif

extension Set {
  // FIXME: Remove
  public func _dump() {
    switch _variant {
    case .native(let native):
      print("\(native._dump())")
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      print("\(cocoa._dump())")
#endif
    }
  }
}

extension Set {
  @usableFromInline // @testable
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    guard case .native(let native) = self._variant else {
      return
    }
    native._invariantCheck()
#endif
  }
}

extension _NativeSet {
  @usableFromInline // @testable
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(
      _storage === _SwiftRawSetStorage.empty ||
      _isValidAddress(UInt(bitPattern: _storage.rawElements)),
    "Invalid rawElements pointer")
    _storage.hashTable._invariantCheck(with: self)
#endif
  }
}
