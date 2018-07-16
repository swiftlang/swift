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
  internal typealias _VariantBuffer = _VariantSetBuffer<Element>
  @usableFromInline
  internal typealias _NativeBuffer = _NativeSetBuffer<Element>

  @usableFromInline
  internal var _variantBuffer: _VariantBuffer
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
  @inlinable // FIXME(sil-serialize-all)
  public init(minimumCapacity: Int) {
    _variantBuffer =
      _VariantBuffer.native(
        _NativeBuffer(minimumCapacity: minimumCapacity))
  }

  /// Private initializer.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_nativeBuffer: _NativeSetBuffer<Element>) {
    _variantBuffer = _VariantBuffer.native(_nativeBuffer)
  }

  //
  // All APIs below should dispatch to `_variantBuffer`, without doing any
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
  @inlinable // FIXME(sil-serialize-all)
  public init(_immutableCocoaSet: _NSSet) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self),
      "Set can be backed by NSSet _variantBuffer only when the member type can be bridged verbatim to Objective-C")
    _variantBuffer = _VariantSetBuffer.cocoa(
      _CocoaSetBuffer(cocoaSet: _immutableCocoaSet))
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
  @inlinable // FIXME(sil-serialize-all)
  public init(arrayLiteral elements: Element...) {
    self.init(_nativeBuffer: _NativeSetBuffer.fromArray(elements))
  }
}

extension Set: Sequence {
  /// Returns an iterator over the members of the set.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public func makeIterator() -> SetIterator<Element> {
    return _variantBuffer.makeIterator()
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
  @inlinable // FIXME(sil-serialize-all)
  public func contains(_ member: Element) -> Bool {
    return _variantBuffer.maybeGet(member) != nil
  }

  @inlinable // FIXME(sil-serialize-all)
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
    var result = Set()
    for element in self {
      if try isIncluded(element) {
        result.insert(element)
      }
    }
    return result
  }
}

extension Set: Collection {
  /// The starting position for iterating members of the set.
  ///
  /// If the set is empty, `startIndex` is equal to `endIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return _variantBuffer.startIndex
  }

  /// The "past the end" position for the set---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the set is empty, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return _variantBuffer.endIndex
  }

  /// Accesses the member at the given position.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(position: Index) -> Element {
    return _variantBuffer.assertingGet(at: position)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    return _variantBuffer.index(after: i)
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
  @inlinable // FIXME(sil-serialize-all)
  public func firstIndex(of member: Element) -> Index? {
    return _variantBuffer.index(forKey: member)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func _customIndexOfEquatableElement(
     _ member: Element
    ) -> Index?? {
    return Optional(firstIndex(of: member))
  }

  @inlinable // FIXME(sil-serialize-all)
  public func _customLastIndexOfEquatableElement(
     _ member: Element
    ) -> Index?? {
    // The first and last elements are the same because each element is unique.
    return _customIndexOfEquatableElement(member)
  }

  /// The number of elements in the set.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  public var count: Int {
    return _variantBuffer.count
  }

  /// A Boolean value that indicates whether the set is empty.
  @inlinable // FIXME(sil-serialize-all)
  public var isEmpty: Bool {
    return count == 0
  }

  /// The first element of the set.
  ///
  /// The first element of the set is not necessarily the first element added
  /// to the set. Don't expect any particular ordering of set elements.
  ///
  /// If the set is empty, the value of this property is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public var first: Element? {
    return count > 0 ? self[startIndex] : nil
  }
}

/// Check for both subset and equality relationship between
/// a set and some sequence (which may itself be a `Set`).
///
/// (isSubset: lhs ⊂ rhs, isEqual: lhs ⊂ rhs and |lhs| = |rhs|)
@inlinable
internal func _compareSets<Element>(_ lhs: Set<Element>, _ rhs: Set<Element>)
  -> (isSubset: Bool, isEqual: Bool) {
  // FIXME(performance): performance could be better if we start by comparing
  // counts.
  for member in lhs {
    if !rhs.contains(member) {
      return (false, false)
    }
  }
  return (true, lhs.count == rhs.count)
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
  @inlinable // FIXME(sil-serialize-all)
  public static func == (lhs: Set<Element>, rhs: Set<Element>) -> Bool {
    switch (lhs._variantBuffer, rhs._variantBuffer) {
    case (.native(let lhsNative), .native(let rhsNative)):

      if lhsNative._storage === rhsNative._storage {
        return true
      }

      if lhsNative.count != rhsNative.count {
        return false
      }

      for member in lhs {
        let (_, found) =
          rhsNative._find(member, startBucket: rhsNative._bucket(member))
        if !found {
          return false
        }
      }
      return true

  #if _runtime(_ObjC)
    case (_VariantSetBuffer.cocoa(let lhsCocoa),
        _VariantSetBuffer.cocoa(let rhsCocoa)):
      return _stdlib_NSObject_isEqual(lhsCocoa.cocoaSet, rhsCocoa.cocoaSet)

    case (_VariantSetBuffer.native(let lhsNative),
      _VariantSetBuffer.cocoa(let rhsCocoa)):

      if lhsNative.count != rhsCocoa.count {
        return false
      }

      let endIndex = lhsNative.endIndex
      var i = lhsNative.startIndex
      while i != endIndex {
        let key = lhsNative.assertingGet(at: i)
        let bridgedKey: AnyObject = _bridgeAnythingToObjectiveC(key)
        let optRhsValue: AnyObject? = rhsCocoa.maybeGet(bridgedKey)
        if let rhsValue = optRhsValue {
          if key == _forceBridgeFromObjectiveC(rhsValue, Element.self) {
            i = lhsNative.index(after: i)
            continue
          }
        }
        i = lhsNative.index(after: i)
        return false
      }
      return true

    case (_VariantSetBuffer.cocoa, _VariantSetBuffer.native):
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
    // FIXME(ABI)#177: <rdar://problem/18915294> Cache Set<T> hashValue
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
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func insert(
    _ newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    return _variantBuffer.insert(newMember)
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
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func update(with newMember: Element) -> Element? {
    return _variantBuffer.update(with: newMember)
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
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func remove(_ member: Element) -> Element? {
    return _variantBuffer.remove(member)
  }

  /// Removes the element at the given index of the set.
  ///
  /// - Parameter position: The index of the member to remove. `position` must
  ///   be a valid index of the set, and must not be equal to the set's end
  ///   index.
  /// - Returns: The element that was removed from the set.
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func remove(at position: Index) -> Element {
    return _variantBuffer.remove(at: position)
  }

  /// Removes all members from the set.
  ///
  /// - Parameter keepingCapacity: If `true`, the set's buffer capacity is
  ///   preserved; if `false`, the underlying buffer is released. The
  ///   default is `false`.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _variantBuffer.removeAll(keepingCapacity: keepCapacity)
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
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func removeFirst() -> Element {
    _precondition(!isEmpty, "Can't removeFirst from an empty Set")
    return remove(at: startIndex)
  }

  //
  // APIs below this comment should be implemented strictly in terms of
  // *public* APIs above.  `_variantBuffer` should not be accessed directly.
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
  @inlinable // FIXME(sil-serialize-all)
  public init() {
    self = Set<Element>(_nativeBuffer: _NativeBuffer())
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
  @inlinable // FIXME(sil-serialize-all)
  public init<Source: Sequence>(_ sequence: Source)
    where Source.Element == Element {
    self.init(minimumCapacity: sequence.underestimatedCount)
    if let s = sequence as? Set<Element> {
      // If this sequence is actually a native `Set`, then we can quickly
      // adopt its native buffer and let COW handle uniquing only
      // if necessary.
      switch s._variantBuffer {
        case .native(let buffer):
          _variantBuffer = .native(buffer)
#if _runtime(_ObjC)
        case .cocoa(let owner):
          _variantBuffer = .cocoa(owner)
#endif
      }
    } else {
      for item in sequence {
        insert(item)
      }
    }
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
    // FIXME(performance): isEmpty fast path, here and elsewhere.
    let other = Set(possibleSuperset)
    return isSubset(of: other)
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
    // FIXME: code duplication.
    let other = Set(possibleStrictSuperset)
    return isStrictSubset(of: other)
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
    // FIXME(performance): Don't build a set; just ask if every element is in
    // `self`.
    let other = Set(possibleSubset)
    return other.isSubset(of: self)
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
  @inlinable // FIXME(sil-serialize-all)
  public func isStrictSuperset<S: Sequence>(of possibleStrictSubset: S) -> Bool
    where S.Element == Element {
    let other = Set(possibleStrictSubset)
    return other.isStrictSubset(of: self)
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
  @inlinable // FIXME(sil-serialize-all)
  public func isDisjoint<S: Sequence>(with other: S) -> Bool
    where S.Element == Element {
    // FIXME(performance): Don't need to build a set.
    let otherSet = Set(other)
    return isDisjoint(with: otherSet)
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
    for item in other {
      insert(item)
    }
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
    return self._subtracting(other)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _subtracting<S: Sequence>(_ other: S) -> Set<Element>
    where S.Element == Element {
    var newSet = self
    newSet.subtract(other)
    return newSet
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
  @inlinable // FIXME(sil-serialize-all)
  public mutating func subtract<S: Sequence>(_ other: S)
    where S.Element == Element {
    _subtract(other)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func _subtract<S: Sequence>(_ other: S)
    where S.Element == Element {
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
    let otherSet = Set(other)
    return intersection(otherSet)
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
    // Because `intersect` needs to both modify and iterate over
    // the left-hand side, the index may become invalidated during
    // traversal so an intermediate set must be created.
    //
    // FIXME(performance): perform this operation at a lower level
    // to avoid invalidating the index and avoiding a copy.
    let result = self.intersection(other)

    // The result can only have fewer or the same number of elements.
    // If no elements were removed, don't perform a reassignment
    // as this may cause an unnecessary uniquing COW.
    if result.count != count {
      self = result
    }
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
    var newSet = self
    newSet.formSymmetricDifference(other)
    return newSet
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
    let otherSet = Set(other)
    formSymmetricDifference(otherSet)
  }
}

extension Set: CustomStringConvertible, CustomDebugStringConvertible {
  /// A string that represents the contents of the set.
  @inlinable // FIXME(sil-serialize-all)
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
@usableFromInline // FIXME(sil-serialize-all)
internal
func _stdlib_CFSetGetValues(_ nss: _NSSet, _: UnsafeMutablePointer<AnyObject>)

/// Equivalent to `NSSet.allObjects`, but does not leave objects on the
/// autorelease pool.
@inlinable // FIXME(sil-serialize-all)
internal func _stdlib_NSSet_allObjects(_ nss: _NSSet) ->
  _HeapBuffer<Int, AnyObject> {
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
@inlinable // FIXME(sil-serialize-all)
public func _setUpCast<DerivedValue, BaseValue>(_ source: Set<DerivedValue>)
  -> Set<BaseValue> {
  var builder = _SetBuilder<BaseValue>(count: source.count)
  for x in source {
    builder.add(member: x as! BaseValue)
  }
  return builder.take()
}

#if _runtime(_ObjC)

/// Implements an unconditional upcast that involves bridging.
///
/// The cast can fail if bridging fails.
///
/// - Precondition: `SwiftValue` is bridged to Objective-C
///   and requires non-trivial bridging.
@inlinable // FIXME(sil-serialize-all)
public func _setBridgeToObjectiveC<SwiftValue, ObjCValue>(
  _ source: Set<SwiftValue>
) -> Set<ObjCValue> {
  _sanityCheck(_isClassOrObjCExistential(ObjCValue.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(SwiftValue.self))

  var result = Set<ObjCValue>(minimumCapacity: source.count)
  let valueBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
    _isBridgedVerbatimToObjectiveC(ObjCValue.self)

  for member in source {
    var bridgedMember: ObjCValue
    if valueBridgesDirectly {
      bridgedMember = unsafeBitCast(member, to: ObjCValue.self)
    } else {
      let bridged: AnyObject = _bridgeAnythingToObjectiveC(member)
      bridgedMember = unsafeBitCast(bridged, to: ObjCValue.self)
    }
    result.insert(bridgedMember)
  }
  return result
}
#endif

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
@inlinable // FIXME(sil-serialize-all)
public func _setDownCast<BaseValue, DerivedValue>(_ source: Set<BaseValue>)
  -> Set<DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedValue.self) {
    switch source._variantBuffer {
    case _VariantSetBuffer.native(let buffer):
      return Set(_immutableCocoaSet: buffer.bridged())
    case _VariantSetBuffer.cocoa(let cocoaBuffer):
      return Set(_immutableCocoaSet: cocoaBuffer.cocoaSet)
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
@inlinable // FIXME(sil-serialize-all)
public func _setDownCastConditional<BaseValue, DerivedValue>(
  _ source: Set<BaseValue>
) -> Set<DerivedValue>? {
  var result = Set<DerivedValue>(minimumCapacity: source.count)
  for member in source {
    if let derivedMember = member as? DerivedValue {
      result.insert(derivedMember)
      continue
    }
    return nil
  }
  return result
}

#if _runtime(_ObjC)

/// Implements an unconditional downcast that involves bridging.
///
/// - Precondition: At least one of `SwiftValue` is a bridged value
///   type, and the corresponding `ObjCValue` is a reference type.
@inlinable // FIXME(sil-serialize-all)
public func _setBridgeFromObjectiveC<ObjCValue, SwiftValue>(
  _ source: Set<ObjCValue>
) -> Set<SwiftValue> {
  let result: Set<SwiftValue>? = _setBridgeFromObjectiveCConditional(source)
  _precondition(result != nil, "This set cannot be bridged from Objective-C")
  return result!
}

/// Implements a conditional downcast that involves bridging.
///
/// If the cast fails, the function returns `nil`.  All checks should be
/// performed eagerly.
///
/// - Precondition: At least one of `SwiftValue` is a bridged value
///   type, and the corresponding `ObjCValue` is a reference type.
@inlinable // FIXME(sil-serialize-all)
public func _setBridgeFromObjectiveCConditional<
  ObjCValue, SwiftValue
>(
  _ source: Set<ObjCValue>
) -> Set<SwiftValue>? {
  _sanityCheck(_isClassOrObjCExistential(ObjCValue.self))
  _sanityCheck(!_isBridgedVerbatimToObjectiveC(SwiftValue.self))

  let valueBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
      _isBridgedVerbatimToObjectiveC(ObjCValue.self)

  var result = Set<SwiftValue>(minimumCapacity: source.count)
  for value in source {
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
    result.insert(resultValue)
  }
  return result
}
#endif

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
  @inlinable // FIXME(sil-serialize-all)
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
    let (isSubset, isEqual) = _compareSets(self, other)
    return isSubset || isEqual
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
    for member in self {
      if other.contains(member) {
        return false
      }
    }
    return true
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
    return self._subtracting(other)
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
    return self.isSuperset(of: other) && self != other
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
    var newSet = Set<Element>()
    for member in self {
      if other.contains(member) {
        newSet.insert(member)
      }
    }
    return newSet
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
    for member in other {
      if contains(member) {
        remove(member)
      } else {
        insert(member)
      }
    }
  }
}

//===--- APIs templated for Dictionary and Set ----------------------------===//

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _SetBuffer { // FIXME: Remove or refactor for Set.
  associatedtype Element
  associatedtype Index

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func formIndex(after i: inout Index)
  func index(forKey key: Element) -> Index?
  var count: Int { get }

  func assertingGet(at i: Index) -> Element
  func maybeGet(_ key: Element) -> Element?
}

/// An instance of this class has all `Set` data tail-allocated.
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
@usableFromInline // FIXME(sil-serialize-all)
@_objc_non_lazy_realization
internal class _RawNativeSetStorage: _SwiftNativeNSSet, _NSSetCore {
  @usableFromInline // FIXME(sil-serialize-all)
  @nonobjc
  internal final var bucketCount: Int

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var count: Int

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var initializedEntries: _UnsafeBitMap

  @usableFromInline // FIXME(sil-serialize-all)
  @nonobjc
  internal final var keys: UnsafeMutableRawPointer

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var seed: (UInt64, UInt64)

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal final
  var _initializedHashtableEntriesBitMapBuffer: UnsafeMutablePointer<UInt> {
    return UnsafeMutablePointer(Builtin.projectTailElems(self, UInt.self))
  }

  /// The empty singleton that is used for every single Dictionary that is
  /// created without any elements. The contents of the storage should never
  /// be mutated.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal static var empty: _RawNativeSetStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptySetStorage))
  }

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by using the `empty` singleton")
  }

#if _runtime(_ObjC)
  //
  // NSSet implementation, assuming Self is the empty singleton
  //

  /// Get the NSEnumerator implementation for self.
  /// _HashableTypedNativeSetStorage overloads this to give
  /// _NativeSelfNSEnumerator proper type parameters.
  @objc
  internal func enumerator() -> _NSEnumerator {
    return _NativeSetNSEnumerator<AnyObject>(
        _NativeSetBuffer(_storage: self))
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @inlinable // FIXME(sil-serialize-all)
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

  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal func member(_ object: AnyObject) -> AnyObject? {
    return nil
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    return enumerator()
  }
#endif
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal class _TypedNativeSetStorage<Element>: _RawNativeSetStorage {

  deinit {
    let keys = self.keys.assumingMemoryBound(to: Element.self)

    if !_isPOD(Element.self) {
      for i in 0 ..< bucketCount {
        if initializedEntries[i] {
          (keys+i).deinitialize(count: 1)
        }
      }
    }

    _fixLifetime(self)
  }

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits")
  }

#if _runtime(_ObjC)
  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("don't call this designated initializer")
  }
#endif
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
final internal class _HashableTypedNativeSetStorage<Element: Hashable>
  : _TypedNativeSetStorage<Element> {

  @usableFromInline
  internal typealias FullContainer = Set<Element>
  @usableFromInline
  internal typealias Buffer = _NativeSetBuffer<Element>

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits'")
  }

#if _runtime(_ObjC)
  // NSSet bridging:

  // All actual functionality comes from buffer/full, which are
  // just wrappers around a RawNativeSetStorage.

  @inlinable // FIXME(sil-serialize-all)
  internal var buffer: Buffer {
    return Buffer(_storage: self)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var full: FullContainer {
    return FullContainer(_nativeBuffer: buffer)
  }

  @objc
  internal override func enumerator() -> _NSEnumerator {
    return _NativeSetNSEnumerator<Element>(
        Buffer(_storage: self))
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(countByEnumeratingWithState:objects:count:)
  internal override func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(full.startIndex._nativeIndex.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeSetIndex<Element>(
        offset: Int(theState.extra.0))
    let endIndex = buffer.endIndex
    var stored = 0
    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      unmanagedObjects[i] = buffer.bridgedKey(at: currIndex)

      stored += 1
      buffer.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.pointee = theState
    return stored
  }

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal func getObjectFor(_ aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Element.self)
    else { return nil }

    let (i, found) = buffer._find(nativeKey,
        startBucket: buffer._bucket(nativeKey))

    if found {
      return buffer.bridgedValue(at: i)
    }
    return nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc
  override internal func member(_ object: AnyObject) -> AnyObject? {
    return getObjectFor(object)
  }
#endif
}

/// A wrapper around _RawNativeSetStorage that provides most of the
/// implementation of Set.
///
/// This type and most of its functionality doesn't require Hashable at all.
/// The reason for this is to support storing AnyObject for bridging
/// with _SwiftDeferredNSSet. What functionality actually relies on
/// Hashable can be found in an extension.
@usableFromInline
@_fixed_layout
internal struct _NativeSetBuffer<Element> {
  @usableFromInline
  internal typealias TypedStorage = _TypedNativeSetStorage<Element>
  @usableFromInline
  internal typealias Buffer = _NativeSetBuffer<Element>
  @usableFromInline
  internal typealias Index = _NativeSetIndex<Element>

  /// See this comments on _RawNativeSetStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _storage: _RawNativeSetStorage

  /// Creates a Buffer with a storage that is typed, but doesn't understand
  /// Hashing. Mostly for bridging; prefer `init(minimumCapacity:)`.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int, unhashable: ()) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_2(TypedStorage.self,
        bitmapWordCount._builtinWordValue, UInt.self,
        bucketCount._builtinWordValue, Element.self)
    self.init(_exactBucketCount: bucketCount, storage: storage)
  }

  /// Given a bucket count and uninitialized _RawNativeSetStorage, completes the
  /// initialization and returns a Buffer.
  @inlinable // FIXME(sil-serialize-all)
  internal init(
    _exactBucketCount bucketCount: Int,
    storage: _RawNativeSetStorage
  ) {
    storage.bucketCount = bucketCount
    storage.count = 0

    self.init(_storage: storage)

    let initializedEntries = _UnsafeBitMap(
        storage: _initializedHashtableEntriesBitMapBuffer,
        bitCount: bucketCount)
    initializedEntries.initializeToZero()

    // Compute all the array offsets now, so we don't have to later
    let bitmapAddr = Builtin.projectTailElems(_storage, UInt.self)
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let keysAddr = Builtin.getTailAddr_Word(bitmapAddr,
           bitmapWordCount._builtinWordValue, UInt.self, Element.self)

    // Initialize header
    _storage.initializedEntries = initializedEntries
    _storage.keys = UnsafeMutableRawPointer(keysAddr)
    // We assign a unique hash seed to each distinct hash table size, so that we
    // avoid certain copy operations becoming quadratic, without breaking value
    // semantics. (See https://bugs.swift.org/browse/SR-3268)
    //
    // We don't need to generate a brand new seed for each table size: it's
    // enough to change a single bit in the global seed by XORing the bucket
    // count to it. (The bucket count is always a power of two.)
    //
    // FIXME: Use an approximation of true per-instance seeding. We can't just
    // use the base address, because COW copies need to share the same seed.
    let seed = Hasher._seed
    let perturbation = bucketCount
    _storage.seed = (seed.0 ^ UInt64(truncatingIfNeeded: perturbation), seed.1)
  }

  // Forwarding the individual fields of the storage in various forms

  @inlinable // FIXME(sil-serialize-all)
  internal var bucketCount: Int {
    return _assumeNonNegative(_storage.bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    set {
      _storage.count = newValue
    }
    get {
      return _assumeNonNegative(_storage.count)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal
  var _initializedHashtableEntriesBitMapBuffer: UnsafeMutablePointer<UInt> {
    return _storage._initializedHashtableEntriesBitMapBuffer
  }

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable // FIXME(sil-serialize-all)
  internal var keys: UnsafeMutablePointer<Element> {
    return _storage.keys.assumingMemoryBound(to: Element.self)
  }

  /// Constructs a buffer adopting the given storage.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_storage: _RawNativeSetStorage) {
    self._storage = _storage
  }

  /// Constructs an instance from the empty singleton.
  @inlinable // FIXME(sil-serialize-all)
  internal init() {
    self._storage = _RawNativeSetStorage.empty
  }

  // Most of the implementation of the _SetBuffer protocol,
  // but only the parts that don't actually rely on hashing.

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func key(at i: Int) -> Element {
    _sanityCheck(i >= 0 && i < bucketCount)
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    let res = (keys + i).pointee
    return res
  }

#if _runtime(_ObjC)
  /// Returns the key at the given Index, bridged.
  ///
  /// Intended for use with verbatim bridgeable keys.
  @inlinable // FIXME(sil-serialize-all)
  internal func bridgedKey(at index: Index) -> AnyObject {
    let k = key(at: index.offset)
    return _bridgeAnythingToObjectiveC(k)
  }

  /// Returns the value at the given Index, bridged.
  ///
  /// Intended for use with verbatim bridgeable keys.
  @inlinable // FIXME(sil-serialize-all)
  internal func bridgedValue(at index: Index) -> AnyObject {
    let v = value(at: index.offset)
    return _bridgeAnythingToObjectiveC(v)
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  internal func isInitializedEntry(at i: Int) -> Bool {
    _sanityCheck(i >= 0 && i < bucketCount)
    defer { _fixLifetime(self) }

    return _storage.initializedEntries[i]
  }

  @usableFromInline @_transparent
  internal func destroyEntry(at i: Int) {
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).deinitialize(count: 1)
    _storage.initializedEntries[i] = false
  }

  @usableFromInline @_transparent
  internal func initializeKey(_ k: Element, at i: Int) {
    _sanityCheck(!isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).initialize(to: k)
    _storage.initializedEntries[i] = true
  }

  @usableFromInline @_transparent
  internal func moveInitializeEntry(from: Buffer, at: Int, toEntryAt: Int) {
    _sanityCheck(!isInitializedEntry(at: toEntryAt))

    defer { _fixLifetime(self) }

    (keys + toEntryAt).initialize(to: (from.keys + at).move())
    from._storage.initializedEntries[at] = false
    _storage.initializedEntries[toEntryAt] = true
  }

  /// Alias for key(at:) in Sets for better code reuse
  @usableFromInline @_transparent
  internal func value(at i: Int) -> Element {
    return key(at: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func setKey(_ key: Element, at i: Int) {
    _sanityCheck(i >= 0 && i < bucketCount)
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).pointee = key
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    // We start at "index after -1" instead of "0" because we need to find the
    // first occupied slot.
    return index(after: Index(offset: -1))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(offset: bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    _precondition(i != endIndex)
    var idx = i.offset + 1
    while idx < bucketCount && !isInitializedEntry(at: idx) {
      idx += 1
    }

    return Index(offset: idx)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> Element {
    _precondition(i.offset >= 0 && i.offset < bucketCount)
    _precondition(
      isInitializedEntry(at: i.offset),
      "Attempting to access Set elements using an invalid Index")
    let key = self.key(at: i.offset)
    return key
  }
}

extension _NativeSetBuffer/*: _SetBuffer*/ where Element: Hashable {
  @usableFromInline
  internal typealias HashTypedStorage =
    _HashableTypedNativeSetStorage<Element>

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal init(minimumCapacity: Int) {
    let bucketCount = _NativeSetBuffer.bucketCount(
      forCapacity: minimumCapacity,
      maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)
    self.init(bucketCount: bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal init(bucketCount: Int) {
    // Actual bucket count is the next power of 2 greater than or equal to
    // bucketCount. Make sure that is representable.
    _sanityCheck(bucketCount <= (Int.max >> 1) + 1)
    let buckets = 1 &<< ((Swift.max(bucketCount, 2) - 1)._binaryLogarithm() + 1)
    self.init(_exactBucketCount: buckets)
  }

  /// Create a buffer instance with room for at least 'bucketCount' entries,
  /// marking all entries invalid.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_2(HashTypedStorage.self,
        bitmapWordCount._builtinWordValue, UInt.self,
        bucketCount._builtinWordValue, Element.self)
    self.init(_exactBucketCount: bucketCount, storage: storage)
  }

#if _runtime(_ObjC)
  @usableFromInline
  internal func bridged() -> _NSSet {
    // We can zero-cost bridge if our keys are verbatim
    // or if we're the empty singleton.

    // Temporary var for SOME type safety before a cast.
    let nsSet: _NSSetCore

    if _isBridgedVerbatimToObjectiveC(Element.self) ||
      self._storage === _RawNativeSetStorage.empty {
      nsSet = self._storage
    } else {
      nsSet = _SwiftDeferredNSSet(nativeBuffer: self)
    }

    // Cast from "minimal NSSet" to "NSSet"
    // Note that if you actually ask Swift for this cast, it will fail.
    // Never trust a shadow protocol!
    return unsafeBitCast(nsSet, to: _NSSet.self)
  }
#endif

  /// A textual representation of `self`.
  @inlinable // FIXME(sil-serialize-all)
  internal var description: String {
    var result = ""
#if INTERNAL_CHECKS_ENABLED
    for i in 0..<bucketCount {
      if isInitializedEntry(at: i) {
        let key = self.key(at: i)
        result += "bucket \(i), ideal bucket = \(_bucket(key)), key = \(key)\n"
      } else {
        result += "bucket \(i), empty\n"
      }
    }
#endif
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _bucketMask: Int {
    // The bucket count is not negative, therefore subtracting 1 will not
    // overflow.
    return bucketCount &- 1
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always) // For performance reasons.
  internal func _bucket(_ k: Element) -> Int {
    return k._rawHashValue(seed: _storage.seed) & _bucketMask
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _index(after bucket: Int) -> Int {
    // Bucket is within 0 and bucketCount. Therefore adding 1 does not overflow.
    return (bucket &+ 1) & _bucketMask
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _prev(_ bucket: Int) -> Int {
    // Bucket is not negative. Therefore subtracting 1 does not overflow.
    return (bucket &- 1) & _bucketMask
  }

  /// Search for a given key starting from the specified bucket.
  ///
  /// If the key is not present, returns the position where it could be
  /// inserted.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func _find(_ key: Element, startBucket: Int)
    -> (pos: Index, found: Bool) {

    var bucket = startBucket

    // The invariant guarantees there's always a hole, so we just loop
    // until we find one
    while true {
      let isHole = !isInitializedEntry(at: bucket)
      if isHole {
        return (Index(offset: bucket), false)
      }
      if self.key(at: bucket) == key {
        return (Index(offset: bucket), true)
      }
      bucket = _index(after: bucket)
    }
  }

  @usableFromInline @_transparent
  internal static func bucketCount(
    forCapacity capacity: Int,
    maxLoadFactorInverse: Double
  ) -> Int {
    // `capacity + 1` below ensures that we don't fill in the last hole
    return max(Int((Double(capacity) * maxLoadFactorInverse).rounded(.up)),
               capacity + 1)
  }

  /// Buffer should be uniquely referenced.
  /// The `key` should not be present in the Set.
  /// This function does *not* update `count`.

  @inlinable // FIXME(sil-serialize-all)
  internal func unsafeAddNew(key newKey: Element) {
    let (i, found) = _find(newKey, startBucket: _bucket(newKey))
    _precondition(
      !found, "Duplicate element found in Set. Elements may have been mutated after insertion")
    initializeKey(newKey, at: i.offset)
  }


  @inlinable // FIXME(sil-serialize-all)
  internal static func fromArray(_ elements: [Element])
    -> Buffer
  {
    if elements.isEmpty {
      return Buffer()
    }

    var nativeBuffer = Buffer(minimumCapacity: elements.count)

    var count = 0
    for key in elements {
      let (i, found) =
        nativeBuffer._find(key, startBucket: nativeBuffer._bucket(key))
      if found {
        continue
      }
      nativeBuffer.initializeKey(key, at: i.offset)
      count += 1
    }
    nativeBuffer.count = count

    return nativeBuffer
  }
}

extension _NativeSetBuffer/*: _SetBuffer*/ where Element: Hashable {
  //
  // _SetBuffer conformance
  //

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func index(forKey key: Element) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (i, found) = _find(key, startBucket: _bucket(key))
    return found ? i : nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Element) -> Element? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }

    let (i, found) = _find(key, startBucket: _bucket(key))
    if found {
      return self.key(at: i.offset)
    }
    return nil
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator that works with any NativeSetBuffer of
/// verbatim bridgeable elements. Used by the various NSSet impls.
final internal class _NativeSetNSEnumerator<Element>
  : _SwiftNativeNSEnumerator, _NSEnumerator {

  internal typealias Buffer = _NativeSetBuffer<Element>
  internal typealias Index = _NativeSetIndex<Element>

  internal override required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  internal init(_ buffer: Buffer) {
    self.buffer = buffer
    nextIndex = buffer.startIndex
    endIndex = buffer.endIndex
  }

  internal var buffer: Buffer
  internal var nextIndex: Index
  internal var endIndex: Index

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
    let key = buffer.bridgedKey(at: nextIndex)
    buffer.formIndex(after: &nextIndex)
    return key
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
    let key = buffer.bridgedKey(at: nextIndex)
    buffer.formIndex(after: &nextIndex)

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = key
    state.pointee = theState
    return 1
  }
}
#endif

#if _runtime(_ObjC)
/// This class exists for Objective-C bridging. It holds a reference to a
/// NativeSetBuffer, and can be upcast to NSSelf when bridging is necessary.
/// This is the fallback implementation for situations where toll-free bridging
/// isn't possible. On first access, a NativeSetBuffer of AnyObject will be
/// constructed containing all the bridged elements.
final internal class _SwiftDeferredNSSet<Element: Hashable>
  : _SwiftNativeNSSet, _NSSetCore {

  internal init(nativeBuffer: _NativeSetBuffer<Element>) {
    self.nativeBuffer = nativeBuffer
    super.init()
  }

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  internal var _heapStorageBridged_DoNotUse: AnyObject?

  /// The unbridged elements.
  internal var nativeBuffer: _NativeSetBuffer<Element>

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSSet` type, which is immutable.
    return self
  }

  //
  // NSSet implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `nativeBuffer`.
  //

  @objc
  internal required init(objects: UnsafePointer<AnyObject?>, count: Int) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc
  internal func member(_ object: AnyObject) -> AnyObject? {
    guard let element =
      _conditionallyBridgeFromObjectiveC(object, Element.self)
    else { return nil }

    let (i, found) = nativeBuffer._find(
      element, startBucket: nativeBuffer._bucket(element))
    if found {
      bridgeEverything()
      return bridgedBuffer.value(at: i.offset)
    }
    return nil
  }

  @objc
  internal func objectEnumerator() -> _NSEnumerator {
    return enumerator()
  }

  /// Returns the pointer to the stored property, which contains bridged
  /// Set elements.
  @nonobjc
  internal var _heapStorageBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
      to: Optional<AnyObject>.self)
  }

  /// The buffer for bridged Set elements, if present.
  @nonobjc
  internal var _bridgedStorage: _RawNativeSetStorage? {
    get {
      if let ref = _stdlib_atomicLoadARCRef(object: _heapStorageBridgedPtr) {
        return unsafeDowncast(ref, to: _RawNativeSetStorage.self)
      }
      return nil
    }
  }

  /// Attach a buffer for bridged Set elements.
  @nonobjc
  internal func _initializeHeapStorageBridged(_ newStorage: AnyObject) {
    _stdlib_atomicInitializeARCRef(
      object: _heapStorageBridgedPtr, desired: newStorage)
  }

  /// Returns the bridged Set values.
  internal var bridgedBuffer: _NativeSetBuffer<AnyObject> {
    return _NativeSetBuffer(_storage: _bridgedStorage!)
  }

  @nonobjc
  internal func bridgeEverything() {
    if _fastPath(_bridgedStorage != nil) {
      return
    }

    // FIXME: rdar://problem/19486139 (split bridged buffers for keys and values)
    // We bridge keys and values unconditionally here, even if one of them
    // actually is verbatim bridgeable (e.g. Dictionary<Int, AnyObject>).
    // Investigate only allocating the buffer for a Set in this case.

    // Create buffer for bridged data.
    let bridged = _NativeSetBuffer<AnyObject>(
      _exactBucketCount: nativeBuffer.bucketCount,
      unhashable: ())

    // Bridge everything.
    for i in 0..<nativeBuffer.bucketCount {
      if nativeBuffer.isInitializedEntry(at: i) {
        let key = _bridgeAnythingToObjectiveC(nativeBuffer.key(at: i))
        bridged.initializeKey(key, at: i)
      }
    }

    // Atomically put the bridged elements in place.
    _initializeHeapStorageBridged(bridged._storage)
  }

  @objc
  internal var count: Int {
    return nativeBuffer.count
  }

  @objc
  internal func enumerator() -> _NSEnumerator {
    bridgeEverything()
    return _NativeSetNSEnumerator<AnyObject>(bridgedBuffer)
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
      theState.extra.0 = CUnsignedLong(nativeBuffer.startIndex.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeSetIndex<Element>(
        offset: Int(theState.extra.0))
    let endIndex = nativeBuffer.endIndex
    var stored = 0

    // Only need to bridge once, so we can hoist it out of the loop.
    if (currIndex != endIndex) {
      bridgeEverything()
    }

    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      let bridgedKey = bridgedBuffer.key(at: currIndex.offset)
      unmanagedObjects[i] = bridgedKey
      stored += 1
      nativeBuffer.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.pointee = theState
    return stored
  }
}
#else
final internal class _SwiftDeferredNSSet<Element: Hashable> { }
#endif

#if _runtime(_ObjC)
@usableFromInline
@_fixed_layout
internal struct _CocoaSetBuffer: _SetBuffer {
  @usableFromInline
  internal var cocoaSet: _NSSet

  @inlinable // FIXME(sil-serialize-all)
  internal init(cocoaSet: _NSSet) {
    self.cocoaSet = cocoaSet
  }

  @usableFromInline
  internal typealias Index = _CocoaSetIndex

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    return Index(cocoaSet, startIndex: ())
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(cocoaSet, endIndex: ())
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    return i.successor()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: optimize if possible.
    i = i.successor()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(forKey key: AnyObject) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if maybeGet(key) == nil {
      return nil
    }

    let allKeys = _stdlib_NSSet_allObjects(cocoaSet)
    var keyIndex = -1
    for i in 0..<allKeys.value {
      if _stdlib_NSObject_isEqual(key, allKeys[i]) {
        keyIndex = i
        break
      }
    }
    _sanityCheck(keyIndex >= 0,
        "Key was found in fast path, but not found later?")
    return Index(cocoaSet, allKeys, keyIndex)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    return cocoaSet.count
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> AnyObject {
    let value: AnyObject? = i.allKeys[i.currentKeyIndex]
    _sanityCheck(value != nil, "Item not found in underlying NSSet")
    return value!
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: AnyObject) -> AnyObject? {
    return cocoaSet.member(key)
  }

  @usableFromInline
  internal func _toNative<Element: Hashable>(
    bucketCount: Int
  ) -> _NativeSetBuffer<Element> {
    let cocoaSet = self.cocoaSet
    var newNativeBuffer = _NativeSetBuffer<Element>(bucketCount: bucketCount)
    let oldCocoaIterator = _CocoaSetIterator(cocoaSet)
    while let element = oldCocoaIterator.next() {
      newNativeBuffer.unsafeAddNew(
        key: _forceBridgeFromObjectiveC(element, Element.self))
    }
    newNativeBuffer.count = cocoaSet.count
    return newNativeBuffer
  }
}
#endif

@usableFromInline
@_frozen
internal enum _VariantSetBuffer<Element: Hashable>: _SetBuffer {

  @usableFromInline
  internal typealias NativeBuffer = _NativeSetBuffer<Element>
  @usableFromInline
  internal typealias NativeIndex = _NativeSetIndex<Element>
#if _runtime(_ObjC)
  @usableFromInline
  internal typealias CocoaBuffer = _CocoaSetBuffer
#endif
  @usableFromInline
  internal typealias SelfType = _VariantSetBuffer

  case native(NativeBuffer)
#if _runtime(_ObjC)
  case cocoa(CocoaBuffer)
#endif

  @usableFromInline @_transparent
  internal var guaranteedNative: Bool {
    return _canBeClass(Element.self) == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func isUniquelyReferenced() -> Bool {
    // Note that &self drills down through .native(NativeBuffer) to the first
    // property in NativeBuffer, which is the reference to the storage.
    if _fastPath(guaranteedNative) {
      return _isUnique_native(&self)
    }

    switch self {
    case .native:
      return _isUnique_native(&self)
#if _runtime(_ObjC)
    case .cocoa:
      // Don't consider Cocoa buffer mutable, even if it is mutable and is
      // uniquely referenced.
      return false
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var asNative: NativeBuffer {
    get {
      switch self {
      case .native(let buffer):
        return buffer
#if _runtime(_ObjC)
      case .cocoa:
        _sanityCheckFailure("internal error: not backed by native buffer")
#endif
      }
    }
    set {
      self = .native(newValue)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureNativeBuffer() {
#if _runtime(_ObjC)
    if _fastPath(guaranteedNative) { return }
    if case .cocoa(let cocoaBuffer) = self {
      migrateDataToNativeBuffer(cocoaBuffer)
    }
#endif
  }

#if _runtime(_ObjC)
  @inlinable // FIXME(sil-serialize-all)
  internal var asCocoa: CocoaBuffer {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSSet")
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer
    }
  }
#endif

  /// Return true if self is native.
  @inlinable // FIXME(sil-serialize-all)
  internal var _isNative: Bool {
#if _runtime(_ObjC)
    switch self {
    case .native:
      return true
    case .cocoa:
      return false
    }
#else
    return true
#endif
  }

  @inline(__always)
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBufferNative(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let oldBucketCount = asNative.bucketCount
    if oldBucketCount >= desiredBucketCount && isUniquelyReferenced() {
      return (reallocated: false, capacityChanged: false)
    }

    let oldNativeBuffer = asNative
    var newNativeBuffer = NativeBuffer(bucketCount: desiredBucketCount)
    let newBucketCount = newNativeBuffer.bucketCount
    for i in 0..<oldBucketCount {
      if oldNativeBuffer.isInitializedEntry(at: i) {
        if oldBucketCount == newBucketCount {
          let key = oldNativeBuffer.key(at: i)
          newNativeBuffer.initializeKey(key, at: i)
        } else {
          let key = oldNativeBuffer.key(at: i)
          newNativeBuffer.unsafeAddNew(key: key)
        }
      }
    }
    newNativeBuffer.count = oldNativeBuffer.count

    self = .native(newNativeBuffer)
    return (reallocated: true,
      capacityChanged: oldBucketCount != newBucketCount)
  }

  @inline(__always)
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBuffer(
    withCapacity minimumCapacity: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let bucketCount = NativeBuffer.bucketCount(
      forCapacity: minimumCapacity,
      maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)
    return ensureUniqueNativeBuffer(withBucketCount: bucketCount)
  }

  /// Ensure this we hold a unique reference to a native buffer
  /// having at least `minimumCapacity` elements.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBuffer(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
#if _runtime(_ObjC)
    // This is a performance optimization that was put in to ensure that we did
    // not make a copy of self to call _isNative over the entire if region
    // causing at -Onone the uniqueness check to fail. This code used to be:
    //
    //  if _isNative {
    //    return ensureUniqueNativeBufferNative(
    //      withBucketCount: desiredBucketCount)
    //  }
    //
    // SR-6437
    let n = _isNative
    if n {
      return ensureUniqueNativeBufferNative(withBucketCount: desiredBucketCount)
    }

    switch self {
    case .native:
      fatalError("This should have been handled earlier")
    case .cocoa(let cocoaBuffer):
      self = .native(cocoaBuffer._toNative(bucketCount: desiredBucketCount))
      return (reallocated: true, capacityChanged: true)
    }
#else
    return ensureUniqueNativeBufferNative(withBucketCount: desiredBucketCount)
#endif
  }

#if _runtime(_ObjC)
  @usableFromInline
  internal mutating func migrateDataToNativeBuffer(
    _ cocoaBuffer: _CocoaSetBuffer
  ) {
    let allocated = ensureUniqueNativeBuffer(
      withCapacity: cocoaBuffer.count).reallocated
    _sanityCheck(allocated, "failed to allocate native Set buffer")
  }
#endif

  /// Reserves enough space for the specified number of elements to be stored
  /// without reallocating additional storage.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func reserveCapacity(_ capacity: Int) {
    _ = ensureUniqueNativeBuffer(withCapacity: capacity)
  }

  /// The number of elements that can be stored without expanding the current
  /// storage.
  ///
  /// For bridged storage, this is equal to the current count of the
  /// collection, since any addition will trigger a copy of the elements into
  /// newly allocated storage. For native storage, this is the element count
  /// at which adding any more elements will exceed the load factor.
  @inlinable // FIXME(sil-serialize-all)
  internal var capacity: Int {
    switch self {
    case .native:
      return Int(Double(asNative.bucketCount) /
        _hashContainerDefaultMaxLoadFactorInverse)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer.count
#endif
    }
  }

  //
  // _SetBuffer conformance
  //

  @usableFromInline
  internal typealias Index = SetIndex<Element>

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.startIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.startIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.startIndex)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.endIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.endIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.endIndex)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.index(after: i._nativeIndex))
    }

    switch self {
    case .native:
      return ._native(asNative.index(after: i._nativeIndex))
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.index(after: i._cocoaIndex))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: optimize if possible.
    i = index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func index(forKey key: Element) -> Index? {
    if _fastPath(guaranteedNative) {
      if let nativeIndex = asNative.index(forKey: key) {
        return ._native(nativeIndex)
      }
      return nil
    }

    switch self {
    case .native:
      if let nativeIndex = asNative.index(forKey: key) {
        return ._native(nativeIndex)
      }
      return nil
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let anyObjectKey: AnyObject = _bridgeAnythingToObjectiveC(key)
      if let cocoaIndex = cocoaBuffer.index(forKey: anyObjectKey) {
        return ._cocoa(cocoaIndex)
      }
      return nil
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> Element {
    if _fastPath(guaranteedNative) {
      return asNative.assertingGet(at: i._nativeIndex)
    }

    switch self {
    case .native:
      return asNative.assertingGet(at: i._nativeIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let anyObjectValue = cocoaBuffer.assertingGet(at: i._cocoaIndex)
      let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Element.self)
      return nativeValue
#endif
    }
  }

#if _runtime(_ObjC)
  @inline(never)
  @usableFromInline
  internal static func maybeGetFromCocoaBuffer(
    _ cocoaBuffer: CocoaBuffer, forKey key: Element
  ) -> Element? {
    let anyObjectKey: AnyObject = _bridgeAnythingToObjectiveC(key)
    if let anyObjectValue = cocoaBuffer.maybeGet(anyObjectKey) {
      return _forceBridgeFromObjectiveC(anyObjectValue, Element.self)
    }
    return nil
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Element) -> Element? {
    if _fastPath(guaranteedNative) {
      return asNative.maybeGet(key)
    }

    switch self {
    case .native:
      return asNative.maybeGet(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return SelfType.maybeGetFromCocoaBuffer(cocoaBuffer, forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeUpdate(
    with key: Element
  ) -> Element? {
    var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))

    let minBuckets = found
      ? asNative.bucketCount
      : NativeBuffer.bucketCount(
          forCapacity: asNative.count + 1,
          maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)

    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withBucketCount: minBuckets)
    if capacityChanged {
      i = asNative._find(key, startBucket: asNative._bucket(key)).pos
    }

    let old: Element? = found ? asNative.key(at: i.offset) : nil
    if found {
      asNative.setKey(key, at: i.offset)
    } else {
      asNative.initializeKey(key, at: i.offset)
      asNative.count += 1
    }

    return old
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func update(with value: Element) -> Element? {
    if _fastPath(guaranteedNative) {
      return nativeUpdate(with: value)
    }

    switch self {
    case .native:
      return nativeUpdate(with: value)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      migrateDataToNativeBuffer(cocoaBuffer)
      return nativeUpdate(with: value)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func insert(
    _ key: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    ensureNativeBuffer()

    var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))
    if found {
      return (inserted: false, memberAfterInsert: asNative.key(at: i.offset))
    }

    let minCapacity = asNative.count + 1
    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withCapacity: minCapacity)

    if capacityChanged {
      i = asNative._find(key, startBucket: asNative._bucket(key)).pos
    }

    asNative.initializeKey(key, at: i.offset)
    asNative.count += 1

    return (inserted: true, memberAfterInsert: key)
  }

  /// - parameter idealBucket: The ideal bucket for the element being deleted.
  /// - parameter offset: The offset of the element that will be deleted.
  /// Precondition: there should be an initialized entry at offset.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeDelete(
    _ nativeBuffer: NativeBuffer, idealBucket: Int, offset: Int
  ) {
    _sanityCheck(
        nativeBuffer.isInitializedEntry(at: offset), "expected initialized entry")

    var nativeBuffer = nativeBuffer

    // remove the element
    nativeBuffer.destroyEntry(at: offset)
    nativeBuffer.count -= 1

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = offset

    // Find the first bucket in the contiguous chain
    var start = idealBucket
    while nativeBuffer.isInitializedEntry(at: nativeBuffer._prev(start)) {
      start = nativeBuffer._prev(start)
    }

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    var b = nativeBuffer._index(after: lastInChain)
    while nativeBuffer.isInitializedEntry(at: b) {
      lastInChain = b
      b = nativeBuffer._index(after: b)
    }

    // Relocate out-of-place elements in the chain, repeating until
    // none are found.
    while hole != lastInChain {
      // Walk backwards from the end of the chain looking for
      // something out-of-place.
      var b = lastInChain
      while b != hole {
        let idealBucket = nativeBuffer._bucket(nativeBuffer.key(at: b))

        // Does this element belong between start and hole?  We need
        // two separate tests depending on whether [start, hole] wraps
        // around the end of the storage
        let c0 = idealBucket >= start
        let c1 = idealBucket <= hole
        if start <= hole ? (c0 && c1) : (c0 || c1) {
          break // Found it
        }
        b = nativeBuffer._prev(b)
      }

      if b == hole { // No out-of-place elements found; we're done adjusting
        break
      }

      // Move the found element into the hole
      nativeBuffer.moveInitializeEntry(
        from: nativeBuffer,
        at: b,
        toEntryAt: hole)
      hole = b
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemove(_ key: Element) -> Element? {
    var idealBucket = asNative._bucket(key)
    var (index, found) = asNative._find(key, startBucket: idealBucket)

    // Fast path: if the key is not present, we will not mutate the set,
    // so don't force unique buffer.
    if !found {
      return nil
    }

    // This is a performance optimization that was put in to ensure that we
    // did not make a copy of self to call asNative.bucketCount over
    // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
    // fail. This code used to be:
    //
    // ... = ensureUniqueNativeBuffer(withBucketCount: asNative.bucketCount)
    //
    // SR-6437
    let bucketCount = asNative.bucketCount
    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withBucketCount: bucketCount)
    let nativeBuffer = asNative
    if capacityChanged {
      idealBucket = nativeBuffer._bucket(key)
      (index, found) = nativeBuffer._find(key, startBucket: idealBucket)
      _sanityCheck(found, "key was lost during buffer migration")
    }
    let oldValue = nativeBuffer.key(at: index.offset)
    nativeDelete(nativeBuffer, idealBucket: idealBucket,
      offset: index.offset)
    return oldValue
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemove(at nativeIndex: NativeIndex) -> Element {
    // This is a performance optimization that was put in to ensure that we did
    // not make a copy of self to call asNative.bucketCount over
    // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
    // fail. This code used to be:
    //
    // _ = ensureUniqueNativeBuffer(withBucketCount: asNative.bucketCount)
    //
    // SR-6437
    let bucketCount = asNative.bucketCount
    // The provided index should be valid, so we will always mutating the
    // set buffer.  Request unique buffer.
    _ = ensureUniqueNativeBuffer(withBucketCount: bucketCount)
    let nativeBuffer = asNative

    let result = nativeBuffer.assertingGet(at: nativeIndex)
    let key = result

    nativeDelete(nativeBuffer, idealBucket: nativeBuffer._bucket(key),
        offset: nativeIndex.offset)
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func remove(at index: Index) -> Element {
    if _fastPath(guaranteedNative) {
      return nativeRemove(at: index._nativeIndex)
    }

    switch self {
    case .native:
      return nativeRemove(at: index._nativeIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      //
      // FIXME(performance): fuse data migration and element deletion into one
      // operation.
      let index = index._cocoaIndex
      let anyObjectKey: AnyObject = index.allKeys[index.currentKeyIndex]
      migrateDataToNativeBuffer(cocoaBuffer)
      let key = _forceBridgeFromObjectiveC(anyObjectKey, Element.self)
      let old = nativeRemove(key)
      _sanityCheck(key == old, "bridging did not preserve equality")
      return key
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func remove(_ member: Element) -> Element? {
    if _fastPath(guaranteedNative) {
      return nativeRemove(member)
    }

    switch self {
    case .native:
      return nativeRemove(member)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let cocoaMember = _bridgeAnythingToObjectiveC(member)
      if cocoaBuffer.maybeGet(cocoaMember) == nil {
        return nil
      }
      migrateDataToNativeBuffer(cocoaBuffer)
      return nativeRemove(member)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemoveAll() {
    if !isUniquelyReferenced() {
        asNative = NativeBuffer(_exactBucketCount: asNative.bucketCount)
        return
    }

    // We have already checked for the empty dictionary case and unique
    // reference, so we will always mutate the dictionary buffer.
    var nativeBuffer = asNative

    for b in 0..<nativeBuffer.bucketCount {
      if nativeBuffer.isInitializedEntry(at: b) {
        nativeBuffer.destroyEntry(at: b)
      }
    }
    nativeBuffer.count = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if count == 0 {
      return
    }

    if !keepCapacity {
      self = .native(NativeBuffer(bucketCount: 2))
      return
    }

    if _fastPath(guaranteedNative) {
      nativeRemoveAll()
      return
    }

    switch self {
    case .native:
      nativeRemoveAll()
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      self = .native(NativeBuffer(minimumCapacity: cocoaBuffer.count))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    if _fastPath(guaranteedNative) {
      return asNative.count
    }

    switch self {
    case .native:
      return asNative.count
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer.count
#endif
    }
  }

  /// Returns an iterator over the elements.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func makeIterator() -> SetIterator<Element> {
    switch self {
    case .native(let buffer):
      return ._native(
        start: asNative.startIndex, end: asNative.endIndex, buffer: buffer)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer)
#endif
    }
  }
}

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal struct _NativeSetIndex<Element>: Comparable {
  @usableFromInline
  internal var offset: Int

  @inlinable // FIXME(sil-serialize-all)
  internal init(offset: Int) {
    self.offset = offset
  }
}

extension _NativeSetIndex {

  @inlinable // FIXME(sil-serialize-all)
  internal static func < (
    lhs: _NativeSetIndex<Element>,
    rhs: _NativeSetIndex<Element>
  ) -> Bool {
    return lhs.offset < rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func <= (
    lhs: _NativeSetIndex<Element>,
    rhs: _NativeSetIndex<Element>
  ) -> Bool {
    return lhs.offset <= rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func > (
    lhs: _NativeSetIndex<Element>,
    rhs: _NativeSetIndex<Element>
  ) -> Bool {
    return lhs.offset > rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func >= (
    lhs: _NativeSetIndex<Element>,
    rhs: _NativeSetIndex<Element>
  ) -> Bool {
    return lhs.offset >= rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func == (
    lhs: _NativeSetIndex<Element>,
    rhs: _NativeSetIndex<Element>
  ) -> Bool {
    return lhs.offset == rhs.offset
  }
}

#if _runtime(_ObjC)
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal struct _CocoaSetIndex: Comparable {
  // Assumption: we rely on NSDictionary.getObjects when being
  // repeatedly called on the same NSDictionary, returning items in the same
  // order every time.
  // Similarly, the same assumption holds for NSSet.allObjects.

  /// A reference to the NSSet, which owns members in `allObjects`,
  /// or `allKeys`, for NSSet and NSDictionary respectively.
  @usableFromInline // FIXME(sil-serialize-all)
  internal let cocoaSet: _NSSet
  // FIXME: swift-3-indexing-model: try to remove the cocoa reference, but make
  // sure that we have a safety check for accessing `allKeys`.  Maybe move both
  // into the dictionary/set itself.

  /// An unowned array of keys.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var allKeys: _HeapBuffer<Int, AnyObject>

  /// Index into `allKeys`
  @usableFromInline // FIXME(sil-serialize-all)
  internal var currentKeyIndex: Int

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaSet: _NSSet, startIndex: ()) {
    self.cocoaSet = cocoaSet
    self.allKeys = _stdlib_NSSet_allObjects(cocoaSet)
    self.currentKeyIndex = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaSet: _NSSet, endIndex: ()) {
    self.cocoaSet = cocoaSet
    self.allKeys = _stdlib_NSSet_allObjects(cocoaSet)
    self.currentKeyIndex = allKeys.value
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaSet: _NSSet,
    _ allKeys: _HeapBuffer<Int, AnyObject>,
    _ currentKeyIndex: Int
  ) {
    self.cocoaSet = cocoaSet
    self.allKeys = allKeys
    self.currentKeyIndex = currentKeyIndex
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// - Precondition: The next value is representable.
  @inlinable // FIXME(sil-serialize-all)
  internal func successor() -> _CocoaSetIndex {
    // FIXME: swift-3-indexing-model: remove this method.
    _precondition(
      currentKeyIndex < allKeys.value, "Cannot increment endIndex")
    return _CocoaSetIndex(cocoaSet, allKeys, currentKeyIndex + 1)
  }
}

extension _CocoaSetIndex {

  @inlinable // FIXME(sil-serialize-all)
  internal static func < (
    lhs: _CocoaSetIndex,
    rhs: _CocoaSetIndex
  ) -> Bool {
    return lhs.currentKeyIndex < rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func <= (
    lhs: _CocoaSetIndex,
    rhs: _CocoaSetIndex
  ) -> Bool {
    return lhs.currentKeyIndex <= rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func > (
    lhs: _CocoaSetIndex,
    rhs: _CocoaSetIndex
  ) -> Bool {
    return lhs.currentKeyIndex > rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func >= (
    lhs: _CocoaSetIndex,
    rhs: _CocoaSetIndex
  ) -> Bool {
    return lhs.currentKeyIndex >= rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func == (
    lhs: _CocoaSetIndex,
    rhs: _CocoaSetIndex
  ) -> Bool {
    return lhs.currentKeyIndex == rhs.currentKeyIndex
  }
}
#endif

@_frozen // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal enum SetIndexRepresentation<Element: Hashable> {
  @usableFromInline
  typealias _Index = SetIndex<Element>
  @usableFromInline
  typealias _NativeIndex = _Index._NativeIndex
#if _runtime(_ObjC)
  @usableFromInline
  typealias _CocoaIndex = _Index._CocoaIndex
#endif

  case _native(_NativeIndex)
#if _runtime(_ObjC)
  case _cocoa(_CocoaIndex)
#endif
}

extension Set {
  /// The position of an element in a set.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index: Comparable, Hashable {
    // Index for native buffer is efficient.  Index for bridged NSSet is
    // not, because neither NSEnumerator nor fast enumeration support moving
    // backwards.  Even if they did, there is another issue: NSEnumerator does
    // not support NSCopying, and fast enumeration does not document that it is
    // safe to copy the state.  So, we cannot implement Index that is a value
    // type for bridged NSSet in terms of Cocoa enumeration facilities.

    @usableFromInline
    internal typealias _NativeIndex = _NativeSetIndex<Element>
#if _runtime(_ObjC)
    @usableFromInline
    internal typealias _CocoaIndex = _CocoaSetIndex
#endif

    @inlinable // FIXME(sil-serialize-all)
    internal init(_value: SetIndexRepresentation<Element>) {
      self._value = _value
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _value: SetIndexRepresentation<Element>

    @inlinable // FIXME(sil-serialize-all)
    internal static func _native(_ index: _NativeIndex) -> Index {
      return SetIndex(_value: ._native(index))
    }
#if _runtime(_ObjC)
    @inlinable // FIXME(sil-serialize-all)
    internal static func _cocoa(_ index: _CocoaIndex) -> Index {
      return SetIndex(_value: ._cocoa(index))
    }
#endif

    @usableFromInline @_transparent
    internal var _guaranteedNative: Bool {
      return _canBeClass(Element.self) == 0
    }

    @usableFromInline @_transparent
    internal var _nativeIndex: _NativeIndex {
      switch _value {
      case ._native(let nativeIndex):
        return nativeIndex
#if _runtime(_ObjC)
      case ._cocoa:
        _sanityCheckFailure("internal error: does not contain a native index")
#endif
      }
    }

#if _runtime(_ObjC)
    @usableFromInline @_transparent
    internal var _cocoaIndex: _CocoaIndex {
      switch _value {
      case ._native:
        _sanityCheckFailure("internal error: does not contain a Cocoa index")
      case ._cocoa(let cocoaIndex):
        return cocoaIndex
      }
    }
#endif
  }
}

public typealias SetIndex<Element: Hashable> = Set<Element>.Index

extension Set.Index {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (
    lhs: Set<Element>.Index,
    rhs: Set<Element>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._nativeIndex == rhs._nativeIndex
    }

    switch (lhs._value, rhs._value) {
    case (._native(let lhsNative), ._native(let rhsNative)):
      return lhsNative == rhsNative
  #if _runtime(_ObjC)
    case (._cocoa(let lhsCocoa), ._cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func < (
    lhs: Set<Element>.Index,
    rhs: Set<Element>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._nativeIndex < rhs._nativeIndex
    }

    switch (lhs._value, rhs._value) {
    case (._native(let lhsNative), ._native(let rhsNative)):
      return lhsNative < rhsNative
  #if _runtime(_ObjC)
    case (._cocoa(let lhsCocoa), ._cocoa(let rhsCocoa)):
      return lhsCocoa < rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }

  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
  #if _runtime(_ObjC)
    if _fastPath(_guaranteedNative) {
      hasher.combine(0 as UInt8)
      hasher.combine(_nativeIndex.offset)
      return
    }
    switch _value {
    case ._native(let nativeIndex):
      hasher.combine(0 as UInt8)
      hasher.combine(nativeIndex.offset)
    case ._cocoa(let cocoaIndex):
      hasher.combine(1 as UInt8)
      hasher.combine(cocoaIndex.currentKeyIndex)
    }
  #else
    hasher.combine(_nativeIndex.offset)
  #endif
  }
}

#if _runtime(_ObjC)
@usableFromInline
final internal class _CocoaSetIterator: IteratorProtocol {
  internal typealias Element = AnyObject

  // Cocoa Set iterator has to be a class, otherwise we cannot
  // guarantee that the fast enumeration struct is pinned to a certain memory
  // location.

  // This stored property should be stored at offset zero.  There's code below
  // relying on this.
  internal var _fastEnumerationState: _SwiftNSFastEnumerationState =
    _makeSwiftNSFastEnumerationState()

  // This stored property should be stored right after `_fastEnumerationState`.
  // There's code below relying on this.
  internal var _fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

  internal let cocoaSet: _NSSet

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

  internal init(_ cocoaSet: _NSSet) {
    self.cocoaSet = cocoaSet
  }

  @usableFromInline
  internal func next() -> Element? {
    if itemIndex < 0 {
      return nil
    }
    let cocoaSet = self.cocoaSet
    if itemIndex == itemCount {
      let stackBufCount = _fastEnumerationStackBuf.count
      // We can't use `withUnsafeMutablePointer` here to get pointers to
      // properties, because doing so might introduce a writeback storage, but
      // fast enumeration relies on the pointer identity of the enumeration
      // state struct.
      itemCount = cocoaSet.countByEnumerating(
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

@usableFromInline
@_frozen // FIXME(sil-serialize-all)
internal enum SetIteratorRepresentation<Element: Hashable> {
  @usableFromInline
  internal typealias _Iterator = SetIterator<Element>
  @usableFromInline
  internal typealias _NativeBuffer = _NativeSetBuffer<Element>
  @usableFromInline
  internal typealias _NativeIndex = _Iterator._NativeIndex

  // For native buffer, we keep two indices to keep track of the iteration
  // progress and the buffer owner to make the buffer non-uniquely
  // referenced.
  //
  // Iterator is iterating over a frozen view of the collection
  // state, so it should keep its own reference to the buffer.
  case _native(
    start: _NativeIndex, end: _NativeIndex, buffer: _NativeBuffer)
#if _runtime(_ObjC)
  case _cocoa(_CocoaSetIterator)
#endif
}

/// An iterator over the members of a `Set<Element>`.
@_fixed_layout // FIXME(sil-serialize-all)
public struct SetIterator<Element: Hashable>: IteratorProtocol {
  // Set has a separate IteratorProtocol and Index because of efficiency
  // and implementability reasons.
  //
  // Index for native buffer is efficient.  Index for bridged NSSet is
  // not.
  //
  // Even though fast enumeration is not suitable for implementing
  // Index, which is multi-pass, it is suitable for implementing a
  // IteratorProtocol, which is being consumed as iteration proceeds.

  @usableFromInline
  internal typealias _NativeBuffer = _NativeSetBuffer<Element>
  @usableFromInline
  internal typealias _NativeIndex = _NativeSetIndex<Element>

  @usableFromInline
  internal var _state: SetIteratorRepresentation<Element>

  @inlinable // FIXME(sil-serialize-all)
  internal init(_state: SetIteratorRepresentation<Element>) {
    self._state = _state
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func _native(
    start: _NativeIndex, end: _NativeIndex, buffer: _NativeBuffer
  ) -> SetIterator {
    return SetIterator(
      _state: ._native(start: start, end: end, buffer: buffer))
  }
#if _runtime(_ObjC)
  @usableFromInline
  internal static func _cocoa(_ buffer: _CocoaSetBuffer) -> SetIterator {
    let iterator = _CocoaSetIterator(buffer.cocoaSet)
    return SetIterator(_state: ._cocoa(iterator))
  }
#endif

  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Element.self) == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func _nativeNext() -> Element? {
    switch _state {
    case ._native(let startIndex, let endIndex, let buffer):
      if startIndex == endIndex {
        return nil
      }
      let result = buffer.assertingGet(at: startIndex)
      _state =
        ._native(start: buffer.index(after: startIndex), end: endIndex, buffer: buffer)
      return result
#if _runtime(_ObjC)
    case ._cocoa:
      _sanityCheckFailure("internal error: not backed by NSSet")
#endif
    }
  }

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public mutating func next() -> Element? {
    if _fastPath(_guaranteedNative) {
      return _nativeNext()
    }

    switch _state {
    case ._native:
      return _nativeNext()
#if _runtime(_ObjC)
    case ._cocoa(let cocoaIterator):
      if let anyObjectElement = cocoaIterator.next() {
        return _forceBridgeFromObjectiveC(anyObjectElement, Element.self)
      }
      return nil
#endif
    }
  }
}

extension SetIterator: CustomReflectable {
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
@_fixed_layout // FIXME(sil-serialize-all)
public struct _SetBuilder<Element: Hashable> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _result: Set<Element>
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _nativeBuffer: _NativeSetBuffer<Element>
  @usableFromInline // FIXME(sil-serialize-all)
  internal let _requestedCount: Int
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _actualCount: Int

  @inlinable // FIXME(sil-serialize-all)
  public init(count: Int) {
    _result = Set<Element>(minimumCapacity: count)
    _nativeBuffer = _result._variantBuffer.asNative
    _requestedCount = count
    _actualCount = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  public mutating func add(member: Element) {
    _nativeBuffer.unsafeAddNew(key: member)
    _actualCount += 1
  }

  @inlinable // FIXME(sil-serialize-all)
  public mutating func take() -> Set<Element> {
    _precondition(_actualCount >= 0,
      "Cannot take the result twice")
    _precondition(_actualCount == _requestedCount,
      "The number of members added does not match the promised count")

    // Finish building the `Set`.
    _nativeBuffer.count = _requestedCount

    // Prevent taking the result twice.
    _actualCount = -1
    return _result
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
  @inlinable // FIXME(sil-serialize-all)
  public var capacity: Int {
    return _variantBuffer.capacity
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
    _variantBuffer.reserveCapacity(minimumCapacity)
    _sanityCheck(self.capacity >= minimumCapacity)
  }
}

//===--- Bridging ---------------------------------------------------------===//

#if _runtime(_ObjC)
extension Set {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveCImpl() -> _NSSetCore {
    switch _variantBuffer {
    case _VariantSetBuffer.native(let buffer):
      return buffer.bridged()
    case _VariantSetBuffer.cocoa(let cocoaBuffer):
      return cocoaBuffer.cocoaSet
    }
  }

  /// Returns the native Dictionary hidden inside this NSDictionary;
  /// returns nil otherwise.
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ s: AnyObject
  ) -> Set<Element>? {

    // Try all three NSSet impls that we currently provide.

    if let deferredBuffer = s as? _SwiftDeferredNSSet<Element> {
      return Set(_nativeBuffer: deferredBuffer.nativeBuffer)
    }

    if let nativeStorage = s as? _HashableTypedNativeSetStorage<Element> {
      return Set(_nativeBuffer:
          _NativeSetBuffer(_storage: nativeStorage))
    }

    if s === _RawNativeSetStorage.empty {
      return Set()
    }

    // FIXME: what if `s` is native storage, but for different key/value type?
    return nil
  }
}
#endif
