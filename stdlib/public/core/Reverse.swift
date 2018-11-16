//===--- Reverse.swift - Sequence and collection reversal -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension MutableCollection where Self: BidirectionalCollection {
  /// Reverses the elements of the collection in place.
  ///
  /// The following example reverses the elements of an array of characters:
  ///
  ///     var characters: [Character] = ["C", "a", "f", "é"]
  ///     characters.reverse()
  ///     print(characters)
  ///     // Prints "["é", "f", "a", "C"]
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the
  ///   collection.
  @inlinable // protocol-only
  public mutating func reverse() {
    if isEmpty { return }
    var f = startIndex
    var l = index(before: endIndex)
    while f < l {
      swapAt(f, l)
      formIndex(after: &f)
      formIndex(before: &l)
    }
  }
}

/// A collection that presents the elements of its base collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reversed()` where `x` is a
///   collection having bidirectional indices.
///
/// The `reversed()` method is always lazy when applied to a collection
/// with bidirectional indices, but does not implicitly confer
/// laziness on algorithms applied to its result.  In other words, for
/// ordinary collections `c` having bidirectional indices:
///
/// * `c.reversed()` does not create new storage
/// * `c.reversed().map(f)` maps eagerly and returns a new array
/// * `c.lazy.reversed().map(f)` maps lazily and returns a `LazyMapCollection`
@_fixed_layout
public struct ReversedCollection<Base: BidirectionalCollection> {
  public let _base: Base

  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  @inlinable
  internal init(_base: Base) {
    self._base = _base
  }
}

extension ReversedCollection {
  // An iterator that can be much faster than the iterator of a reversed slice.
  @_fixed_layout
  public struct Iterator {
    @usableFromInline
    internal let _base: Base
    @usableFromInline
    internal var _position: Base.Index

    @inlinable
    @inline(__always)
    /// Creates an iterator over the given collection.
    public /// @testable
    init(_base: Base) {
      self._base = _base
      self._position = _base.endIndex
    }
  }
}

extension ReversedCollection.Iterator: IteratorProtocol, Sequence {
  public typealias Element = Base.Element
  
  @inlinable
  @inline(__always)
  public mutating func next() -> Element? {
    guard _fastPath(_position != _base.startIndex) else { return nil }
    _base.formIndex(before: &_position)
    return _base[_position]
  }
}

extension ReversedCollection: Sequence {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Element = Base.Element

  @inlinable
  @inline(__always)
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: _base)
  }
}

extension ReversedCollection {
  /// An index that traverses the same positions as an underlying index,
  /// with inverted traversal direction.
  @_fixed_layout
  public struct Index {
    /// The position after this position in the underlying collection.
    ///
    /// To find the position that corresponds with this index in the original,
    /// underlying collection, use that collection's `index(before:)` method
    /// with the `base` property.
    ///
    /// The following example declares a function that returns the index of the
    /// last even number in the passed array, if one is found. First, the
    /// function finds the position of the last even number as a `ReversedIndex`
    /// in a reversed view of the array of numbers. Next, the function calls the
    /// array's `index(before:)` method to return the correct position in the
    /// passed array.
    ///
    ///     func indexOfLastEven(_ numbers: [Int]) -> Int? {
    ///         let reversedNumbers = numbers.reversed()
    ///         guard let i = reversedNumbers.firstIndex(where: { $0 % 2 == 0 })
    ///             else { return nil }
    ///
    ///         return numbers.index(before: i.base)
    ///     }
    ///
    ///     let numbers = [10, 20, 13, 19, 30, 52, 17, 40, 51]
    ///     if let lastEven = indexOfLastEven(numbers) {
    ///         print("Last even number: \(numbers[lastEven])")
    ///     }
    ///     // Prints "Last even number: 40"
    public let base: Base.Index

    /// Creates a new index into a reversed collection for the position before
    /// the specified index.
    ///
    /// When you create an index into a reversed collection using `base`, an
    /// index from the underlying collection, the resulting index is the
    /// position of the element *before* the element referenced by `base`. The
    /// following example creates a new `ReversedIndex` from the index of the
    /// `"a"` character in a string's character view.
    ///
    ///     let name = "Horatio"
    ///     let aIndex = name.firstIndex(of: "a")!
    ///     // name[aIndex] == "a"
    ///
    ///     let reversedName = name.reversed()
    ///     let i = ReversedIndex<String>(aIndex)
    ///     // reversedName[i] == "r"
    ///
    /// The element at the position created using `ReversedIndex<...>(aIndex)` is
    /// `"r"`, the character before `"a"` in the `name` string.
    ///
    /// - Parameter base: The position after the element to create an index for.
    @inlinable
    public init(_ base: Base.Index) {
      self.base = base
    }
  }
}

extension ReversedCollection.Index: Comparable {
  @inlinable
  public static func == (
    lhs: ReversedCollection<Base>.Index,
    rhs: ReversedCollection<Base>.Index
  ) -> Bool {
    // Note ReversedIndex has inverted logic compared to base Base.Index
    return lhs.base == rhs.base
  }

  @inlinable
  public static func < (
    lhs: ReversedCollection<Base>.Index,
    rhs: ReversedCollection<Base>.Index
  ) -> Bool {
    // Note ReversedIndex has inverted logic compared to base Base.Index
    return lhs.base > rhs.base
  }
}

extension ReversedCollection.Index: Hashable where Base.Index: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(base)
  }
}

extension ReversedCollection: BidirectionalCollection {  
  @inlinable
  public var startIndex: Index {
    return Index(_base.endIndex)
  }

  @inlinable
  public var endIndex: Index {
    return Index(_base.startIndex)
  }

  @inlinable
  public func index(after i: Index) -> Index {
    return Index(_base.index(before: i.base))
  }

  @inlinable
  public func index(before i: Index) -> Index {
    return Index(_base.index(after: i.base))
  }

  @inlinable
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    return Index(_base.index(i.base, offsetBy: -n))
  }

  @inlinable
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    return _base.index(i.base, offsetBy: -n, limitedBy: limit.base)
                .map(Index.init)
  }

  @inlinable
  public func distance(from start: Index, to end: Index) -> Int {
    return _base.distance(from: end.base, to: start.base)
  }

  @inlinable
  public subscript(position: Index) -> Element {
    return _base[_base.index(before: position.base)]
  }
}

extension ReversedCollection: RandomAccessCollection where Base: RandomAccessCollection { }

extension ReversedCollection {
  /// Reversing a reversed collection returns the original collection.
  ///
  /// - Complexity: O(1)
  @inlinable
  @available(swift, introduced: 4.2)
  public __consuming func reversed() -> Base {
    return _base
  }
}

extension BidirectionalCollection {
  /// Returns a view presenting the elements of the collection in reverse
  /// order.
  ///
  /// You can reverse a collection without allocating new space for its
  /// elements by calling this `reversed()` method. A `ReversedCollection`
  /// instance wraps an underlying collection and provides access to its
  /// elements in reverse order. This example prints the characters of a
  /// string in reverse order:
  ///
  ///     let word = "Backwards"
  ///     for char in word.reversed() {
  ///         print(char, terminator: "")
  ///     }
  ///     // Prints "sdrawkcaB"
  ///
  /// If you need a reversed collection of the same type, you may be able to
  /// use the collection's sequence-based or collection-based initializer. For
  /// example, to get the reversed version of a string, reverse its
  /// characters and initialize a new `String` instance from the result.
  ///
  ///     let reversedWord = String(word.reversed())
  ///     print(reversedWord)
  ///     // Prints "sdrawkcaB"
  ///
  /// - Complexity: O(1)
  @inlinable
  public __consuming func reversed() -> ReversedCollection<Self> {
    return ReversedCollection(_base: self)
  }
}
