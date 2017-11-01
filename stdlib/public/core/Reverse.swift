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

extension MutableCollection where Self : BidirectionalCollection {
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
  @_inlineable // FIXME(sil-serialize-all)
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

@available(*, deprecated, renamed: "ReversedCollection.Index")
public typealias ReversedIndex<T: BidirectionalCollection> = ReversedCollection<T>.Index

// FIXME(ABI)#59 (Conditional Conformance): we should have just one type,
// `ReversedCollection`, that has conditional conformances to
// `RandomAccessCollection`, and possibly `MutableCollection` and
// `RangeReplaceableCollection`.
// rdar://problem/17144340

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
///
/// - See also: `ReversedRandomAccessCollection`
@_fixed_layout
public struct ReversedCollection<
  Base : BidirectionalCollection
> : BidirectionalCollection {
  public let base: Base

  @_fixed_layout
  public struct Iterator: IteratorProtocol, Sequence {
    @_inlineable
    @inline(__always)
    /// Creates an iterator over the given collection.
    public /// @testable
    init(elements: Base, position: Base.Index) {
      self._elements = elements
      self._position = position
    }

    @_inlineable
    @inline(__always)
    public mutating func next() -> Base.Element? {
      guard _fastPath(_position != _elements.startIndex) else { return nil }
      _position = _elements.index(before: _position)
      return _elements[_position]
    }
  
    @_versioned
    internal let _elements: Base
    @_versioned
    internal var _position: Base.Index
  }
  
  @_inlineable
  @inline(__always)
  public func makeIterator() -> Iterator {
    return Iterator(elements: base, position: base.endIndex)
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public enum Index: Comparable {
      case position(Base.Index)
      case end

      public static func < (lhs: Index, rhs: Index) -> Bool {
          switch (lhs,rhs) {
          case let (.position(i),.position(j)):
              return j < i
          case (.position,.end):
              return true
          case (.end, _):
              return false
          }
      }

      public var base: Base.Index {
          guard case let .position(i) = self
          else { fatalError("Can't get equivalent of a reversed endIndex") }
          return i
      }
  }

  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  @_versioned
  @_inlineable
  internal init(base: Base) {
    self.base = base
  }

  public typealias IndexDistance = Base.IndexDistance
  
  @_inlineable
  public var startIndex: Index { 
    if base.startIndex == base.endIndex {
      return .end
    }
    else {
      return .position(base.index(before: base.endIndex)) 
    }
  }  


  @_inlineable
  public var endIndex: Index {
    return .end
  }

  public func index(after: Index) -> Index {
      switch after {
      case .end:
          fatalError("Can't advance beyond endIndex")
      case let .position(i) where i == base.startIndex:
          return .end
      case let .position(i):
          return .position(base.index(before: i))
      }
  }

  public func index(before: Index) -> Index {
      switch before {
      case .end:
        return .position(base.startIndex)
      case let .position(i):
          return .position(base.index(after: i))
      }
  }

  @_inlineable
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    switch i {
    case let .position(j) where n > 0:
      // go one less, to account for not going one past the base's start
      let k = base.index(j, offsetBy: -(n-1))
      // then go one more
      return k == base.startIndex ? .end : .position(base.index(before: k))
    case let .position(j):
      // backwards is easier... it's just forwards. base handles out-of-bounds.
      return .position(base.index(j, offsetBy: -n))
    case .end where n == 0:
      // end offset by nothing is still end
      return .end
    case .end where n < 0:
      // backwards from end by n is base.start offset by one less
      return .position(base.index(base.startIndex, offsetBy: -(n+1)))
    case .end:
      fatalError("Cannot advance beyond endIndex")
    }
  }

  @_inlineable
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    switch (start,end) {
    case let (.position(i),.position(j)):
      return base.distance(from: j, to: i)
    case (.end,.end):
      return 0
    case let (.position(i),.end):
      return base.distance(from: base.startIndex, to: i) + 1
    case let (.end,.position(i)):
      return base.distance(from: i, to: base.endIndex) - 1
    }
  }

  @_inlineable
  public subscript(i: Index) -> Base.Element {
    return base[i.base]
  }
}

/// A collection that presents the elements of its base collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reversed()` where `x` is a
///   collection having random access indices.
/// - See also: `ReversedCollection`
@_fixed_layout
public struct ReversedRandomAccessCollection<
  Base : RandomAccessCollection
> : RandomAccessCollection {
  public let base: Base

  @_fixed_layout
  public struct Iterator: IteratorProtocol, Sequence {
    @_inlineable
    @inline(__always)
    /// Creates an iterator over the given collection.
    public /// @testable
    init(elements: Base, position: Base.Index) {
      self._elements = elements
      self._position = position
    }
  
    @_inlineable
    @inline(__always)
    public mutating func next() -> Base.Element? {
      guard _fastPath(_position != _elements.startIndex) else { return nil }
      _position = _elements.index(before: _position)
      return _elements[_position]
    }
  
    @_versioned
    internal let _elements: Base
    @_versioned
    internal var _position: Base.Index
  }
  
  @_inlineable
  @inline(__always)
  public func makeIterator() -> Iterator {
    return Iterator(elements: base, position: base.endIndex)
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public enum Index: Comparable {
      case position(Base.Index)
      case end

      public static func < (lhs: Index, rhs: Index) -> Bool {
          switch (lhs,rhs) {
          case let (.position(i),.position(j)):
              return j < i
          case (.position,.end):
              return true
          case (.end, _):
              return false
          }
      }

      public var base: Base.Index {
          guard case let .position(i) = self
          else { fatalError("Can't get equivalent of a reversed endIndex") }
          return i
      }
  }

  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  @_versioned
  @_inlineable
  internal init(base: Base) {
    self.base = base
  }

  public typealias IndexDistance = Base.IndexDistance
  
  @_inlineable
  public var startIndex: Index { 
    if base.startIndex == base.endIndex {
      return .end
    }
    else {
      return .position(base.index(before: base.endIndex)) 
    }
  }  

  @_inlineable
  public var endIndex: Index {
    return .end
  }

  public func index(after: Index) -> Index {
      switch after {
      case .end:
          fatalError("Can't advance beyond endIndex")
      case let .position(i) where i == base.startIndex:
          return .end
      case let .position(i):
          return .position(base.index(before: i))
      }
  }

  public func index(before: Index) -> Index {
      switch before {
      case .end:
        return .position(base.startIndex)
      case let .position(i):
          return .position(base.index(after: i))
      }
  }

  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    switch i {
    case let .position(j) where n > 0:
      // go one less, to account for not going one past the base's start
      let k = base.index(j, offsetBy: -(n-1))
      // then go one more
      return k == base.startIndex ? .end : .position(base.index(before: k))
    case let .position(j):
      // backwards is easier... it's just forwards. base handles out-of-bounds.
      return .position(base.index(j, offsetBy: -n))
    case .end where n == 0:
      // end offset by nothing is still end
      return .end
    case .end where n < 0:
      // backwards from end by n is base.start offset by one less
      return .position(base.index(base.startIndex, offsetBy: -(n+1)))
    case .end:
      fatalError("Cannot advance beyond endIndex")
    }
  }
  
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    switch (start,end) {
    case let (.position(i),.position(j)):
      return base.distance(from: j, to: i)
    case (.end,.end):
      return 0
    case let (.position(i),.end):
      return base.distance(from: base.startIndex, to: i) + 1
    case let (.end,.position(i)):
      return base.distance(from: i, to: base.endIndex) - 1
    }
  }

  @_inlineable
  public subscript(i: Index) -> Base.Element {
    return base[i.base]
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
  @_inlineable
  public func reversed() -> ReversedCollection<Self> {
    return ReversedCollection(base: self)
  }
}

extension RandomAccessCollection {
  /// Returns a view presenting the elements of the collection in reverse
  /// order.
  ///
  /// You can reverse a collection without allocating new space for its
  /// elements by calling this `reversed()` method. A
  /// `ReversedRandomAccessCollection` instance wraps an underlying collection
  /// and provides access to its elements in reverse order. This example
  /// prints the elements of an array in reverse order:
  ///
  ///     let numbers = [3, 5, 7]
  ///     for number in numbers.reversed() {
  ///         print(number)
  ///     }
  ///     // Prints "7"
  ///     // Prints "5"
  ///     // Prints "3"
  ///
  /// If you need a reversed collection of the same type, you may be able to
  /// use the collection's sequence-based or collection-based initializer. For
  /// example, to get the reversed version of an array, initialize a new
  /// `Array` instance from the result of this `reversed()` method.
  ///
  ///     let reversedNumbers = Array(numbers.reversed())
  ///     print(reversedNumbers)
  ///     // Prints "[7, 5, 3]"
  ///
  /// - Complexity: O(1)
  @_inlineable
  public func reversed() -> ReversedRandomAccessCollection<Self> {
    return ReversedRandomAccessCollection(base: self)
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection {

  /// Returns the elements of the collection in reverse order.
  ///
  /// - Complexity: O(1)
  @_inlineable
  public func reversed() -> LazyBidirectionalCollection<
    ReversedCollection<Elements>
  > {
    return ReversedCollection(base: elements).lazy
  }
}

extension LazyCollectionProtocol
  where
  Self : RandomAccessCollection,
  Elements : RandomAccessCollection {

  /// Returns the elements of the collection in reverse order.
  ///
  /// - Complexity: O(1)
  @_inlineable
  public func reversed() -> LazyRandomAccessCollection<
    ReversedRandomAccessCollection<Elements>
  > {
    return ReversedRandomAccessCollection(base: elements).lazy
  }
}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
