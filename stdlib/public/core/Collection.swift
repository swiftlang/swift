//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that provides subscript access to its elements, with forward
/// index traversal.
///
/// In most cases, it's best to ignore this protocol and use the `Collection`
/// protocol instead, because it has a more complete interface.
public protocol IndexableBase {
  // FIXME(ABI)(compiler limitation): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  //
  // This protocol is almost an implementation detail of the standard
  // library; it is used to deduce things like the `SubSequence` and
  // `Iterator` type from a minimal collection, but it is also used in
  // exposed places like as a constraint on `IndexingIterator`.

  /// A type that represents a position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript
  /// argument.
  ///
  /// - SeeAlso: endIndex
  associatedtype Index : Comparable

  /// The position of the first element in a nonempty collection.
  ///
  /// If the collection is empty, `startIndex` is equal to `endIndex`.
  var startIndex: Index { get }

  /// The collection's "past the end" position, or one greater than the last
  /// valid subscript argument.
  ///
  /// When you need a range that includes the last element of a collection, use
  /// the half-open range operator (`..<`) with `endIndex`. The `..<` operator
  /// creates a range that doesn't include the upper bound, so it's always
  /// safe to use with `endIndex`. For example:
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let index = numbers.index(of: 30) {
  ///         print(numbers[index ..< numbers.endIndex])
  ///     }
  ///     // Prints "[30, 40, 50]"
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  var endIndex: Index { get }

  // The declaration of _Element and subscript here is a trick used to
  // break a cyclic conformance/deduction that Swift can't handle.  We
  // need something other than a Collection.Iterator.Element that can
  // be used as IndexingIterator<T>'s Element.  Here we arrange for
  // the Collection itself to have an Element type that's deducible from
  // its subscript.  Ideally we'd like to constrain this Element to be the same
  // as Collection.Iterator.Element (see below), but we have no way of
  // expressing it today.
  associatedtype _Element

  /// Accesses the element at the specified position.
  ///
  /// For example, access an element of an array through its subscript to
  /// print its value:
  ///
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     print(streets[1])
  ///     // Prints "Bryant"
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's end index. The end index refers to the position one
  /// past the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  subscript(position: Index) -> _Element { get }

  // WORKAROUND: rdar://25214066
  /// A `Sequence` that can represent a contiguous subrange of `self`'s
  /// elements.
  associatedtype SubSequence

  /// Accesses the subsequence bounded by `bounds`.
  ///
  /// - Complexity: O(1)
  ///
  /// - Precondition: `(startIndex...endIndex).contains(bounds.lowerBound)` 
  ///   and `(startIndex...endIndex).contains(bounds.upperBound)`
  subscript(bounds: Range<Index>) -> SubSequence { get }
  
  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(bounds.contains(index))
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>)

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(
  ///       bounds.contains(range.lowerBound) ||
  ///       range.lowerBound == bounds.upperBound)
  ///     precondition(
  ///       bounds.contains(range.upperBound) ||
  ///       range.upperBound == bounds.upperBound)
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>)

  /// Returns the position immediately after `i`.
  ///
  /// - Precondition: `(startIndex..<endIndex).contains(i)`
  @warn_unused_result
  func index(after i: Index) -> Index

  /// Replaces `i` with its successor.
  func formIndex(after i: inout Index)
}

public protocol Indexable : IndexableBase {
  /// A type that can represent the number of steps between pairs of
  /// `Index` values where one value is reachable from the other.
  ///
  /// Reachability is defined by the ability to produce one value from
  /// the other via zero or more applications of `index(after: i)`.
  associatedtype IndexDistance : SignedInteger = Int

  /// Returns the result of advancing `i` by `n` positions.
  ///
  /// - Returns:
  ///   - If `n > 0`, the `n`th successor of `i`.
  ///   - If `n < 0`, the `n`th predecessor of `i`.
  ///   - Otherwise, `i` unmodified.
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  /// - Precondition:
  ///   - If `n > 0`, `n <= self.distance(from: i, to: self.endIndex)`
  ///   - If `n < 0`, `n >= self.distance(from: i, to: self.startIndex)`
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  @warn_unused_result
  func index(_ i: Index, offsetBy n: IndexDistance) -> Index

  /// Returns the result of advancing `i` by `n` positions, or `nil`
  /// if doing so would pass `limit`.
  ///
  /// - Returns:
  ///   - `nil` if `(limit > i) == (n > 0) && abs(distance(i, limit)) < abs(n)`
  ///   - Otherwise, `index(i, offsetBy: n)`
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  @warn_unused_result
  func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index?

  /// Advances `i` by `n` positions.
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  /// - Precondition:
  ///   - If `n > 0`, `n <= self.distance(from: i, to: self.endIndex)`
  ///   - If `n < 0`, `n >= self.distance(from: i, to: self.startIndex)`
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  func formIndex(_ i: inout Index, offsetBy n: IndexDistance)

  /// Advances `i` by `n` positions, or until it equals `limit`.
  ///
  /// - Returns `true` if index has been advanced by exactly `n` steps without
  ///   passing the `limit`, and `false` otherwise.
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  func formIndex(
    _ i: inout Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Bool

  /// Returns the distance between `start` and `end`.
  ///
  /// - Precondition: `start <= end` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`n`) otherwise, where `n` is the method's result.
  @warn_unused_result
  func distance(from start: Index, to end: Index) -> IndexDistance
}

/// A type that iterates over a collection using its indices.
///
/// The `IndexingIterator` type is the default iterator for any collection that
/// doesn't declare its own. It acts as an iterator by using a collection's
/// indices to step over each value in the collection. Most collections in the
/// standard library use `IndexingIterator` as their iterator.
///
/// By default, any custom collection type you create will inherit a
/// `makeIterator()` method that returns an `IndexingIterator` instance,
/// making it unnecessary to declare your own. When creating a custom
/// collection type, add the minimal requirements of the `Collection`
/// protocol: starting and ending indices and a subscript for accessing
/// elements. With those elements defined, the inherited `makeIterator()`
/// method satisfies the requirements of the `Sequence` protocol.
///
/// Here's an example of a type that declares the minimal requirements for a
/// collection. The `CollectionOfTwo` structure is a fixed-size collection
/// that always holds two elements of a specific type.
///
///     struct CollectionOfTwo<Element>: Collection {
///         let elements: (Element, Element)
///
///         init(_ first: Element, _ second: Element) {
///             self.elements = (first, second)
///         }
///
///         var startIndex: Int { return 0 }
///         var endIndex: Int   { return 2 }
///
///         subscript(index: Int) -> Element {
///             switch index {
///             case 0: return elements.0
///             case 1: return elements.1
///             default: fatalError("Index out of bounds.")
///             }
///         }
///     }
///
/// The `CollectionOfTwo` type uses the default iterator type,
/// `IndexingIterator`, because it doesn't define its own `makeIterator()`
/// method or `Iterator` associated type. This example shows how a
/// `CollectionOfTwo` instance can be created holding the values of a point,
/// and then iterated over using a `for`-`in` loop.
///
///     let point = CollectionOfTwo(15.0, 20.0)
///     for element in point {
///         print(element)
///     }
///     // Prints "15.0"
///     // Prints "20.0"
public struct IndexingIterator<
  Elements : IndexableBase
  // FIXME(compiler limitation):
  // Elements : Collection
> : IteratorProtocol, Sequence {

  /// Creates an iterator over the given collection.
  public /// @testable
  init(_elements: Elements) {
    self._elements = _elements
    self._position = _elements.startIndex
  }

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Repeatedly calling this method returns all the elements of the underlying
  /// sequence in order. As soon as the sequence has run out of elements, the
  /// `next()` method returns `nil`.
  ///
  /// You must not call this method if it has previously returned `nil`.
  ///
  /// This example shows how an iterator can be used explicitly to emulate a
  /// `for`-`in` loop. First, retrieve a sequence's iterator, and then call
  /// the iterator's `next()` method until it returns `nil`.
  ///
  ///     let numbers = [2, 3, 5, 7]
  ///     var numbersIterator = numbers.makeIterator()
  ///
  ///     while let num = numbersIterator.next() {
  ///         print(num)
  ///     }
  ///     // Prints "2"
  ///     // Prints "3"
  ///     // Prints "5"
  ///     // Prints "7"
  ///
  /// - Returns: The next element in the underlying sequence if a next element
  ///   exists; otherwise, `nil`.
  public mutating func next() -> Elements._Element? {
    if _position == _elements.endIndex { return nil }
    let element = _elements[_position]
    _elements.formIndex(after: &_position)
    return element
  }

  internal let _elements: Elements
  internal var _position: Elements.Index
}

/// A sequence whose elements can be traversed multiple times,
/// nondestructively, and accessed by indexed subscript.
///
/// Collections are used extensively throughout the standard library. When
/// you use arrays, dictionaries, views of a string's contents and other
/// types, you benefit from the operations that the `Collection` protocol
/// declares and implements.
///
/// In addition to the methods that collections inherit from the `Sequence`
/// protocol, you gain access to methods that depend on accessing an element
/// at a specific position when using a collection.
///
/// For example, if you want to print only the first word in a string,
/// search for the index of the first space, and then create a subsequence up
/// to that position.
///
///     let text = "Buffalo buffalo buffalo buffalo."
///     if let firstSpace = text.characters.index(of: " ") {
///         print(String(text.characters.prefix(upTo: firstSpace)))
///     }
///     // Prints "Buffalo"
///
/// The `firstSpace` constant is an index into the `text.characters`
/// collection. `firstSpace` is the position of the first space in the
/// collection. You can store indices in variables, and pass them to
/// collection algorithms or use them later to access the corresponding
/// element. In the example above, `firstSpace` is used to extract the prefix
/// that contains elements up to that index.
///
/// You can pass only valid indices to collection operations. You can find a
/// complete set of a collection's valid indices by starting with the
/// collection's `startIndex` property and finding every successor up to, and
/// including, the `endIndex` property. All other values of the `Index` type,
/// such as the `startIndex` property of a different collection, are invalid
/// indices for this collection.
///
/// Saved indices may become invalid as a result of mutating operations; for
/// more information about index invalidation in mutable collections, see the
/// reference for the `MutableCollection` and `RangeReplaceableCollection`
/// protocols, as well as for the specific type you're using.
///
/// Accessing Individual Elements
/// =============================
///
/// You can access an element of a collection through its subscript with any
/// valid index except the collection's `endIndex` property, a "past-the-end"
/// index that does not correspond with any element of the collection.
///
/// Here's an example of accessing the first character in a string through its
/// subscript:
///
///     let firstChar = text.characters[text.characters.startIndex]
///     print(firstChar)
///     // Prints "B"
///
/// The `Collection` protocol declares and provides default implementations
/// for many operations that depend on elements being accessible by their
/// subscript. For example, you can also access the first character of
/// `text` using the `first` property, which has the value of the first
/// element of the collection, or `nil` if the collection is empty.
///
///     print(text.characters.first)
///     // Prints "Optional("B")"
///
/// Traversing a Collection 
/// =======================
///
/// While a sequence may be consumed as it is traversed, a collection is
/// guaranteed to be multi-pass: Any element may be repeatedly accessed by
/// saving its index. Moreover, a collection's indices form a finite range
/// of the positions of the collection's elements. This guarantees the
/// safety of operations that depend on a sequence being finite, such as
/// checking to see whether a collection contains an element.
///
/// Iterating over the elements of a collection by their positions yields the
/// same elements in the same order as iterating over that collection using
/// its iterator. This example demonstrates that the `characters` view of a
/// string returns the same characters in the same order whether the view's
/// indices or the view itself is being iterated.
///
///     let word = "Swift"
///     for character in word.characters {
///         print(character)
///     }
///     // Prints "S"
///     // Prints "w"
///     // Prints "i"
///     // Prints "f"
///     // Prints "t"
/// 
///     for i in word.characters.indices {
///         print(word.characters[i])
///     }
///     // Prints "S"
///     // Prints "w"
///     // Prints "i"
///     // Prints "f"
///     // Prints "t"
///
/// Conforming to the Collection Protocol 
/// =====================================
///
/// If you create a custom type that can provide repeated access to its
/// elements, conformance to the `Collection` protocol gives your
/// custom type a more useful and more efficient interface for sequence and
/// collection operations. To add conformance to your type, declare
/// `startIndex` and `endIndex` properties and a subscript that provides at
/// least read-only access to your type's elements.
///
/// Expected Performance
/// ====================
///
/// Types that conform to `Collection` are expected to provide the
/// `startIndex` and `endIndex` properties and subscript access to elements
/// as O(1) operations. Types that are not able to guarantee that expected
/// performance must document the departure, because many collection operations
/// depend on O(1) subscripting performance for their own performance
/// guarantees.
///
/// The performance of some collection operations depends on the type of index
/// that the collection provides. For example, a random-access collection,
/// which can measure the distance between two indices in O(1) time, will be
/// able to calculate its `count` property in O(1) time. Conversely, because a
/// forward or bidirectional collection must traverse the entire collection to
/// count the number of contained elements, accessing its `count` property is
/// an O(N) operation.
public protocol Collection : Indexable, Sequence {
  /// A type that can represent the number of steps between pairs of
  /// `Index` values where one value is reachable from the other.
  ///
  /// Reachability is defined by the ability to produce one value from
  /// the other via zero or more applications of `index(after:)`.
  associatedtype IndexDistance : SignedInteger = Int

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  ///
  /// By default, a `Collection` satisfies `Sequence` by
  /// supplying a `IndexingIterator` as its associated `Iterator`
  /// type.
  associatedtype Iterator : IteratorProtocol = IndexingIterator<Self>

  // FIXME: Needed here so that the `Iterator` is properly deduced from
  // a custom `makeIterator()` function.  Otherwise we get an
  // `IndexingIterator`. <rdar://problem/21539115>
  /// Returns an iterator over the elements of the collection.
  func makeIterator() -> Iterator

  /// A sequence that represents a contiguous subrange of the collection's
  /// elements.
  ///
  /// This associated type appears as a requirement in the `Sequence`
  /// protocol, but it is restated here with stricter constraints. In a
  /// collection, the subsequence should also conform to `Collection`.
  associatedtype SubSequence : IndexableBase, Sequence = Slice<Self>
  // FIXME(compiler limitation):
  // associatedtype SubSequence : Collection
  //   where
  //   Iterator.Element == SubSequence.Iterator.Element,
  //   SubSequence.Index == Index,
  //   SubSequence.Indices == Indices,
  //   SubSequence.SubSequence == SubSequence
  //
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  //
  // These constraints allow processing collections in generic code by
  // repeatedly slicing them in a loop.

  /// Accesses the element at the specified position.
  ///
  /// For example, access an element of an array through its subscript to
  /// print its value:
  ///
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     print(streets[1])
  ///     // Prints "Bryant"
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's end index. The end index refers to the position one
  /// past the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  subscript(position: Index) -> Iterator.Element { get }

  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     print(streets[index!])
  ///     // Prints "Evarts"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  subscript(bounds: Range<Index>) -> SubSequence { get }

  /// A collection type whose elements are the indices of `self` that
  /// are valid for subscripting, in ascending order.
  associatedtype Indices : IndexableBase, Sequence = DefaultIndices<Self>

  // FIXME(compiler limitation):
  // associatedtype Indices : Collection
  //   where
  //   Indices.Iterator.Element == Index,
  //   Indices.Index == Index,
  //   Indices.SubSequence == Indices
  //   = DefaultIndices<Self>

  /// The indices that are valid for subscripting `self`, in ascending order.
  ///
  /// - Note: `indices` can hold a strong reference to the collection itself,
  ///   causing the collection to be non-uniquely referenced.  If you need to
  ///   mutate the collection while iterating over its indices, use the
  ///   `index(after:)` method starting with `startIndex` to produce indices
  ///   instead.
  ///   
  ///   ```
  ///   var c = [10, 20, 30, 40, 50]
  ///   var i = c.startIndex
  ///   while i != c.endIndex {
  ///       c[i] /= 5
  ///       i = c.index(after: i)
  ///   }
  ///   // c == [2, 4, 6, 8, 10]
  ///   ```
  var indices: Indices { get }

  /// Returns a subsequence from the start of the collection up to, but not
  /// including, the specified position.
  ///
  /// The resulting subsequence *does not include* the element at the
  /// position `end`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.prefix(upTo: i))
  ///     }
  ///     // Prints "[10, 20, 30]"
  ///
  /// Passing the collection's starting index as the `end` parameter results in
  /// an empty subsequence.
  ///
  ///     print(numbers.prefix(upTo: numbers.startIndex))
  ///     // Prints "[]"
  ///
  /// - Parameter end: The "past-the-end" index of the resulting subsequence.
  ///   `end` must be a valid index of the collection.
  /// - Returns: A subsequence up to, but not including, the `end` position.
  ///
  /// - Complexity: O(1)
  /// - SeeAlso: `prefix(through:)`
  @warn_unused_result
  func prefix(upTo end: Index) -> SubSequence

  /// Returns a subsequence from the specified position to the end of the
  /// collection.
  ///
  /// For example:
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.suffix(from: i))
  ///     }
  ///     // Prints "[40, 50, 60]"
  ///
  /// Passing the collection's `endIndex` as the `start` parameter results in
  /// an empty subsequence.
  ///
  ///     print(numbers.suffix(from: numbers.endIndex))
  ///     // Prints "[]"
  ///
  /// - Parameter start: The index at which to start the resulting subsequence.
  ///   `start` must be a valid index of the collection.
  /// - Returns: A subsequence starting at the `start` position.
  ///
  /// - Precondition: `start >= self.startIndex && start <= self.endIndex`
  /// - Complexity: O(1)
  @warn_unused_result
  func suffix(from start: Index) -> SubSequence

  /// Returns a subsequence from the start of the collection through the
  /// specified position.
  ///
  /// The resulting subsequence *includes* the element at the position `end`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.prefix(through: i))
  ///     }
  ///     // Prints "[10, 20, 30, 40]"
  ///
  /// - Parameter end: The index of the last element to include in the
  ///   resulting subsequence. `end` must be a valid index of the collection
  ///   that is not equal to the `endIndex` property.
  /// - Returns: A subsequence up to, and including, the `end` position.
  ///
  /// - Complexity: O(1)
  /// - SeeAlso: `prefix(upTo:)`
  @warn_unused_result
  func prefix(through position: Index) -> SubSequence

  /// A Boolean value indicating whether the collection is empty.
  ///
  /// When you need to check whether your collection is empty, use the
  /// `isEmpty` property instead of checking that the `count` property is
  /// equal to zero. For collections that don't conform to
  /// `RandomAccessCollection`, accessing the `count` property iterates
  /// through the elements of the collection.
  ///
  ///     let horseName = "Silver"
  ///     if horseName.characters.isEmpty {
  ///         print("I've been through the desert on a horse with no name.")
  ///     } else {
  ///         print("Hi ho, \(horseName)!")
  ///     }
  ///     // Prints "Hi ho, Silver!")
  ///
  /// - Complexity: O(1)
  var isEmpty: Bool { get }

  /// The number of elements in the collection.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*n*), where *n* is the length
  ///   of the collection.
  var count: IndexDistance { get }
  
  // The following requirement enables dispatching for index(of:) when
  // the element type is Equatable.
  /// Returns `Optional(Optional(index))` if an element was found
  /// or `Optional(nil)` if an element was determined to be missing;
  /// otherwise, `nil`.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  func _customIndexOfEquatableElement(_ element: Iterator.Element) -> Index??

  /// The first element of the collection.
  ///
  /// If the collection is empty, the value of this property is `nil`.
  /// 
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let firstNumber = numbers.first {
  ///         print(firstNumber)
  ///     }
  ///     // Prints "10"
  var first: Iterator.Element? { get }

  /// Returns the result of advancing `i` by `n` positions.
  ///
  /// - Returns:
  ///   - If `n > 0`, the `n`th successor of `i`.
  ///   - If `n < 0`, the `n`th predecessor of `i`.
  ///   - Otherwise, `i` unmodified.
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  /// - Precondition:
  ///   - If `n > 0`, `n <= self.distance(from: i, to: self.endIndex)`
  ///   - If `n < 0`, `n >= self.distance(from: i, to: self.startIndex)`
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  @warn_unused_result
  func index(_ i: Index, offsetBy n: IndexDistance) -> Index

  // FIXME: swift-3-indexing-model: Should this mention preconditions on `n`?
  /// Returns the result of advancing `i` by `n` positions, or `nil`
  /// if doing so would pass `limit`.
  ///
  /// - Returns:
  ///   - `nil` if `(limit > i) == (n > 0) && abs(distance(i, limit)) < abs(n)`
  ///   - Otherwise, `index(i, offsetBy: n)`
  ///
  /// - Precondition: `n >= 0` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`abs(n)`) otherwise.
  @warn_unused_result
  func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index?

  /// Returns the distance between `start` and `end`.
  ///
  /// - Precondition: `start <= end` unless `Self` conforms to
  ///   `BidirectionalCollection`.
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`.
  ///   - O(`n`) otherwise, where `n` is the method's result.
  @warn_unused_result
  func distance(from start: Index, to end: Index) -> IndexDistance
}

/// Default implementation for forward collections.
extension Indexable {
  @inline(__always)
  public func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: tests.
    i = index(after: i)
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    // FIXME: swift-3-indexing-model: tests.
    _precondition(
      bounds.lowerBound <= index,
      "out of bounds: index < startIndex")
    _precondition(
      index < bounds.upperBound,
      "out of bounds: index >= endIndex")
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    // FIXME: swift-3-indexing-model: tests.
    _precondition(
      bounds.lowerBound <= range.lowerBound,
      "out of bounds: range begins before startIndex")
    _precondition(
      range.lowerBound <= bounds.upperBound,
      "out of bounds: range ends after endIndex")
    _precondition(
      bounds.lowerBound <= range.upperBound,
      "out of bounds: range ends before bounds.lowerBound")
    _precondition(
      range.upperBound <= bounds.upperBound,
      "out of bounds: range begins after bounds.upperBound")
  }

  @warn_unused_result
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    // FIXME: swift-3-indexing-model: tests.
    return self._advanceForward(i, by: n)
  }

  @warn_unused_result
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    // FIXME: swift-3-indexing-model: tests.
    return self._advanceForward(i, by: n, limitedBy: limit)
  }

  public func formIndex(_ i: inout Index, offsetBy n: IndexDistance) {
    i = index(i, offsetBy: n)
  }

  public func formIndex(
    _ i: inout Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Bool {
    if let advancedIndex = index(i, offsetBy: n, limitedBy: limit) {
      i = advancedIndex
      return true
    }
    i = limit
    return false
  }
  
  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    // FIXME: swift-3-indexing-model: tests.
    _precondition(start <= end,
      "Only BidirectionalCollections can have end come before start")

    var start = start
    var count: IndexDistance = 0
    while start != end {
      count = count + 1
      formIndex(after: &start)
    }
    return count
  }

  /// Do not use this method directly; call advanced(by: n) instead.
  @inline(__always)
  @warn_unused_result
  internal func _advanceForward(_ i: Index, by n: IndexDistance) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")

    var i = i
    for _ in stride(from: 0, to: n, by: 1) {
      formIndex(after: &i)
    }
    return i
  }

  /// Do not use this method directly; call advanced(by: n, limit) instead.
  @inline(__always)
  @warn_unused_result
  internal
  func _advanceForward(
    _ i: Index, by n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")

    var i = i
    for _ in stride(from: 0, to: n, by: 1) {
      if i == limit {
        return nil
      }
      formIndex(after: &i)
    }
    return i
  }
}

/// Supply the default `makeIterator()` method for `Collection` models
/// that accept the default associated `Iterator`,
/// `IndexingIterator<Self>`.
extension Collection where Iterator == IndexingIterator<Self> {
  /// Returns an iterator over the elements of the collection.
  public func makeIterator() -> IndexingIterator<Self> {
    return IndexingIterator(_elements: self)
  }
}

/// Supply the default "slicing" `subscript` for `Collection` models
/// that accept the default associated `SubSequence`, `Slice<Self>`.
extension Collection where SubSequence == Slice<Self> {
  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     print(streets[index!])
  ///     // Prints "Evarts"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  public subscript(bounds: Range<Index>) -> Slice<Self> {
    _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
    return Slice(base: self, bounds: bounds)
  }
}

// TODO: swift-3-indexing-model - review the following
extension Collection where SubSequence == Self {
  /// Removes and returns the first element of the collection.
  ///
  /// - Returns: The first element of the collection if the collection is
  ///   not empty; otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public mutating func popFirst() -> Iterator.Element? {
    guard !isEmpty else { return nil }
    let element = first!
    self = self[index(after: startIndex)..<endIndex]
    return element
  }
}

/// Default implementations of core requirements
extension Collection {
  /// A Boolean value indicating whether the collection is empty.
  ///
  /// When you need to check whether your collection is empty, use the
  /// `isEmpty` property instead of checking that the `count` property is
  /// equal to zero. For collections that don't conform to
  /// `RandomAccessCollection`, accessing the `count` property iterates
  /// through the elements of the collection.
  ///
  ///     let horseName = "Silver"
  ///     if horseName.characters.isEmpty {
  ///         print("I've been through the desert on a horse with no name.")
  ///     } else {
  ///         print("Hi ho, \(horseName)!")
  ///     }
  ///     // Prints "Hi ho, Silver!")
  ///
  /// - Complexity: O(1)
  public var isEmpty: Bool {
    return startIndex == endIndex
  }

  /// The first element of the collection.
  ///
  /// If the collection is empty, the value of this property is `nil`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let firstNumber = numbers.first {
  ///         print(firstNumber)
  ///     }
  ///     // Prints "10"
  public var first: Iterator.Element? {
    // NB: Accessing `startIndex` may not be O(1) for some lazy collections,
    // so instead of testing `isEmpty` and then returning the first element,
    // we'll just rely on the fact that the iterator always yields the
    // first element first.
    var i = makeIterator()
    return i.next()
  }
// TODO: swift-3-indexing-model - uncomment and replace above ready (or should we still use the iterator one?)
  /// Returns the first element of `self`, or `nil` if `self` is empty.
  ///
  /// - Complexity: O(1)
  //  public var first: Iterator.Element? {
  //    return isEmpty ? nil : self[startIndex]
  //  }

// TODO: swift-3-indexing-model - review the following
  /// A value less than or equal to the number of elements in the collection.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*n*), where *n* is the length
  ///   of the collection.
  public var underestimatedCount: Int {
    return numericCast(count)
  }

  /// The number of elements in the collection.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*n*), where *n* is the length
  ///   of the collection.
  public var count: IndexDistance {
    return distance(from: startIndex, to: endIndex)
  }

// TODO: swift-3-indexing-model - rename the following to _customIndexOfEquatable(element)?
  /// Customization point for `Sequence.index(of:)`.
  ///
  /// Define this method if the collection can find an element in less than
  /// O(N) by exploiting collection-specific knowledge.
  ///
  /// - Returns: `nil` if a linear search should be attempted instead,
  ///   `Optional(nil)` if the element was not found, or
  ///   `Optional(Optional(index))` if an element was found.
  ///
  /// - Complexity: O(`count`).
  @warn_unused_result
  public // dispatching
  func _customIndexOfEquatableElement(_: Iterator.Element) -> Index?? {
    return nil
  }
}

//===----------------------------------------------------------------------===//
// Default implementations for Collection
//===----------------------------------------------------------------------===//

extension Collection {
// TODO: swift-3-indexing-model - review the following
  /// Returns an array containing the results of mapping the given closure
  /// over the sequence's elements.
  ///
  /// In this example, `map` is used first to convert the names in the array
  /// to lowercase strings and then to count their characters.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let lowercaseNames = cast.map { $0.lowercaseString }
  ///     // 'lowercaseNames' == ["vivien", "marlon", "kim", "karl"]
  ///     let letterCounts = cast.map { $0.characters.count }
  ///     // 'letterCounts' == [6, 6, 3, 4]
  ///
  /// - Parameter transform: A mapping closure. `transform` accepts an
  ///   element of this sequence as its parameter and returns a transformed
  ///   value of the same or of a different type.
  /// - Returns: An array containing the transformed elements of this
  ///   sequence.
  @warn_unused_result
  public func map<T>(
    _ transform: @noescape (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    let count: Int = numericCast(self.count)
    if count == 0 {
      return []
    }

    var result = ContiguousArray<T>()
    result.reserveCapacity(count)

    var i = self.startIndex

    for _ in 0..<count {
      result.append(try transform(self[i]))
      formIndex(after: &i)
    }

    _expectEnd(i, self)
    return Array(result)
  }

  /// Returns a subsequence containing all but the given number of initial
  /// elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the sequence, the result is an empty subsequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.dropFirst(2))
  ///     // Prints "[3, 4, 5]"
  ///     print(numbers.dropFirst(10))
  ///     // Prints "[]"
  ///
  /// - Parameter n: The number of elements to drop from the beginning of
  ///   the sequence. `n` must be greater than or equal to zero.
  /// - Returns: A subsequence starting after the specified number of
  ///   elements.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements to drop from
  ///   the beginning of the sequence.
  @warn_unused_result
  public func dropFirst(_ n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    let start = index(startIndex,
      offsetBy: numericCast(n), limitedBy: endIndex) ?? endIndex
    return self[start..<endIndex]
  }

  /// Returns a subsequence containing all but the specified number of final
  /// elements.
  ///
  /// The sequence must be finite. If the number of elements to drop exceeds
  /// the number of elements in the sequence, the result is an empty
  /// subsequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.dropLast(2))
  ///     // Prints "[1, 2, 3]"
  ///     print(numbers.dropLast(10))
  ///     // Prints "[]"
  ///
  /// - Parameter n: The number of elements to drop off the end of the
  ///   sequence. `n` must be greater than or equal to zero.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @warn_unused_result
  public func dropLast(_ n: Int) -> SubSequence {
    _precondition(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let amount = Swift.max(0, numericCast(count) - n)
    let end = index(startIndex,
      offsetBy: numericCast(amount), limitedBy: endIndex) ?? endIndex
    return self[startIndex..<end]
  }

  /// Returns a subsequence, up to the specified maximum length, containing
  /// the initial elements of the sequence.
  ///
  /// If the maximum length exceeds the number of elements in the sequence,
  /// the result contains all the elements in the sequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.prefix(2))
  ///     // Prints "[1, 2]"
  ///     print(numbers.prefix(10))
  ///     // Prints "[1, 2, 3, 4, 5]"
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A subsequence starting at the beginning of this sequence
  ///   with at most `maxLength` elements.
  @warn_unused_result
  public func prefix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a prefix of negative length from a collection")
    let end = index(startIndex,
      offsetBy: numericCast(maxLength), limitedBy: endIndex) ?? endIndex
    return self[startIndex..<end]
  }

  /// Returns a subsequence, up to the given maximum length, containing the
  /// final elements of the sequence.
  ///
  /// The sequence must be finite. If the maximum length exceeds the number
  /// of elements in the sequence, the result contains all the elements in
  /// the sequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.suffix(2))
  ///     // Prints "[4, 5]"
  ///     print(numbers.suffix(10))
  ///     // Prints "[1, 2, 3, 4, 5]"
  ///
  /// - Parameter maxLength: The maximum number of elements to return. Must
  ///   be greater than or equal to zero.
  /// - Returns: A subsequence terminating at the end of this sequence with
  ///   at most `maxLength` elements.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @warn_unused_result
  public func suffix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let amount = Swift.max(0, numericCast(count) - maxLength)
    let start = index(startIndex,
      offsetBy: numericCast(amount), limitedBy: endIndex) ?? endIndex
    return self[start..<endIndex]
  }

  /// Returns a subsequence from the start of the collection up to, but not
  /// including, the specified position.
  ///
  /// The resulting subsequence *does not include* the element at the
  /// position `end`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.prefix(upTo: i))
  ///     }
  ///     // Prints "[10, 20, 30]"
  ///
  /// Passing the collection's starting index as the `end` parameter results in
  /// an empty subsequence.
  ///
  ///     print(numbers.prefix(upTo: numbers.startIndex))
  ///     // Prints "[]"
  ///
  /// - Parameter end: The "past-the-end" index of the resulting subsequence.
  ///   `end` must be a valid index of the collection.
  /// - Returns: A subsequence up to, but not including, the `end` position.
  ///
  /// - Precondition: `end >= self.startIndex && end <= self.endIndex`
  /// - Complexity: O(1)
  /// - SeeAlso: `prefix(through:)`
  @warn_unused_result
  public func prefix(upTo end: Index) -> SubSequence {
    return self[startIndex..<end]
  }

  /// Returns a subsequence from the specified position to the end of the
  /// collection.
  ///
  /// For example:
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.suffix(from: i))
  ///     }
  ///     // Prints "[40, 50, 60]"
  ///
  /// Passing the collection's `endIndex` as the `start` parameter results in
  /// an empty subsequence.
  ///
  ///     print(numbers.suffix(from: numbers.endIndex))
  ///     // Prints "[]"
  ///
  /// - Parameter start: The index at which to start the resulting
  ///   subsequence. `start` must be a valid index of the collection.
  /// - Returns: A subsequence starting at the `start` position.
  ///
  /// - Precondition: `start >= self.startIndex && start <= self.endIndex`
  /// - Complexity: O(1)
  @warn_unused_result
  public func suffix(from start: Index) -> SubSequence {
    return self[start..<endIndex]
  }

  /// Returns a subsequence from the start of the collection through the
  /// specified position.
  ///
  /// The resulting subsequence *includes* the element at the position
  /// `end`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60]
  ///     if let i = numbers.index(of: 40) {
  ///         print(numbers.prefix(through: i))
  ///     }
  ///     // Prints "[10, 20, 30, 40]"
  ///
  /// - Parameter end: The index of the last element to include in the
  ///   resulting subsequence. `end` must be a valid index of the collection
  ///   that is not equal to the `endIndex` property.
  /// - Returns: A subsequence up to, and including, the `end` position.
  ///
  /// - Complexity: O(1)
  /// - SeeAlso: `prefix(upTo:)`
  @warn_unused_result
  public func prefix(through position: Index) -> SubSequence {
    return prefix(upTo: index(after: position))
  }

  // TODO: swift-3-indexing-model - review the following
  /// Returns the longest possible subsequences of the sequence, in order, that
  /// don't contain elements satisfying the given predicate. Elements that are
  /// used to split the sequence are not returned as part of any subsequence.
  ///
  /// The following examples show the effects of the `maxSplits` and
  /// `omittingEmptySubsequences` parameters when splitting a string using a
  /// closure that matches spaces. The first use of `split` returns each word
  /// that was originally separated by one or more spaces.
  ///
  ///     let line = "BLANCHE:   I don't want realism. I want magic!"
  ///     print(line.characters.split(isSeparator: { $0 == " " })
  ///                          .map(String.init))
  ///     // Prints "["BLANCHE:", "I", "don't", "want", "realism.", "I", "want", "magic!"]"
  ///
  /// The second example passes `1` for the `maxSplits` parameter, so the
  /// original string is split just once, into two new strings.
  ///
  ///     print(line.characters.split(maxSplits: 1, isSeparator: { $0 == " " })
  ///                           .map(String.init))
  ///     // Prints "["BLANCHE:", "  I don\'t want realism. I want magic!"]"
  ///
  /// The final example passes `false` for the `omittingEmptySubsequences`
  /// parameter, so the returned array contains empty strings where spaces
  /// were repeated.
  ///
  ///     print(line.characters.split(omittingEmptySubsequences: false, isSeparator: { $0 == " " })
  ///                           .map(String.init))
  ///     // Prints "["BLANCHE:", "", "", "I", "don\'t", "want", "realism.", "I", "want", "magic!"]"
  ///
  /// - Parameters:
  ///   - maxSplits: The maximum number of times to split the sequence, or one
  ///     less than the number of subsequences to return. If `maxSplits + 1`
  ///     subsequences are returned, the last one is a suffix of the original
  ///     sequence containing the remaining elements. `maxSplits` must be
  ///     greater than or equal to zero.
  ///   - omittingEmptySubsequences: If `false`, an empty subsequence is
  ///     returned in the result for each pair of consecutive elements
  ///     satisfying the `isSeparator` predicate and for each element at the
  ///     start or end of the sequence satisfying the `isSeparator` predicate.
  ///   - isSeparator: A closure that takes an element as an argument and
  ///     returns a Boolean value indicating whether the sequence should be
  ///     split at that element.
  /// - Returns: An array of subsequences, split from this sequence's elements.
  @warn_unused_result
  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    isSeparator: @noescape (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    _precondition(maxSplits >= 0, "Must take zero or more splits")

    var result: [SubSequence] = []
    var subSequenceStart: Index = startIndex

    func appendSubsequence(end: Index) -> Bool {
      if subSequenceStart == end && omittingEmptySubsequences {
        return false
      }
      result.append(self[subSequenceStart..<end])
      return true
    }

    if maxSplits == 0 || isEmpty {
      _ = appendSubsequence(end: endIndex)
      return result
    }

    var subSequenceEnd = subSequenceStart
    let cachedEndIndex = endIndex
    while subSequenceEnd != cachedEndIndex {
      if try isSeparator(self[subSequenceEnd]) {
        let didAppend = appendSubsequence(end: subSequenceEnd)
        formIndex(after: &subSequenceEnd)
        subSequenceStart = subSequenceEnd
        if didAppend && result.count == maxSplits {
          break
        }
        continue
      }
      formIndex(after: &subSequenceEnd)
    }

    if subSequenceStart != cachedEndIndex || !omittingEmptySubsequences {
      result.append(self[subSequenceStart..<cachedEndIndex])
    }

    return result
  }
}

// TODO: swift-3-indexing-model - review the following
extension Collection where Iterator.Element : Equatable {
  /// Returns the longest possible subsequences of the sequence, in order, that
  /// don't contain elements satisfying the given predicate. Elements that are
  /// used to split the sequence are not returned as part of any subsequence.
  ///
  /// The following examples show the effects of the `maxSplits` and
  /// `omittingEmptySubsequences` parameters when splitting a string using a
  /// closure that matches spaces. The first use of `split` returns each word
  /// that was originally separated by one or more spaces.
  ///
  ///     let line = "BLANCHE:   I don't want realism. I want magic!"
  ///     print(line.characters.split(isSeparator: { $0 == " " })
  ///                          .map(String.init))
  ///     // Prints "["BLANCHE:", "I", "don't", "want", "realism.", "I", "want", "magic!"]"
  ///
  /// The second example passes `1` for the `maxSplits` parameter, so the
  /// original string is split just once, into two new strings.
  ///
  ///     print(line.characters.split(maxSplits: 1, isSeparator: { $0 == " " })
  ///                           .map(String.init))
  ///     // Prints "["BLANCHE:", "  I don\'t want realism. I want magic!"]"
  ///
  /// The final example passes `false` for the `omittingEmptySubsequences`
  /// parameter, so the returned array contains empty strings where spaces
  /// were repeated.
  ///
  ///     print(line.characters.split(omittingEmptySubsequences: false, isSeparator: { $0 == " " })
  ///                           .map(String.init))
  ///     // Prints "["BLANCHE:", "", "", "I", "don\'t", "want", "realism.", "I", "want", "magic!"]"
  ///
  /// - Parameters:
  ///   - maxSplits: The maximum number of times to split the sequence, or one
  ///     less than the number of subsequences to return. If `maxSplits + 1`
  ///     subsequences are returned, the last one is a suffix of the original
  ///     sequence containing the remaining elements. `maxSplits` must be
  ///     greater than or equal to zero.
  ///   - omittingEmptySubsequences: If `false`, an empty subsequence is
  ///     returned in the result for each pair of consecutive elements
  ///     satisfying the `isSeparator` predicate and for each element at the
  ///     start or end of the sequence satisfying the `isSeparator` predicate.
  ///   - isSeparator: A closure that takes an element as an argument and
  ///     returns a Boolean value indicating whether the sequence should be
  ///     split at that element.
  /// - Returns: An array of subsequences, split from this sequence's elements.
  @warn_unused_result
  public func split(
    separator: Iterator.Element,
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true
  ) -> [SubSequence] {
  return split(
    maxSplits: maxSplits,
    omittingEmptySubsequences: omittingEmptySubsequences,
    isSeparator: { $0 == separator })
  }
}

// TODO: swift-3-indexing-model - review the following
extension Collection where SubSequence == Self {
  /// Removes and returns the first element of the collection.
  ///
  /// The collection must not be empty.
  ///
  /// - Returns: The first element of the collection.
  ///
  /// - Complexity: O(1)
  /// - SeeAlso: `popFirst()`
  @discardableResult
  public mutating func removeFirst() -> Iterator.Element {
    _precondition(!isEmpty, "can't remove items from an empty collection")
    let element = first!
    self = self[index(after: startIndex)..<endIndex]
    return element
  }

  /// Removes the specified number of elements from the beginning of the
  /// collection.
  ///
  /// - Parameter n: The number of elements to remove. `n` must be greater than
  ///   or equal to zero and less than or equal to the number of elements in
  ///   the collection.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*n*).
  public mutating func removeFirst(_ n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[index(startIndex, offsetBy: numericCast(n))..<endIndex]
  }
}

// TODO: swift-3-indexing-model - review the following
extension Sequence
  where Self : _ArrayProtocol, Self.Element == Self.Iterator.Element {
  // A fast implementation for when you are backed by a contiguous array.
  @discardableResult
  public func _copyContents(
    initializing ptr: UnsafeMutablePointer<Iterator.Element>
  ) -> UnsafeMutablePointer<Iterator.Element> {
    if let s = self._baseAddressIfContiguous {
      let count = self.count
      ptr.initializeFrom(s, count: count)
      _fixLifetime(self._owner)
      return ptr + count
    } else {
      var p = ptr
      for x in self {
        p.initialize(with: x)
        p += 1
      }
      return p
    }
  }
}

extension Collection {
  public func _preprocessingPass<R>(
    _ preprocess: @noescape () throws -> R
  ) rethrows -> R? {
    return try preprocess()
  }
}

@available(*, unavailable, message: "Bit enum has been removed. Please use Int instead.")
public enum Bit {}

@available(*, unavailable, renamed: "IndexingIterator")
public struct IndexingGenerator<Elements : IndexableBase> {}

@available(*, unavailable, renamed: "Collection")
public typealias CollectionType = Collection

extension Collection {
  @available(*, unavailable, renamed: "Iterator")
  public typealias Generator = Iterator

  @available(*, unavailable, renamed: "makeIterator")
  public func generate() -> Iterator {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Removed in Swift 3. Please use underestimatedCount property.")
  public func underestimateCount() -> Int {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Please use split(maxSplits:omittingEmptySubsequences:isSeparator:) instead")
  public func split(
    _ maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false,
    isSeparator: @noescape (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Builtin.unreachable()
  }
}

extension Collection where Iterator.Element : Equatable {
  @available(*, unavailable, message: "Please use split(separator:maxSplits:omittingEmptySubsequences:) instead")
  public func split(
    _ separator: Iterator.Element,
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false
  ) -> [SubSequence] {
    Builtin.unreachable()
  }
}

@available(*, unavailable, message: "PermutationGenerator has been removed in Swift 3")
public struct PermutationGenerator<C : Collection, Indices : Sequence> {}
