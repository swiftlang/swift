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

/// Encapsulates iteration state and interface for iteration over a
/// sequence.
///
/// - Note: While it is safe to copy an iterator, advancing one
///   copy may invalidate the others.
///
/// Any code that uses multiple iterators (or `for`...`in` loops)
/// over a single sequence should have static knowledge that the
/// specific sequence is multi-pass, either because its concrete
/// type is known or because it is constrained to `CollectionType`.
/// Also, the iterators must be obtained by distinct calls to the
/// sequence's `iterator()` method, rather than by copying.
public protocol IteratorProtocol {
  /// The type of element traversed by `self`.
  associatedtype Element

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.  Specific implementations of this protocol
  ///   are encouraged to respond to violations of this requirement by
  ///   calling `preconditionFailure("...")`.
  @warn_unused_result
  mutating func next() -> Element?
}

/// A type that can be iterated with a `for`...`in` loop.
///
/// `Sequence` makes no requirement on conforming types regarding
/// whether they will be destructively "consumed" by iteration.  To
/// ensure non-destructive iteration, constrain your sequence to
/// `Collection`.
///
/// As a consequence, it is not possible to run multiple `for` loops
/// on a sequence to "resume" iteration:
///
///     for element in sequence {
///       if ... some condition { break }
///     }
///
///     for element in sequence {
///       // Not guaranteed to continue from the next element.
///     }
///
/// `Sequence` makes no requirement about the behavior in that
/// case.  It is not correct to assume that a sequence will either be
/// "consumable" and will resume iteration, or that a sequence is a
/// collection and will restart iteration from the first element.
/// A conforming sequence that is not a collection is allowed to
/// produce an arbitrary sequence of elements from the second iterator.
public protocol Sequence {
  //@available(*, unavailable, renamed="Iterator")
  //typealias Generator = ()

  /// A type that provides the *sequence*'s iteration interface and
  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype Iterator : IteratorProtocol

  // FIXME: should be constrained to Sequence
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)

  /// A type that represents a subsequence of some of the elements.
  associatedtype SubSequence

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @warn_unused_result
  func iterator() -> Iterator

  /// Returns a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - Complexity: O(N).
  var underestimatedCount: Int { get }

  /// Returns an `Array` containing the results of mapping `transform`
  /// over `self`.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  func map<T>(
    @noescape transform: (Iterator.Element) throws -> T
  ) rethrows -> [T]

  /// Returns an `Array` containing the elements of `self`,
  /// in order, that satisfy the predicate `includeElement`.
  @warn_unused_result
  func filter(
    @noescape includeElement: (Iterator.Element) throws -> Bool
  ) rethrows -> [Iterator.Element]

  /// Call `body` on each element in `self` in the same order as a
  /// *for-in loop.*
  ///
  ///     sequence.forEach {
  ///       // body code
  ///     }
  ///
  /// is similar to:
  ///
  ///     for element in sequence {
  ///       // body code
  ///     }
  ///
  /// - Note: You cannot use the `break` or `continue` statement to exit the
  ///   current call of the `body` closure or skip subsequent calls.
  /// - Note: Using the `return` statement in the `body` closure will only
  ///   exit from the current call to `body`, not any outer scope, and won't
  ///   skip subsequent calls.
  ///
  /// - Complexity: O(`self.count`)
  func forEach(@noescape body: (Iterator.Element) throws -> Void) rethrows

  /// Returns a subsequence containing all but the first `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  func dropFirst(n: Int) -> SubSequence

  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `self` is a finite sequence.
  /// - Requires: `n >= 0`
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  func dropLast(n: Int) -> SubSequence

  /// Returns a subsequence, up to `maxLength` in length, containing the
  /// initial elements.
  ///
  /// If `maxLength` exceeds `self.count`, the result contains all
  /// the elements of `self`.
  ///
  /// - Requires: `maxLength >= 0`
  @warn_unused_result
  func prefix(maxLength: Int) -> SubSequence

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `s`.
  ///
  /// If `maxLength` exceeds `s.count`, the result contains all
  /// the elements of `s`.
  ///
  /// - Requires: `self` is a finite sequence.
  /// - Requires: `maxLength >= 0`
  @warn_unused_result
  func suffix(maxLength: Int) -> SubSequence

  /// Returns the maximal `SubSequence`s of `self`, in order, that
  /// don't contain elements satisfying the predicate `isSeparator`.
  ///
  /// - Parameter maxSplits: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplits + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing *all* the elements of `self` following the
  ///   last split point.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter omittingEmptySubsequences: If `false`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   satisfying `isSeparator`.
  ///   The default value is `true`.
  ///
  /// - Requires: `maxSplits >= 0`
  @warn_unused_result
  func split(
    maxSplits maxSplits: Int, omittingEmptySubsequences: Bool,
    @noescape isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence]

  @warn_unused_result
  func _customContainsEquatableElement(
    element: Iterator.Element
  ) -> Bool?

  /// If `self` is multi-pass (i.e., a `Collection`), invoke `preprocess` and
  /// return its result.  Otherwise, return `nil`.
  func _preprocessingPass<R>(@noescape preprocess: () -> R) -> R?

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  func _copyToNativeArrayBuffer() -> _ContiguousArrayBuffer<Iterator.Element>

  /// Copy a Sequence into an array, returning one past the last
  /// element initialized.
  func _initializeTo(ptr: UnsafeMutablePointer<Iterator.Element>)
    -> UnsafeMutablePointer<Iterator.Element>
}

/// A default iterator() function for `IteratorProtocol` instances that
/// are declared to conform to `Sequence`
extension Sequence
  where Self.Iterator == Self, Self : IteratorProtocol {
  public func iterator() -> Self {
    return self
  }
}

/// A sequence that lazily consumes and drops `n` elements from an underlying
/// `Base` iterator before possibly returning the first available element.
///
/// The underlying iterator's sequence may be infinite.
///
/// This is a class - we require reference semantics to keep track
/// of how many elements we've already dropped from the underlying sequence.
internal class _DropFirstSequence<Base : IteratorProtocol>
    : Sequence, IteratorProtocol {

  internal var _iterator: Base
  internal let _limit: Int
  internal var _dropped: Int

  internal init(_ iterator: Base, limit: Int, dropped: Int = 0) {
    self._iterator = iterator
    self._limit = limit
    self._dropped = dropped
  }

  internal func iterator() -> _DropFirstSequence<Base> {
    return self
  }

  internal func next() -> Base.Element? {
    while _dropped < _limit {
      if _iterator.next() == nil {
        _dropped = _limit
        return nil
      }
      _dropped += 1
    }
    return _iterator.next()
  }

  internal func dropFirst(n: Int) -> AnySequence<Base.Element> {
    // If this is already a _DropFirstSequence, we need to fold in
    // the current drop count and drop limit so no data is lost.
    //
    // i.e. [1,2,3,4].dropFirst(1).dropFirst(1) should be equivalent to
    // [1,2,3,4].dropFirst(2).
    return AnySequence(
      _DropFirstSequence(_iterator, limit: _limit + n, dropped: _dropped))
  }
}

/// A sequence that only consumes up to `n` elements from an underlying
/// `Base` iterator.
///
/// The underlying iterator's sequence may be infinite.
///
/// This is a class - we require reference semantics to keep track
/// of how many elements we've already taken from the underlying sequence.
internal class _PrefixSequence<Base : IteratorProtocol>
    : Sequence, IteratorProtocol {
  internal let _maxLength: Int
  internal var _iterator: Base
  internal var _taken: Int

  internal init(_ iterator: Base, maxLength: Int, taken: Int = 0) {
    self._iterator = iterator
    self._maxLength = maxLength
    self._taken = taken
  }

  internal func iterator() -> _PrefixSequence<Base> {
    return self
  }

  internal func next() -> Base.Element? {
    if _taken >= _maxLength { return nil }
    _taken += 1

    if let next = _iterator.next() {
      return next
    }

    _taken = _maxLength
    return nil
  }

  internal func prefix(maxLength: Int) -> AnySequence<Base.Element> {
    return AnySequence(
      _PrefixSequence(_iterator,
        maxLength: Swift.min(maxLength, self._maxLength), taken: _taken))
  }
}

//===----------------------------------------------------------------------===//
// Default implementations for Sequence
//===----------------------------------------------------------------------===//

extension Sequence {
  /// Returns an `Array` containing the results of mapping `transform`
  /// over `self`.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  public func map<T>(
    @noescape transform: (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    let initialCapacity = underestimatedCount
    var result = ContiguousArray<T>()
    result.reserveCapacity(initialCapacity)

    var iterator = self.iterator()

    // Add elements up to the initial capacity without checking for regrowth.
    for _ in 0..<initialCapacity {
      result.append(try transform(iterator.next()!))
    }
    // Add remaining elements, if any.
    while let element = iterator.next() {
      result.append(try transform(element))
    }
    return Array(result)
  }

  /// Returns an `Array` containing the elements of `self`,
  /// in order, that satisfy the predicate `includeElement`.
  @warn_unused_result
  public func filter(
    @noescape includeElement: (Iterator.Element) throws -> Bool
  ) rethrows -> [Iterator.Element] {

    var result = ContiguousArray<Iterator.Element>()

    var iterator = self.iterator()

    while let element = iterator.next() {
      if try includeElement(element) {
        result.append(element)
      }
    }

    return Array(result)
  }

  @warn_unused_result
  public func suffix(maxLength: Int) -> AnySequence<Iterator.Element> {
    _precondition(maxLength >= 0, "Can't take a suffix of negative length from a sequence")
    if maxLength == 0 { return AnySequence([]) }
    // FIXME: <rdar://problem/21885650> Create reusable RingBuffer<T>
    // Put incoming elements into a ring buffer to save space. Once all
    // elements are consumed, reorder the ring buffer into an `Array`
    // and return it. This saves memory for sequences particularly longer
    // than `maxLength`.
    var ringBuffer: [Iterator.Element] = []
    ringBuffer.reserveCapacity(Swift.min(maxLength, underestimatedCount))

    var i = ringBuffer.startIndex

    for element in self {
      if ringBuffer.count < maxLength {
        ringBuffer.append(element)
      } else {
        ringBuffer[i] = element
        i = i.successor() % maxLength
      }
    }

    if i != ringBuffer.startIndex {
      return AnySequence(
        [ringBuffer[i..<ringBuffer.endIndex], ringBuffer[0..<i]].flatten())
    }
    return AnySequence(ringBuffer)
  }

  /// Returns the maximal `SubSequence`s of `self`, in order, that
  /// don't contain elements satisfying the predicate `isSeparator`.
  ///
  /// - Parameter maxSplits: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplits + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing *all* the elements of `self` following the
  ///   last split point.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter omittingEmptySubsequences: If `false`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   satisfying `isSeparator`.
  ///   The default value is `true`.
  ///
  /// - Requires: `maxSplits >= 0`
  @warn_unused_result
  public func split(
    maxSplits maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    @noescape isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [AnySequence<Iterator.Element>] {
    _precondition(maxSplits >= 0, "Must take zero or more splits")
    var result: [AnySequence<Iterator.Element>] = []
    var subSequence: [Iterator.Element] = []

    func appendSubsequence() -> Bool {
      if subSequence.isEmpty && omittingEmptySubsequences {
        return false
      }
      result.append(AnySequence(subSequence))
      subSequence = []
      return true
    }

    if maxSplits == 0 {
      // We aren't really splitting the sequence.  Convert `self` into an
      // `Array` using a fast entry point.
      subSequence = Array(self)
      appendSubsequence()
      return result
    }

    var hitEnd = false
    var iterator = self.iterator()
    while true {
      guard let element = iterator.next() else {
        hitEnd = true
        break
      }
      if try isSeparator(element) {
        if !appendSubsequence() {
          continue
        }
        if result.count == maxSplits {
          break
        }
      } else {
        subSequence.append(element)
      }
    }
    if !hitEnd {
      while let element = iterator.next() {
        subSequence.append(element)
      }
    }
    appendSubsequence()
    return result
  }

  /// Returns a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - Complexity: O(N).
  public var underestimatedCount: Int {
    return 0
  }

  public func _preprocessingPass<R>(@noescape preprocess: () -> R) -> R? {
    return nil
  }

  @warn_unused_result
  public func _customContainsEquatableElement(
    element: Iterator.Element
  ) -> Bool? {
    return nil
  }

  /// Call `body` on each element in `self` in the same order as a
  /// *for-in loop.*
  ///
  ///     sequence.forEach {
  ///       // body code
  ///     }
  ///
  /// is similar to:
  ///
  ///     for element in sequence {
  ///       // body code
  ///     }
  ///
  /// - Note: You cannot use the `break` or `continue` statement to exit the
  ///   current call of the `body` closure or skip subsequent calls.
  /// - Note: Using the `return` statement in the `body` closure will only
  ///   exit from the current call to `body`, not any outer scope, and won't
  ///   skip subsequent calls.
  ///
  /// - Complexity: O(`self.count`)
  public func forEach(
    @noescape body: (Iterator.Element) throws -> Void
  ) rethrows {
    for element in self {
      try body(element)
    }
  }
}

extension Sequence where Iterator.Element : Equatable {
  /// Returns the maximal `SubSequence`s of `self`, in order, around a
  /// `separator` element.
  ///
  /// - Parameter maxSplits: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplits + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing *all* the elements of `self` following the
  ///   last split point.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter omittingEmptySubsequences: If `false`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   equal to `separator`.
  ///   The default value is `true`.
  ///
  /// - Requires: `maxSplits >= 0`
  @warn_unused_result
  public func split(
    separator: Iterator.Element,
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true
  ) -> [AnySequence<Iterator.Element>] {
    return split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      isSeparator: { $0 == separator })
  }
}

extension Sequence where
  SubSequence : Sequence,
  SubSequence.Iterator.Element == Iterator.Element,
  SubSequence.SubSequence == SubSequence {

  /// Returns a subsequence containing all but the first `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropFirst(n: Int) -> AnySequence<Iterator.Element> {
    precondition(n >= 0, "Can't drop a negative number of elements from a sequence")
    if n == 0 { return AnySequence(self) }
    return AnySequence(_DropFirstSequence(iterator(), limit: n))
  }

  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `self` is a finite collection.
  /// - Requires: `n >= 0`
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public func dropLast(n: Int) -> AnySequence<Iterator.Element> {
    precondition(n >= 0, "Can't drop a negative number of elements from a sequence")
    if n == 0 { return AnySequence(self) }

    // FIXME: <rdar://problem/21885650> Create reusable RingBuffer<T>
    // Put incoming elements from this sequence in a holding tank, a ring buffer
    // of size <= n. If more elements keep coming in, pull them out of the
    // holding tank into the result, an `Array`. This saves
    // `n` * sizeof(Iterator.Element) of memory, because slices keep the entire
    // memory of an `Array` alive.
    var result: [Iterator.Element] = []
    var ringBuffer: [Iterator.Element] = []
    var i = ringBuffer.startIndex

    for element in self {
      if ringBuffer.count < n {
        ringBuffer.append(element)
      } else {
        result.append(ringBuffer[i])
        ringBuffer[i] = element
        i = i.successor() % n
      }
    }
    return AnySequence(result)
  }

  @warn_unused_result
  public func prefix(maxLength: Int) -> AnySequence<Iterator.Element> {
    precondition(maxLength >= 0, "Can't take a prefix of negative length from a sequence")
    if maxLength == 0 {
      return AnySequence(EmptyCollection<Iterator.Element>())
    }
    return AnySequence(_PrefixSequence(iterator(), maxLength: maxLength))
  }
}

extension Sequence {
  /// Returns a subsequence containing all but the first element.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func dropFirst() -> SubSequence { return dropFirst(1) }

  /// Returns a subsequence containing all but the last element.
  ///
  /// - Requires: `self` is a finite sequence.
  /// - Requires: `n >= 0`
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public func dropLast() -> SubSequence  { return dropLast(1) }
}

extension Sequence {
  public func _initializeTo(ptr: UnsafeMutablePointer<Iterator.Element>)
    -> UnsafeMutablePointer<Iterator.Element> {
    var p = UnsafeMutablePointer<Iterator.Element>(ptr)
    for x in IteratorSequence(self.iterator()) {
      p.initializePointee(x)
      p += 1
    }
    return p
  }
}

// Pending <rdar://problem/14011860> and <rdar://problem/14396120>,
// pass a IteratorProtocol through IteratorSequence to give it "Sequence-ness"
/// A sequence built around an iterator of type `Base`.
///
/// Useful mostly to recover the ability to use `for`...`in`,
/// given just an iterator `i`:
///
///     for x in IteratorSequence(i) { ... }
public struct IteratorSequence<
  Base : IteratorProtocol
> : IteratorProtocol, Sequence {
  /// Construct an instance whose iterator is a copy of `base`.
  public init(_ base: Base) {
    _base = base
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Base.Element? {
    return _base.next()
  }

  internal var _base: Base
}

@available(*, unavailable, renamed="IteratorProtocol")
public typealias GeneratorType = IteratorProtocol

@available(*, unavailable, renamed="Sequence")
public typealias SequenceType = Sequence

extension Sequence {
  @available(*, unavailable, renamed="iterator()")
  func generate() -> Iterator {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="it became a property 'underestimatedCount'")
  func underestimateCount() -> Int {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="call 'split(_:omittingEmptySubsequences:isSeparator:)' and invert the 'allowEmptySlices' argument")
  func split(maxSplit: Int, allowEmptySlices: Bool,
    @noescape isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    fatalError("unavailable function can't be called")
  }
}

extension Sequence where Iterator.Element : Equatable {
  @available(*, unavailable, message="call 'split(_:omittingEmptySubsequences:isSeparator:)' and invert the 'allowEmptySlices' argument")
  public func split(
    separator: Iterator.Element,
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false
  ) -> [AnySequence<Iterator.Element>] {
    fatalError("unavailable function can't be called")
  }
}

@available(*, unavailable, renamed="IteratorSequence")
public struct GeneratorSequence<Base : IteratorProtocol> {}

