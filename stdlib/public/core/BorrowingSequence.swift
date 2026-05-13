//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that provides borrowed access to the values of a borrowing sequence.
@available(SwiftStdlib 6.4, *)
public protocol IterableIteratorProtocol<Element, Failure>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable & ~Escapable

  /// A type representing an error that can occur during iteration.
  associatedtype Failure: Error = Never
  
  /// Returns a span over the next group of elements that are ready to by visited,
  /// up to the specifed maximum.
  ///
  /// If the underlying type stores its elements in one or more blocks of
  /// contiguous memory, then the returned span typically directly
  /// addresses one of those buffers. On the other hand, if the underlying type
  /// materializes its elements on demand, then the returned span addresses
  /// a temporary buffer managed by the iterator itself. Consequently, the returned
  /// span is tied to this particular invocation of `nextSpan`, and it cannot
  /// survive beyond the next invocation of this method.
  ///
  /// If the iterator has not yet reached the end of the underlying elements,
  /// then this method returns a non-empty span of at most `maximumCount`
  /// elements and updates the iterator's current position to the element
  /// following the last item in the returned span. The `maximumCount`
  /// argument allows callers to avoid getting more items than they are
  /// able to process in one go, simplifying usage, and
  /// avoiding materializing more elements than needed.
  ///
  /// If the iterator's current position is at the end of the container, then
  /// this method returns an empty span without updating the position.
  ///
  /// This method can be used to efficiently process the items of a container
  /// in bulk, by directly iterating over its piecewise contiguous pieces of
  /// storage:
  ///
  ///     var it = items.makeIterableIterator()
  ///     while true {
  ///       let span = it.nextSpan(maximumCount: .max)
  ///       if span.isEmpty { break }
  ///       // Process items in `span`
  ///     }
  ///
  /// The spans returned by this method are not guaranteed to be disjunct.
  /// Iterators that materialize elements on demand typically reuse the same
  /// buffer over and over again; and even some proper containers may link to a
  /// single storage chunk (or parts of a storage chunk) multiple times, for
  /// example to repeat their contents.
  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maximumCount: Int) throws(Failure) -> Span<Element>

  /// Advances the position of this iterator by the specified offset, or until
  /// the end of the underlying type's elements.
  ///
  /// - Parameter maximumOffset: The maximum number of elements
  ///   to offset the position of this iterator. `maximumOffset` must be
  ///   nonnegative.
  /// - Returns: The number of items that were skipped. If the returned count
  ///   is less than `maximumOffset`, then the underlying type did not have
  ///   enough elements left to skip the requested number of items.
  ///   In that case, the iterator's position is set to the end of the underlying type.
  @_lifetime(self: copy self)
  mutating func skip(by maximumOffset: Int) throws(Failure) -> Int
}

@available(SwiftStdlib 6.4, *)
extension IterableIteratorProtocol where Self: ~Copyable & ~Escapable, Element: ~Copyable & ~Escapable {
  /// Returns a span over the next group of elements that are ready to by visited,
  /// up to the specifed maximum.
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @_transparent
  public mutating func nextSpan() throws(Failure) -> Span<Element> {
    try nextSpan(maximumCount: Int.max)
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func skip(by offset: Int) throws(Failure) -> Int {
    var remainder = offset
    while remainder > 0 {
      let span = try nextSpan(maximumCount: remainder)
      if span.isEmpty { break }
      remainder &-= span.count
    }
    return offset &- remainder
  }
}

@available(SwiftStdlib 6.4, *)
public struct SpanIterator<Element>: IterableIteratorProtocol, ~Copyable, ~Escapable
  where Element: ~Copyable & ~Escapable
{
  public typealias Failure = Never
  
  @usableFromInline
  internal var _span: Span<Element>
  @usableFromInline
  internal var _start: Int
  @usableFromInline
  internal var _count: Int
  
  @_lifetime(copy elements)
  public init(_ elements: Span<Element>) {
    _span = elements
    _start = 0
    _count = elements.count
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @_transparent
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    let c = Swift.min(maximumCount, _count)
    defer {
      _start &+= c
      _count &-= c
    }
    return _span.extracting(droppingFirst: _start).extracting(first: c)
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func skip(by offset: Int) -> Int {
    let c = Swift.min(offset, _count)
    defer {
      _start &+= c
      _count &-= c
    }
    return c
  }
}

/// A type that provides sequential, borrowing access to its elements.
@available(SwiftStdlib 6.4, *)
@reparentable
public protocol Iterable<Element, Failure>: ~Copyable, ~Escapable {
  /// A type representing the sequence's elements.
  associatedtype Element: ~Copyable & ~Escapable

  /// A type representing an error that can occur during iteration.
  associatedtype Failure: Error = Never
  
  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype IterableIterator: IterableIteratorProtocol<Element, Failure> & ~Copyable & ~Escapable

  /// Returns a borrowing iterator over the elements of this sequence.
  @lifetime(borrow self)
  func makeIterableIterator() -> IterableIterator
  
  /// A value less than or equal to the number of elements in the sequence,
  /// calculated nondestructively.
  var underestimatedCount: Int { get }
  
  /// Internal customization point for fast `contains(_:)` checks.
  func _customContainsEquatableElement(_ element: borrowing Element) -> Bool?
}

@available(SwiftStdlib 6.4, *)
extension Iterable where Self: ~Copyable & ~Escapable, Element: ~Copyable & ~Escapable {
  @inlinable
  public var underestimatedCount: Int { 0 }
  @inlinable
  public func _customContainsEquatableElement(_ element: borrowing Element) -> Bool? { nil }
}

// FIXME: Elminate these overloads once Sequence is reparented, they break ambiguity for
// types that conform to both Iterable and Sequence or Collection.

@available(SwiftStdlib 6.4, *)
extension Sequence where Self: Iterable {
  @available(SwiftStdlib 6.4, *)
  @inlinable
  public var underestimatedCount: Int { 0 }
  @available(SwiftStdlib 6.4, *)
  @inlinable
  public func _customContainsEquatableElement(_ element: Element) -> Bool? { nil }
}

@available(SwiftStdlib 6.4, *)
extension Collection where Self: Iterable {
  @available(SwiftStdlib 6.4, *)
  @inlinable
  public var underestimatedCount: Int { 0 }
  @available(SwiftStdlib 6.4, *)
  @inlinable
  public func _customContainsEquatableElement(_ element: Element) -> Bool? { nil }
}

@available(SwiftStdlib 6.4, *)
@frozen
public struct IterableIteratorAdapter<Iterator: IteratorProtocol>: IterableIteratorProtocol {
  @usableFromInline
  var iterator: Iterator
  @usableFromInline
  var curValue: Iterator.Element?

  public typealias Element = Iterator.Element

  @_transparent
  public init(iterator: Iterator) {
    self.iterator = iterator
    curValue = nil
  }

  @_transparent
  @lifetime(&self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Iterator.Element> {
    curValue = iterator.next()
    return curValue._span()
  }
}

@available(SwiftStdlib 6.4, *)
extension Sequence where Self: Iterable {
  @available(SwiftStdlib 6.4, *)
  @_disfavoredOverload
  @_transparent
  public func makeIterableIterator() -> IterableIteratorAdapter<Iterator> {
    IterableIteratorAdapter(iterator: makeIterator())
  }
}
