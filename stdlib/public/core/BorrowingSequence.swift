//===----------------------------------------------------------------------===//
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

/// A type that provides borrowed access to the values of a borrowing sequence.
@available(SwiftStdlib 6.3, *)
public protocol BorrowingIteratorProtocol<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable

  /// Advance the iterator, returning an ephemeral span over the elements
  /// that are ready to be visited.
  ///
  /// If the underlying iterable is a container type, then the returned span
  /// typically directly addresses one of its storage buffers. On the other
  /// hand, if the underlying iterable materializes its elements on demand,
  /// then the returned span addresses some temporary buffer associated with
  /// the iterator itself. Consequently, the returned span is tied to this
  /// particular invocation of `nextSpan`, and it cannot survive until the next
  /// invocation of it.
  ///
  /// If the iterator has not yet reached the end of the underlying iterable,
  /// then this method returns a non-empty span of at most `maximumCount`
  /// elements, and updates the iterator's current position to the element
  /// following the last item in the returned span (or the end, if there is
  /// none). The `maximumCount` argument allows callers to avoid getting more
  /// items that they are able to process in one go, simplifying usage, and
  /// avoiding materializing more elements than needed.
  ///
  /// If the iterator's current position is at the end of the container, then
  /// this method returns an empty span without updating the position.
  ///
  /// This method can be used to efficiently process the items of a container
  /// in bulk, by directly iterating over its piecewise contiguous pieces of
  /// storage:
  ///
  ///     var it = items.startBorrowIteration()
  ///     while true {
  ///       let span = it.nextSpan(after: &index)
  ///       if span.isEmpty { break }
  ///       // Process items in `span`
  ///     }
  ///
  /// Note: The spans returned by this method are not guaranteed to be disjunct.
  /// Iterators that materialize elements on demand typically reuse the same
  /// buffer over and over again; and even some proper containers may link to a
  /// single storage chunk (or parts of a storage chunk) multiple times, for
  /// example to repeat their contents.
  ///
  /// Note: Repeatedly iterating over the same container is expected to return
  /// the same items (collected in similarly sized span instances), but the
  /// returned spans are not guaranteed to be identical. For example, this is
  /// the case with containers that store some of their contents within their
  /// direct representation. Such containers may not always have a unique
  /// address in memory, and so the locations of the spans exposed by this
  /// method may vary between different borrows of the same container.)
  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maximumCount: Int) -> Span<Element>

  /// Advance the position of this iterator by the specified offset, or until
  /// the end of the underlying iterable.
  ///
  /// Returns the number of items that were skipped. If this is less than
  /// `maximumOffset`, then the iterable did not have enough elements left to
  /// skip the requested number of items. In this case, the iterator's current
  /// position is set to the end of the iterable.
  ///
  /// `maximumOffset` must be nonnegative, unless this is a bidirectional
  /// or random-access iterator.
  @_lifetime(self: copy self)
  mutating func skip(by maximumOffset: Int) -> Int
}

@available(SwiftStdlib 6.3, *)
extension BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @_transparent
  public mutating func nextSpan() -> Span<Element> {
    nextSpan(maximumCount: Int.max)
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func skip(by offset: Int) -> Int {
    var remainder = offset
    while remainder > 0 {
      let span = nextSpan(maximumCount: remainder)
      if span.isEmpty { break }
      remainder &-= span.count
    }
    return offset &- remainder
  }
}

/// A type that provides sequential, borrowing access to its elements.
@available(SwiftStdlib 6.3, *)
@reparentable
public protocol BorrowingSequence<Element>: ~Copyable, ~Escapable {
  /// A type representing the sequence's elements.
  associatedtype Element: ~Copyable

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype BorrowingIterator: BorrowingIteratorProtocol<Element> & ~Copyable & ~Escapable

  /// Returns a borrowing iterator over the elements of this sequence.
  @lifetime(borrow self)
  func makeBorrowingIterator() -> BorrowingIterator
}

@available(SwiftStdlib 6.3, *)
extension BorrowingSequence where Self: BorrowingIteratorProtocol & ~Escapable,
  BorrowingIterator == Self
{
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> BorrowingIterator {
    self
  }
}

@available(SwiftStdlib 6.3, *)
@frozen
public struct BorrowingIteratorAdapter<Iterator: IteratorProtocol>: BorrowingIteratorProtocol {
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
    return curValue._span
  }
}

@available(SwiftStdlib 6.3, *)
extension Sequence {
  @_transparent
  public func makeBorrowingIterator() -> BorrowingIteratorAdapter<Iterator> {
    BorrowingIteratorAdapter(iterator: makeIterator())
  }
}

// MARK: Conformances

@available(SwiftStdlib 6.3, *)
extension Span: BorrowingSequence, BorrowingIteratorProtocol where Element: ~Copyable {
  public typealias Element = Element

  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> Self {
    self
  }
  
  @_lifetime(&self)
  @_lifetime(self: copy self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    let result = extracting(first: maximumCount)
    self = extracting(droppingFirst: maximumCount)
    return result
  }
}

@available(SwiftStdlib 6.3, *)
extension MutableSpan: BorrowingSequence where Element: ~Copyable {
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    self.span
  }
}

@available(SwiftStdlib 6.3, *)
extension RawSpan: BorrowingSequence {
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<UInt8> {
    self._span
  }
}

@available(SwiftStdlib 6.3, *)
extension MutableRawSpan: BorrowingSequence {
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<UInt8> {
    let bytes = self.bytes
    return unsafe _overrideLifetime(bytes._span, borrowing: self)
  }
}

@available(SwiftStdlib 6.3, *)
extension Array: BorrowingSequence {
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    self.span
  }
}

@available(SwiftStdlib 6.3, *)
extension InlineArray: BorrowingSequence where Element: ~Copyable {
  public typealias BorrowingIterator = Span<Element>
  
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    self.span
  }
}

@available(SwiftStdlib 6.3, *)
extension UnsafeBufferPointer: @unsafe BorrowingSequence where Element: ~Copyable {
  public typealias BorrowingIterator = Span<Element>
  
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    unsafe self.span
  }
}

@available(SwiftStdlib 6.3, *)
extension UnsafeMutableBufferPointer: @unsafe BorrowingSequence where Element: ~Copyable {
  public typealias BorrowingIterator = Span<Element>
  
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    unsafe self.span
  }
}
