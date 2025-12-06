//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.3, *)
public protocol BorrowingIteratorProtocol<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable

  // FIXME: This ought to be a core requirement, but `Ref` is not a thing yet.
//  @_lifetime(&self)
//  @_lifetime(self: copy self)
//  mutating func next() -> Ref<Element>?

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

  // FIXME: Add BidirectionalBorrowIteratorProtocol and RandomAccessBorrowIteratorProtocol.
  // BidirectionalBorrowIteratorProtocol would need to have a `previousSpan`
  // method, which considerably complicates implementation.
  // Perhaps these would be better left to as variants of protocol Container,
  // which do not need a separate iterator concept.
}

@available(SwiftStdlib 6.3, *)
extension BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable {
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @inlinable
  public mutating func nextSpan() -> Span<Element> {
    nextSpan(maximumCount: Int.max)
  }
}

@available(SwiftStdlib 6.3, *)
extension BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable {
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

//@available(SwiftStdlib 6.3, *)
//extension Span: Iterable where Element: ~Copyable {
//  // FIXME: This simple definition cannot also be a backward (or bidirectional)
//  // iterator, nor a random-access iterator. If we want to go in that direction,
//  // we'll need to rather introduce a type more like `RigidArray.BorrowIterator`.
//  public typealias BorrowIterator = Self
//
//  @_alwaysEmitIntoClient
//  @_transparent
//  public var estimatedCount: EstimatedCount {
//    .exactly(count)
//  }
//
//  @_alwaysEmitIntoClient
//  @_lifetime(copy self)
//  @_transparent
//  public func startBorrowIteration() -> Span<Element> {
//    self
//  }
//}

@available(SwiftStdlib 6.3, *)
extension Span: BorrowingIteratorProtocol where Element: ~Copyable {
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @inlinable
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    let result = extracting(first: maximumCount)
    self = extracting(droppingFirst: maximumCount)
    return result
  }
}

@available(SwiftStdlib 6.3, *)
extension Span: Sequence /* where Element: ~Copyable */ {
  public typealias Iterator = NeverIterator<Element>
  
  @_alwaysEmitIntoClient
  @_lifetime(borrow self)
  @inlinable
  public func makeBorrowingIterator() -> Span<Element> {
    self
  }
}

extension InlineArray: Sequence /* where Element: ~Copyable */ {
  public typealias Element = Element
//  public typealias Iterator = NeverIterator<Element>
  public typealias BorrowingIterator = Span<Element>
  
  // This will NOT work once `Element: ~Copyable` is in the works
  // Problematic because we can only conform to `Sequence` once, so we
  // need to provide a single iterator type, but it can't handle both
  // ~Copyable and copyable elements (I think)
  public struct Iterator: IteratorProtocol {
    var array: [count of Element]
    var index: Int = 0
    
    public mutating func next() -> Element? {
      guard index < array.count else { return nil }
      defer { index &+= 1 }
      return array[index]
    }
  }
  
  public func makeIterator() -> Iterator {
    .init(array: self)
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element> {
    self.span
  }
}

//@available(SwiftStdlib 6.3, *)
//extension MutableSpan: Iterable where Element: ~Copyable {
//  public typealias BorrowIterator = Span<Element>.BorrowIterator
//
//  @_alwaysEmitIntoClient
//  @_transparent
//  public var estimatedCount: EstimatedCount {
//    .exactly(count)
//  }
//
//  @_alwaysEmitIntoClient
//  @_lifetime(borrow self)
//  @_transparent
//  public func startBorrowIteration() -> Span<Element> {
//    span
//  }
//}
//
//@available(SwiftStdlib 6.3, *)
//extension OutputSpan: Iterable where Element: ~Copyable {
//  public typealias BorrowIterator = Span<Element>.BorrowIterator
//
//  @_alwaysEmitIntoClient
//  @_transparent
//  public var estimatedCount: EstimatedCount {
//    .exactly(count)
//  }
//
//  @_alwaysEmitIntoClient
//  @_lifetime(borrow self)
//  @_transparent
//  public func startBorrowIteration() -> Span<Element> {
//    self.span
//  }
//}
