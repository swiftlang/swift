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
@available(SwiftStdlib 6.3, *)
public protocol _BorrowingIteratorProtocol<_Element>: ~Copyable, ~Escapable {
  associatedtype _Element: ~Copyable

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func _nextSpan(maximumCount: Int) -> Span<_Element>

  @_lifetime(self: copy self)
  mutating func _skip(by maximumOffset: Int) -> Int
}

@available(SwiftStdlib 6.3, *)
extension _BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable, _Element: ~Copyable {
  @_alwaysEmitIntoClient
  @_lifetime(&self)
  @_lifetime(self: copy self)
  @_transparent
  public mutating func _nextSpan() -> Span<_Element> {
    _nextSpan(maximumCount: Int.max)
  }
  
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func _skip(by offset: Int) -> Int {
    var remainder = offset
    while remainder > 0 {
      let span = _nextSpan(maximumCount: remainder)
      if span.isEmpty { break }
      remainder &-= span.count
    }
    return offset &- remainder
  }
}

/// A type that provides sequential, borrowing access to its elements.
@available(SwiftStdlib 6.3, *)
public protocol _BorrowingSequence<_Element>: ~Copyable, ~Escapable {
  /// A type representing the sequence's elements.
  associatedtype _Element: ~Copyable

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype _BorrowingIterator: _BorrowingIteratorProtocol<_Element> & ~Copyable & ~Escapable

  /// Returns a borrowing iterator over the elements of this sequence.
  @lifetime(borrow self)
  func _makeBorrowingIterator() -> _BorrowingIterator
}

@available(SwiftStdlib 6.3, *)
extension _BorrowingSequence where Self: _BorrowingIteratorProtocol & ~Escapable,
  _BorrowingIterator == Self
{
  @lifetime(borrow self)
  public func makeBorrowingIterator() -> _BorrowingIterator {
    self
  }
}

// MARK: Conformances

@available(SwiftStdlib 6.3, *)
extension Span: _BorrowingSequence, _BorrowingIteratorProtocol where Element: ~Copyable {
  public typealias _Element = Element

  @_lifetime(borrow self)
  public func _makeBorrowingIterator() -> Self {
    self
  }
  
  @_lifetime(&self)
  @_lifetime(self: copy self)
  public mutating func _nextSpan(maximumCount: Int) -> Span<Element> {
    let result = extracting(first: maximumCount)
    self = extracting(droppingFirst: maximumCount)
    return result
  }
}

@available(SwiftStdlib 6.3, *)
extension InlineArray: _BorrowingSequence where Element: ~Copyable {
  public typealias _BorrowingIterator = Span<Element>
  
  @lifetime(borrow self)
  public func _makeBorrowingIterator() -> Span<Element> {
    self.span
  }
}
