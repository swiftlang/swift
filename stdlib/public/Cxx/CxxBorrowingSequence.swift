//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Use this protocol to conform custom C++ sequence types to Swift's `BorrowingSequence`
/// protocol like this:
///
///     extension MyCxxSequenceType : CxxBorrowingSequence {}
///
/// This requires the C++ sequence type to define const methods `begin()` and
/// `end()` which return input iterators into the C++ sequence. The iterator
/// types must conform to `UnsafeCxxInputIterator`.

@available(SwiftStdlib 6.3, *)
public protocol CxxBorrowingSequence<Element> : BorrowingSequence, ~Copyable, ~Escapable {
  override associatedtype Element: ~Copyable
  override associatedtype BorrowingIterator: BorrowingIteratorProtocol<Element> & ~Copyable & ~Escapable = CxxBorrowingIterator<Self>
  associatedtype RawIterator: UnsafeCxxInputIterator
    where RawIterator.Pointee == Element, RawIterator.DereferenceResult == UnsafePointer<Element>

  /// Do not implement this function manually in Swift.
  func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  func __endUnsafe() -> RawIterator
}

@frozen
@available(SwiftStdlib 6.3, *)
public struct CxxBorrowingIterator<T>: BorrowingIteratorProtocol, ~Escapable, ~Copyable where T: CxxBorrowingSequence & ~Copyable & ~Escapable, T.Element: ~Copyable {
  public typealias Element = T.RawIterator.Pointee

  @usableFromInline
  internal var current: T.RawIterator
  @usableFromInline
  internal let end: T.RawIterator

  @inlinable
  @_lifetime(borrow sequence)
  public init(begin: T.RawIterator, end: T.RawIterator, sequence: borrowing T) {
    self.current = begin
    self.end = end
  }

  @inlinable
  @_lifetime(&self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if self.current == self.end {
      return Span()
    }

    let ptr = unsafe self.current.__operatorStar()
    self.current = self.current.successor()
    return unsafe _cxxOverrideLifetime(
        Span(_unsafeStart: ptr, count: 1),
        borrowing: self)
  }
}

@available(SwiftStdlib 6.3, *)
extension CxxBorrowingSequence where Element: ~Copyable, Self: ~Copyable {
  @inlinable
  @_lifetime(borrow self)
  public borrowing func makeBorrowingIterator() -> CxxBorrowingIterator<Self> {
    let iterator = CxxBorrowingIterator<Self>(begin: __beginUnsafe(), end: __endUnsafe(), sequence: self)
    return unsafe _cxxOverrideLifetime(iterator, borrowing: self)
  }
}
