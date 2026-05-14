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

/// Use this protocol to conform custom C++ sequence types to Swift's `Iterable`
/// protocol like this:
///
///     extension MyCxxSequenceType : CxxIterable {}
///
/// This requires the C++ sequence type to define const methods `begin()` and
/// `end()` which return input iterators into the C++ sequence. The iterator
/// types must conform to `UnsafeCxxInputIterator`.

@available(SwiftStdlib 6.4, *)
public protocol CxxIterable<Element> : Iterable, ~Copyable, ~Escapable {
  override associatedtype Element: ~Copyable
  override associatedtype IterableIterator: IterableIteratorProtocol<Element, Never> & ~Copyable & ~Escapable = CxxIterableIterator<Self>
  associatedtype RawIterator: UnsafeCxxInputIterator
    where RawIterator.Pointee == Element,
          RawIterator.DereferenceResult: _Pointer,
          RawIterator.DereferenceResult.Pointee == Element

  /// Do not implement this function manually in Swift.
  func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  func __endUnsafe() -> RawIterator
}

@frozen
@available(SwiftStdlib 6.4, *)
public struct CxxIterableIterator<T>: IterableIteratorProtocol<T.Element, Never>, ~Escapable, ~Copyable where T: CxxIterable & ~Copyable & ~Escapable, T.Element: ~Copyable {

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
}

// For non-contiguous iterators, we create a span of size one for each element
@available(SwiftStdlib 6.4, *)
extension CxxIterableIterator where T: ~Copyable & ~Escapable, T.Element: ~Copyable {
  @inlinable
  @_lifetime(&self)
  @_lifetime(self: copy self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if self.current == self.end {
      return Span()
    }

    let rawPtr = self.current.__operatorStar()._rawValue
    self.current = self.current.successor()
    return unsafe _cxxOverrideLifetime(
        Span(_unsafeStart: UnsafePointer(rawPtr), count: 1),
        borrowing: self)
  }
}

// For contiguous iterators, we can make iteration more efficient by creating a span with multiple, contiguous, elements
@available(SwiftStdlib 6.4, *)
extension CxxIterableIterator where T: ~Copyable & ~Escapable, T.RawIterator: UnsafeCxxContiguousIterator, T.Element: ~Copyable {
  @inlinable
  @_lifetime(&self)
  @_lifetime(self: copy self)
  public mutating func nextSpan() -> Span<Element> {
    nextSpan(maximumCount: Int.max)
  }

  public var count: Int {
    return Int(self.end - self.current)
  }

  @inlinable
  @_lifetime(&self)
  @_lifetime(self: copy self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if maximumCount < 1 || self.current == self.end {
      return Span()
    }

    let rawPtr = self.current.__operatorStar()._rawValue
    let distance = min(count, maximumCount)
    self.current += T.RawIterator.Distance(distance)
    return unsafe _cxxOverrideLifetime(
        Span(_unsafeStart: UnsafePointer(rawPtr), count: distance),
        borrowing: self)
  }
}

@available(SwiftStdlib 6.4, *)
extension CxxIterable where Element: ~Copyable, Self: ~Copyable {
  @inlinable
  @_lifetime(borrow self)
  public borrowing func makeIterableIterator() -> CxxIterableIterator<Self> {
    let iterator = CxxIterableIterator<Self>(begin: __beginUnsafe(), end: __endUnsafe(), sequence: self)
    return iterator
  }
}
