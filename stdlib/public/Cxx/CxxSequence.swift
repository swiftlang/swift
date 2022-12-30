//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Bridged C++ iterator that allows to traverse the elements of a sequence 
/// using a for-in loop.
///
/// Mostly useful for conforming a type to the `CxxSequence` protocol and should
/// not generally be used directly.
///
/// - SeeAlso: https://en.cppreference.com/w/cpp/named_req/InputIterator
public protocol UnsafeCxxInputIterator: Equatable {
  associatedtype Pointee

  /// Returns the unwrapped result of C++ `operator*()`.
  ///
  /// Generally, Swift creates this property automatically for C++ types that
  /// define `operator*()`.
  var pointee: Pointee { get }

  /// Returns an iterator pointing to the next item in the sequence.
  ///
  /// Generally, Swift creates this property automatically for C++ types that
  /// define pre-increment `operator++()`.
  func successor() -> Self
}

extension UnsafePointer: UnsafeCxxInputIterator {}

extension UnsafeMutablePointer: UnsafeCxxInputIterator {}

extension Optional: UnsafeCxxInputIterator where Wrapped: UnsafeCxxInputIterator {
  public typealias Pointee = Wrapped.Pointee

  public var pointee: Pointee {
    if let value = self {
      return value.pointee
    }
    fatalError("Could not dereference nullptr")
  }

  public func successor() -> Self {
    if let value = self {
      return value.successor()
    }
    fatalError("Could not increment nullptr")
  }
}

/// Use this protocol to conform custom C++ sequence types to Swift's `Sequence`
/// protocol like this:
///
///     extension MyCxxSequenceType : CxxSequence {}
///
/// This requires the C++ sequence type to define const methods `begin()` and
/// `end()` which return input iterators into the C++ sequence. The iterator
/// types must conform to `UnsafeCxxInputIterator`.
public protocol CxxSequence: Sequence {
  associatedtype RawIterator: UnsafeCxxInputIterator
  override associatedtype Element = RawIterator.Pointee
  override associatedtype Iterator = CxxIterator<Self>

  // `begin()` and `end()` have to be mutating, otherwise calling 
  // `self.sequence.begin()` will copy `self.sequence` into a temporary value,
  // and the result will be dangling. This does not mean that the implementing
  // methods _have_ to be mutating.

  /// Do not implement this function manually in Swift.
  mutating func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  mutating func __endUnsafe() -> RawIterator
}

/// Prevents a C++ sequence from being copied or moved implicitly. Makes sure
/// that raw C++ iterators pointing into the sequence are not invalidated.
@usableFromInline
internal final class CxxSequenceBox<T> where T: CxxSequence {
  @usableFromInline
  internal var sequence: T

  @usableFromInline
  internal init(_ sequence: __shared T) {
    self.sequence = sequence
  }
}

@frozen
public struct CxxIterator<T>: IteratorProtocol where T: CxxSequence {
  public typealias Element = T.RawIterator.Pointee

  @usableFromInline
  internal let sequence: CxxSequenceBox<T>
  @usableFromInline
  internal var rawIterator: T.RawIterator
  @usableFromInline
  internal let endIterator: T.RawIterator

  @inlinable
  public init(sequence: __shared T) {
    self.sequence = CxxSequenceBox<T>(sequence)
    self.rawIterator = self.sequence.sequence.__beginUnsafe()
    self.endIterator = self.sequence.sequence.__endUnsafe()
  }

  @inlinable
  public mutating func next() -> Element? {
    if self.rawIterator == self.endIterator {
      return nil
    }
    let object = self.rawIterator.pointee
    self.rawIterator = self.rawIterator.successor()
    return object
  }
}

extension CxxSequence {
  @inlinable
  public func makeIterator() -> CxxIterator<Self> {
    return CxxIterator(sequence: self)
  }
}
