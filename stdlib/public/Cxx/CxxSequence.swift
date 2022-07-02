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
  associatedtype Element = RawIterator.Pointee

  // `begin()` and `end()` have to be mutating, otherwise calling 
  // `self.sequence.begin()` will copy `self.sequence` into a temporary value,
  // and the result will be dangling. This does not mean that the implementing
  // methods _have_ to be mutating.

  /// Do not implement this function manually in Swift.
  mutating func begin() -> RawIterator

  /// Do not implement this function manually in Swift.
  mutating func end() -> RawIterator
}

public class CxxIterator<T>: IteratorProtocol where T: CxxSequence {
  // Declared as a class instead of a struct to avoid copies of this object,
  // which would result in dangling pointers for some C++ sequence types.

  public typealias Element = T.RawIterator.Pointee
  private var sequence: T
  private var rawIterator: T.RawIterator
  private let endIterator: T.RawIterator

  public init(sequence: T) {
    self.sequence = sequence
    self.rawIterator = self.sequence.begin()
    self.endIterator = self.sequence.end()
  }

  public func next() -> Element? {
    if self.rawIterator == self.endIterator {
      return nil
    }
    let object = self.rawIterator.pointee
    self.rawIterator = self.rawIterator.successor()
    return object
  }
}

extension CxxSequence {
  public func makeIterator() -> CxxIterator<Self> {
    return CxxIterator(sequence: self)
  }
}
