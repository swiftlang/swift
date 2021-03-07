//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An enumeration of the elements of a sequence or collection.
///
/// `EnumeratedSequence` is a sequence of pairs (*n*, *x*), where *n*s are
/// consecutive `Int` values starting at zero, and *x*s are the elements of a
/// base sequence.
///
/// To create an instance of `EnumeratedSequence`, call `enumerated()` on a
/// sequence or collection. The following example enumerates the elements of
/// an array.
///
///     var s = ["foo", "bar"].enumerated()
///     for (n, x) in s {
///         print("\(n): \(x)")
///     }
///     // Prints "0: foo"
///     // Prints "1: bar"
@frozen
public struct EnumeratedSequence<Base: Sequence> {
  @usableFromInline
  internal var _base: Base

  /// Construct from a `Base` sequence.
  @inlinable
  internal init(_base: Base) {
    self._base = _base
  }
}

extension EnumeratedSequence {
  /// The iterator for `EnumeratedSequence`.
  ///
  /// An instance of this iterator wraps a base iterator and yields
  /// successive `Int` values, starting at zero, along with the elements of the
  /// underlying base iterator. The following example enumerates the elements of
  /// an array:
  ///
  ///     var iterator = ["foo", "bar"].enumerated().makeIterator()
  ///     iterator.next() // (0, "foo")
  ///     iterator.next() // (1, "bar")
  ///     iterator.next() // nil
  ///
  /// To create an instance, call
  /// `enumerated().makeIterator()` on a sequence or collection.
  @frozen
  public struct Iterator {
    @usableFromInline
    internal var _base: Base.Iterator
    @usableFromInline
    internal var _count: Int

    /// Construct from a `Base` iterator.
    @inlinable
    internal init(_base: Base.Iterator) {
      self._base = _base
      self._count = 0
    }
  }
}

extension EnumeratedSequence.Iterator: IteratorProtocol, Sequence {
  /// The type of element returned by `next()`.
  public typealias Element = (offset: Int, element: Base.Element)

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable
  public mutating func next() -> Element? {
    guard let b = _base.next() else { return nil }
    let result = (offset: _count, element: b)
    _count += 1 
    return result
  }
}

extension EnumeratedSequence: Sequence {
  /// Returns an iterator over the elements of this sequence.
  @inlinable
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator())
  }
}
