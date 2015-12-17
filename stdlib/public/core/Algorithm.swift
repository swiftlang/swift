//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Returns the lesser of `x` and `y`.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T) -> T {
  var r = x
  if y < x {
    r = y
  }
  return r
}

/// Returns the least argument passed.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var r = x
  if y < x {
    r = y
  }
  if z < r {
    r = z
  }
  for t in rest {
    if t < r {
      r = t
    }
  }
  return r
}

/// Returns the greater of `x` and `y`.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T) -> T {
  return y >= x ? y : x
}

/// Returns the greatest argument passed.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var r = max(max(x, y), z)
  for t in rest where t >= r {
    r = t
  }
  return r
}

/// The iterator for `EnumeratedSequence`.  `EnumeratedIterator`
/// wraps a `Base` iterator and yields successive `Int` values,
/// starting at zero, along with the elements of the underlying
/// `Base`:
///
///     var iterator = ["foo", "bar"].enumerated.iterator()
///     iterator.next() // (0, "foo")
///     iterator.next() // (1, "bar")
///     iterator.next() // nil
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateGenerator` directly.
public struct EnumeratedIterator<
  Base : IteratorProtocol
> : IteratorProtocol, Sequence {
  internal var _base: Base
  internal var _count: Int

  /// Construct from a `Base` iterator.
  internal init(_base: Base) {
    self._base = _base
    self._count = 0
  }

  /// The type of element returned by `next()`.
  public typealias Element = (offset: Int, element: Base.Element)

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    guard let b = _base.next() else { return nil }
    return (offset: _count++, element: b)
  }
}

/// The type of the `enumerated` property.
///
/// `EnumeratedSequence` is a sequence of pairs (*n*, *x*), where *n*s
/// are consecutive `Int`s starting at zero, and *x*s are the elements
/// of a `Base` `Sequence`:
///
///     var s = ["foo", "bar"].enumerated
///     Array(s) // [(0, "foo"), (1, "bar")]
public struct EnumeratedSequence<Base : Sequence> : Sequence {
  internal var _base: Base

  /// Construct from a `Base` sequence.
  internal init(_base: Base) {
    self._base = _base
  }

  /// Returns an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> EnumeratedIterator<Base.Iterator> {
    return EnumeratedIterator(_base: _base.iterator())
  }
}

