//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Returns the lesser of `x` and `y`.
///
/// If `x == y`, returns `x`.
@warn_unused_result
public func min<T : Comparable>(_ x: T, _ y: T) -> T {
  // In case `x == y` we pick `x`.
  // This preserves any pre-existing order in case `T` has identity,
  // which is important for e.g. the stability of sorting algorithms.
  // `(min(x, y), max(x, y))` should return `(x, y)` in case `x == y`.
  return y < x ? y : x
}

/// Returns the least argument passed.
///
/// If there are multiple equal least arguments, returns the first one.
@warn_unused_result
public func min<T : Comparable>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var minValue = min(min(x, y), z)
  // In case `value == minValue`, we pick `minValue`. See min(_:_:).
  for value in rest where value < minValue {
    minValue = value
  }
  return minValue
}

/// Returns the greater of `x` and `y`.
///
/// If `x == y`, returns `y`.
@warn_unused_result
public func max<T : Comparable>(_ x: T, _ y: T) -> T {
  // In case `x == y`, we pick `y`. See min(_:_:).
  return y >= x ? y : x
}

/// Returns the greatest argument passed.
///
/// If there are multiple equal greatest arguments, returns the last one.
@warn_unused_result
public func max<T : Comparable>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var maxValue = max(max(x, y), z)
  // In case `value == maxValue`, we pick `value`. See min(_:_:).
  for value in rest where value >= maxValue {
    maxValue = value
  }
  return maxValue
}

/// The iterator for `EnumeratedSequence`.  `EnumeratedIterator`
/// wraps a `Base` iterator and yields successive `Int` values,
/// starting at zero, along with the elements of the underlying
/// `Base`:
///
///     var iterator = ["foo", "bar"].enumerated().makeIterator()
///     iterator.next() // (0, "foo")
///     iterator.next() // (1, "bar")
///     iterator.next() // nil
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateIterator` directly.
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
    defer { _count += 1 }
    return (offset: _count, element: b)
  }
}

/// The type of the `enumerated()` property.
///
/// `EnumeratedSequence` is a sequence of pairs (*n*, *x*), where *n*s
/// are consecutive `Int`s starting at zero, and *x*s are the elements
/// of a `Base` `Sequence`:
///
///     var s = ["foo", "bar"].enumerated()
///     Array(s) // [(0, "foo"), (1, "bar")]
public struct EnumeratedSequence<Base : Sequence> : Sequence {
  internal var _base: Base

  /// Construct from a `Base` sequence.
  internal init(_base: Base) {
    self._base = _base
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> EnumeratedIterator<Base.Iterator> {
    return EnumeratedIterator(_base: _base.makeIterator())
  }
}

@available(*, unavailable, renamed: "EnumeratedIterator")
public struct EnumerateGenerator<Base : IteratorProtocol> { }

@available(*, unavailable, renamed: "EnumeratedSequence")
public struct EnumerateSequence<Base : Sequence> {}

extension EnumeratedIterator {
  @available(*, unavailable, message: "use the 'enumerated()' method on the sequence")
  public init(_ base: Base) {
    Builtin.unreachable()
  }
}

extension EnumeratedSequence {
  @available(*, unavailable, message: "use the 'enumerated()' method on the sequence")
  public init(_ base: Base) {
    Builtin.unreachable()
  }
}

