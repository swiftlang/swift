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
  var r = y
  if y < x {
    r = x
  }
  return r
}

/// Returns the greatest argument passed.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var r = y
  if y < x {
    r = x
  }
  if r < z {
    r = z
  }
  for t in rest {
    if t >= r {
      r = t
    }
  }
  return r
}

/// The `GeneratorType` for `EnumerateSequence`.  `EnumerateGenerator`
/// wraps a `Base` `GeneratorType` and yields successive `Int` values,
/// starting at zero, along with the elements of the underlying
/// `Base`:
///
///     var g = EnumerateGenerator(["foo", "bar"].generate())
///     g.next() // (0, "foo")
///     g.next() // (1, "bar")
///     g.next() // nil
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateGenerator` directly.
public struct EnumerateGenerator<
  Base : GeneratorType
> : GeneratorType, SequenceType {
  /// The type of element returned by `next()`.
  public typealias Element = (index: Int, element: Base.Element)
  var base: Base
  var count: Int

  /// Construct from a `Base` generator.
  public init(_ base: Base) {
    self.base = base
    count = 0
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    guard let b = base.next() else { return .None }
    return .Some((index: count++, element: b))
  }
}

/// The `SequenceType` returned by `enumerate()`.  `EnumerateSequence`
/// is a sequence of pairs (*n*, *x*), where *n*s are consecutive
/// `Int`s starting at zero, and *x*s are the elements of a `Base`
/// `SequenceType`:
///
///     var s = EnumerateSequence(["foo", "bar"])
///     Array(s) // [(0, "foo"), (1, "bar")]
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateSequence` directly.
public struct EnumerateSequence<Base : SequenceType> : SequenceType {
  var base: Base

  /// Construct from a `Base` sequence.
  public init(_ base: Base) {
    self.base = base
  }

  /// Returns a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> EnumerateGenerator<Base.Generator> {
    return EnumerateGenerator(base.generate())
  }
}

