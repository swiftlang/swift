//===--- LazySequence.swift -----------------------------------------------===//
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

/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
public struct LazySequence<Base : SequenceType> : SequenceType {
  @available(*, unavailable, renamed="Base")
  public typealias S = Base

  /// Construct an instance with `base` as its underlying sequence
  /// instance.
  public init(_ base: Base) {
    self._base = base
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Base.Generator {
    return self._base.generate()
  }

  /// an Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  public var array: [Base.Generator.Element] {
    return Array(_base)
  }

  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  var _base: Base
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public func lazy<Base : SequenceType>(s: Base) -> LazySequence<Base> {
  return LazySequence(s)
}
