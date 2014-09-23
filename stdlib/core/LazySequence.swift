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

/// Augments `S` with lazy versions of various sequence algorithms.
public
struct LazySequence<S: SequenceType> : SequenceType {
  public
  init(_ base: S) {
    self._base = base
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> S.Generator {
    return self._base.generate()
  }

  /// an Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  public var array: [S.Generator.Element] {
    return Array(_base)
  }

  var _base: S
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public
func lazy<S: SequenceType>(s: S) -> LazySequence<S> {
  return LazySequence(s)
}
