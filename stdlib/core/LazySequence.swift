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
struct LazySequence<S: Sequence> : Sequence {
  @public
  init(_ base: S) {
    self._base = base
  }

  @public
  func generate() -> S.GeneratorType {
    return self._base.generate()
  }

  /// an Array, created on-demand, containing the elements of this
  /// lazy Sequence.
  @public var array: [S.GeneratorType.Element] {
    return Array(_base)
  }

  var _base: S
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<S: Sequence>(s: S) -> LazySequence<S> {
  return LazySequence(s)
}
