//===--- LazyCollection.swift ---------------------------------------------===//
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

/// Augments `C` with lazy versions of various collection algorithms.
struct LazyCollection<C: Collection> : Collection {
  @public
  init(_ base: C) {
    self._base = base
  }

  @public
  func generate() -> C.GeneratorType {
    return self._base.generate()
  }

  @public
  var startIndex: C.IndexType {
    return _base.startIndex
  }
  
  @public
  var endIndex: C.IndexType {
    return _base.endIndex
  }

  @public
  subscript(i: C.IndexType) -> C.GeneratorType.Element {
    return _base[i]
  }
  
  /// an Array, created on-demand, containing the elements of this
  /// lazy Collection.
  @public var array: [C.GeneratorType.Element] {
    return Array(_base)
  }

  var _base: C
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<C: Collection>(s: C) -> LazyCollection<C> {
  return LazyCollection(s)
}

