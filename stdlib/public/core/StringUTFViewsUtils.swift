//===--- StringUTFViewsUtils.swift ----------------------------------------===//
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

struct _MirrorChildrenCollection<T : CollectionType where T.Index.Distance == Int> : CollectionType {
  let underlying: T

  var startIndex: Int { return 0 }
  var endIndex: Int { return underlying.count }

  subscript(i: Int) -> (label: String?, value: Any) {
    _precondition(i >= startIndex && i < endIndex, "_MirrorChildrenCollection access out of bounds")
    return ("[\(i)]", underlying[underlying.startIndex.advancedBy(i)])
  }

  func generate() -> _MirrorChildrenCollectionGenerator<T> {
    return _MirrorChildrenCollectionGenerator(underlying)
  }
}

struct _MirrorChildrenCollectionGenerator<T : CollectionType where T.Index.Distance == Int> : GeneratorType {
  let underlying: T
  // Invariant: currentStringIndex is always updated at the same time as currentPosition.
  var currentPosition: Int = 0
  var currentStringIndex: T.Index

  init(_ underlying: T) {
    self.underlying = underlying
    currentStringIndex = underlying.startIndex
  }

  mutating func next() -> (label: String?, value: Any)? {
    // This generator implementation provides optimized sequential access whenever the SequenceType APIs are being
    // used to traverse the collection in forward order.
    guard currentPosition < underlying.count else {
      return nil
    }
    let result: (String?, Any) = ("[\(currentPosition)]", underlying[currentStringIndex])
    currentPosition += 1
    currentStringIndex = currentStringIndex.successor()
    return result
  }
}
