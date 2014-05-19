//===--- Reverse.swift - Lazy sequence reversal ---------------------------===//
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
/// A wrapper for a BidirectionalIndex that reverses its
/// direction of traversal
struct ReverseIndex<I: BidirectionalIndex> : BidirectionalIndex {
  var _base: I

  init(_ _base: I) { self._base = _base }

  func succ() -> ReverseIndex {
    return ReverseIndex(_base.pred())
  }
  
  func pred() -> ReverseIndex {
    return ReverseIndex(_base.succ())
  }
}

func == <I> (lhs: ReverseIndex<I>, rhs: ReverseIndex<I>) -> Bool {
  return lhs._base == rhs._base
}

/// The lazy `Collection` returned by `reverse(c)` where `c` is a
/// `Collection`
struct ReverseView<
  T: Collection where T.IndexType: BidirectionalIndex
> : Collection {
  typealias IndexType = ReverseIndex<T.IndexType>
  typealias GeneratorType = IndexingGenerator<ReverseView>

  init(_ _base: T) {
    self._base = _base 
  }

  func generate() -> IndexingGenerator<ReverseView> {
    return IndexingGenerator(self)
  }
  
  var startIndex: IndexType {
    return ReverseIndex(_base.endIndex)
  }
  
  var endIndex: IndexType {
    return ReverseIndex(_base.startIndex)
  }

  subscript(i: IndexType) -> T.GeneratorType.Element {
    return _base[i._base.pred()]
  }
  
  var _base: T
}

/// Return a lazy Collection containing the elements `x` of `source` for
/// which `includeElement(x)` is `true`
func reverse<
  C: Collection where C.IndexType: BidirectionalIndex
>(source: C) -> ReverseView<C> {
  return ReverseView(source)
}

