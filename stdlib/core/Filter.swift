//===--- Filter.swift - Lazily filter the elements of a Sequence ----------===//
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

/// The `Generator` used by `FilterSequenceView` and
/// `FilterCollectionView`
struct FilterGenerator<Base: Generator> : Generator, Sequence {
  mutating func next() -> Base.Element? {
    var n: Base.Element?
    for/*ever*/;; {
      n = _base.next()
      if n ? _include(n!) : true {
        return n
      }
    }
  }
  
  func generate() -> FilterGenerator {
    return self
  }

  var _base: Base
  var _include: (Base.Element)->Bool
}

/// The lazy `Sequence` returned by `filter(c)` where `c` is a
/// `Sequence`
struct FilterSequenceView<Base: Sequence> : Sequence {
  func generate() -> FilterGenerator<Base.GeneratorType> {
    return FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.GeneratorType.Element)->Bool
}

/// The `IndexType` used for subscripting a `FilterCollectionView`
struct FilterCollectionViewIndex<Base: Collection> : ForwardIndex {
  func succ() -> FilterCollectionViewIndex {
    for nextPos in _pos.succ().._end {
      if _include(_base[nextPos]) {
        return FilterCollectionViewIndex(
          _pos: nextPos, _end: _end,
          _base: _base, _include: _include)
      }
    }
    return FilterCollectionViewIndex(
      _pos: _end, _end: _end, _base: _base, _include: _include)
  }
  var _pos: Base.IndexType
  var _end: Base.IndexType
  var _base: Base
  var _include: (Base.GeneratorType.Element)->Bool
}

func == <Base: Collection>(
  lhs: FilterCollectionViewIndex<Base>,
  rhs: FilterCollectionViewIndex<Base>
) -> Bool {
  return lhs._pos == rhs._pos
}

/// The lazy `Collection` returned by `filter(c)` where `c` is a
/// `Collection`
struct FilterCollectionView<Base: Collection> : Collection {

  typealias IndexType = FilterCollectionViewIndex<Base>
  var startIndex: IndexType {
    var first = _base.startIndex
    while first != _base.endIndex {
      if _include(_base[first]) {
        break
      }
      ++first
    }
    return FilterCollectionViewIndex(
      _pos: first, _end: _base.endIndex, _base: _base, _include: _include)
  }
  
  var endIndex: IndexType {
    return FilterCollectionViewIndex(
      _pos: _base.endIndex, _end: _base.endIndex,
      _base: _base, _include: _include)
  }

  subscript(index: IndexType) -> Base.GeneratorType.Element {
    return _base[index._pos]
  }

  func generate() -> FilterGenerator<Base.GeneratorType> {
    return FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.GeneratorType.Element)->Bool
}

/// Return a lazy Sequence containing the elements `x` of `source` for
/// which `includeElement(x)` is `true`
func filter<S:Sequence>(
  source: S, includeElement: (S.GeneratorType.Element)->Bool
) -> FilterSequenceView<S> {
  return FilterSequenceView(_base: source, _include: includeElement)
}

/// Return a lazy Collection containing the elements `x` of `source` for
/// which `includeElement(x)` is `true`
func filter<C:Collection>(
  source: C, includeElement: (C.GeneratorType.Element)->Bool
) -> FilterCollectionView<C> {
  return FilterCollectionView(_base: source, _include: includeElement)
}
