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

public struct DefaultIndices<
  Elements : Indexable
  // FIXME(ABI)(compiler limitation):
  // Elements : Collection
> : Collection {

  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias Index = Elements.Index

  internal init(
    _elements: Elements,
    startIndex: Elements.Index,
    endIndex: Elements.Index
  ) {
    self._elements = _elements
    self._startIndex = startIndex
    self._endIndex = endIndex
  }

  public var startIndex: Elements.Index {
    return _startIndex
  }

  public var endIndex: Elements.Index {
    return _startIndex
  }

  public subscript(i: Index) -> Elements.Index {
    // FIXME: swift-3-indexing-model: range check.
    return i
  }

  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias SubSequence = DefaultIndices<Elements>

  public subscript(bounds: Range<Index>) -> DefaultIndices<Elements> {
    // FIXME: swift-3-indexing-model: range check.
    return DefaultIndices(
      _elements: _elements,
      startIndex: bounds.startIndex,
      endIndex: bounds.endIndex)
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check.
    return _elements.next(i)
  }

  public func _nextInPlace(i: inout Index) {
    // FIXME: swift-3-indexing-model: range check.
    _elements._nextInPlace(&i)
  }

  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias Indices = DefaultIndices<Elements>

  public var indices: Indices {
    return self
  }

  internal var _elements: Elements
  internal var _startIndex: Elements.Index
  internal var _endIndex: Elements.Index
}

extension Collection where Indices == DefaultIndices<Self> {
  public var indices: DefaultIndices<Self> {
    return DefaultIndices(
      _elements: self,
      startIndex: self.startIndex,
      endIndex: self.endIndex)
  }
}

