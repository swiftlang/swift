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

public struct _prext_Slice<Base : _prext_Indexable> : CollectionType {

  public func generate() -> IndexingGenerator<_prext_Slice> {
    return IndexingGenerator(self)
  }
  public typealias Index = Base.Index
  public typealias Element = Base._Element

  public let startIndex: Index
  public let endIndex: Index

  public subscript(index: Index) -> Element {
    return _base[index]
  }

  public subscript(_prext_bounds bounds: Range<Index>) -> _prext_Slice {
    return _prext_Slice(_collection: _base, bounds: bounds)
  }
  
  internal init(_collection: Base, bounds: Range<Index>) {
    self._base = _collection
    self.startIndex = bounds.startIndex
    self.endIndex = bounds.endIndex
  }

  internal let _base: Base
}
