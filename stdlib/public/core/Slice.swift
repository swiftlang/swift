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

public struct _prext_Slice<
  UnderlyingCollection : _CollectionSliceDefaultsType
> : CollectionType {

  public typealias Index = UnderlyingCollection.Index
  public typealias Element = UnderlyingCollection._Element

  public let startIndex: Index
  public let endIndex: Index

  public subscript(index: Index) -> Element {
    return _underlyingCollection[index]
  }

  internal init(_collection: UnderlyingCollection, bounds: Range<Index>) {
    self._underlyingCollection = _collection
    self.startIndex = bounds.startIndex
    self.endIndex = bounds.endIndex
  }

  internal let _underlyingCollection: UnderlyingCollection
}

