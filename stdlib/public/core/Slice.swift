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

public struct Slice<Base : Indexable> : CollectionType {

  public typealias Index = Base.Index

  public let startIndex: Index
  public let endIndex: Index

  public subscript(index: Index) -> Base._Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return _base[index]
  }

  public subscript(bounds: Range<Index>) -> Slice {
    Index._failEarlyRangeCheck2(
      bounds.startIndex, rangeEnd: bounds.endIndex,
      boundsStart: startIndex, boundsEnd: endIndex)
    return Slice(base: _base, bounds: bounds)
  }

  public init(base: Base, bounds: Range<Index>) {
    self._base = base
    self.startIndex = bounds.startIndex
    self.endIndex = bounds.endIndex
  }

  internal let _base: Base
}
