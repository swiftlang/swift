//===--- UnicodeViewSlice.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
public struct UnicodeViewSlice<BaseView: _UnicodeView>
  : BidirectionalCollectionWrapper, UnicodeView {
  public typealias Base = BidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func index(atEncodedOffset n: Int64) -> Index {
    return base.base.index(atEncodedOffset: n)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return BaseView.encodedOffset(of: i)
  }
  public typealias SubSequence = UnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RandomAccessUnicodeViewSlice<
  BaseView: _UnicodeView & RandomAccessCollection
> : BidirectionalCollectionWrapper, UnicodeView, RandomAccessCollection {
  public typealias Base = RandomAccessSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func index(atEncodedOffset n: Int64) -> Index {
    return base.base.index(atEncodedOffset: n)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return BaseView.encodedOffset(of: i)
  }
  public typealias SubSequence = RandomAccessUnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RangeReplaceableUnicodeViewSlice<
  BaseView: _UnicodeView & RangeReplaceableCollection
> : BidirectionalCollectionWrapper, UnicodeView, RangeReplaceableCollection {
  public typealias Base = RangeReplaceableBidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public func index(atEncodedOffset n: Int64) -> Index {
    return base.base.index(atEncodedOffset: n)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return BaseView.encodedOffset(of: i)
  }
  public typealias SubSequence = RangeReplaceableUnicodeViewSlice<BaseView>
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
  public mutating func replaceSubrange<C: Collection>(
    _ r: Range<Index>, with replacement: C
  )
  where C.Iterator.Element == Iterator.Element {
    return base.replaceSubrange(r,  with: replacement)
  }
  init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  
  public init() {
    base = Base()
  }
  public mutating func _tryToReplaceSubrange<C>(
    from targetStart: Index, to targetEnd: Index,
    with replacement: C) -> Bool
  where C : Collection, C.Iterator.Element == Iterator.Element {
    return base._tryToReplaceSubrange(
      from: targetStart, to: targetEnd, with: replacement)
  }
}
