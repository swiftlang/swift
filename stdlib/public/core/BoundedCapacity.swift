//===--- BoundedCapacity.swift --------------------------------------------===//
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
// Bounded capacity inline buffers
//===----------------------------------------------------------------------===//

/// A RangeReplaceableCollection that can report its maximum capacity
internal protocol _BoundedCollection : RangeReplaceableCollection {
  var maxCapacity : IndexDistance { get }
}

internal protocol _DefaultConstructible {
  init()
}

extension _FixedSizeCollection where Iterator.Element : _DefaultConstructible
{
  init() {
    self.init(repeating: Iterator.Element())
  }
}

/// Wraps a fixed-length `Base` collection, making it variable-length up to the
/// length of the base.
internal struct _BoundedCapacity<
  Base: MutableCollection & RandomAccessCollection & _FixedSizeCollection
> : RandomAccessCollection,
    _BoundedCollection,
    MutableCollection
where Base.Iterator.Element : _DefaultConstructible
{
  typealias Element = Base.Iterator.Element
  typealias IndexDistance = Base.IndexDistance
  
  public // accessed from @inline(__always)
  var _base: Base
  var count: IndexDistance

  public typealias Index = Base.Index
  
  public var startIndex : Index {
    @inline(__always) get { return _base.startIndex }
  }
  
  public var endIndex : Index {
    @inline(__always) get { return _base.index(atOffset: count) }
  }
  
  public subscript(i: Index) -> Element {
    @inline(__always)
    get {
      return _base[i]
    }
    @inline(__always)
    set {
      _base[i] = newValue
    }
  }
  
  @inline(__always)
  public mutating func append(_ x: Element) {
    _base[_base.index(atOffset: count)] = x
    count += 1 as IndexDistance
  }

  @inline(__always)
  public init() { _base = Base(); count = 0 }
  
  @inline(__always)
  public init<S: Sequence>(_ s: S) where S.Iterator.Element == Element {
    self.init()
    append(contentsOf: s)
  }

  @inline(__always)
  public mutating func append<S: Sequence>(contentsOf s: S)
  where S.Iterator.Element == Element {
    for x in s {
      append(x)
    }
  }

  @inline(__always)
  public mutating func replaceSubrange<C>(
    _ target: Range<Index>, with newElements: C
  ) where C : Collection, C.Iterator.Element == Element {
    if target.lowerBound == endIndex {
      append(contentsOf: newElements)
    }
    else {
      var newSelf = _BoundedCapacity(self[..<target.lowerBound])
      newSelf.append(contentsOf: newElements)
      newSelf.append(contentsOf: self[target.upperBound...])
      self = newSelf
    }
  }

  @inline(__always)
  public func index(after i: Index) -> Index {
    return _base.index(after: i)
  }
  @inline(__always)
  public func index(before i: Index) -> Index {
    return _base.index(before: i)
  }
  @inline(__always)
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    return _base.index(i, offsetBy: n)

  }

  var maxCapacity : IndexDistance {
    @inline(__always)
    get {
      return _base.count
    }
  }
}
