//===--- ArraySlice.swift -------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  - `ArraySlice<Element>` was previously a custom slice type for `Array`.
//    New language features have since allowed it to be implemented in terms
//    of `Slice`.
//
//===----------------------------------------------------------------------===//


@available(swift, deprecated: 5.0, renamed: "Slice")
public typealias ArraySlice<T> = Slice<[T]>

public protocol _ContiguouslyStored {
  associatedtype Element
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R

  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

extension _ContiguouslyStored where Self: MutableCollection {
  @inlinable
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeMutableBufferPointer {
      (bufferPointer) -> R in
      return try body(&bufferPointer)
    }
  }
}

extension Array: _ContiguouslyStored { }
extension ContiguousArray: _ContiguouslyStored { }

extension Slice: _ContiguouslyStored where Base: _ContiguouslyStored {
  @inlinable
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: _startIndex)
    let j = _base.distance(from: _base.startIndex, to: _endIndex)
    return try _base.withUnsafeBufferPointer { buf in
      let sliced = UnsafeBufferPointer(rebasing: buf[i..<j])
      return try body(sliced)
    }
  }

  @inlinable
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: _startIndex)
    let j = _base.distance(from: _base.startIndex, to: _endIndex)
    return try _base.withUnsafeMutableBufferPointer { buf in
      var sliced = UnsafeMutableBufferPointer(rebasing: buf[i..<j])
      return try body(&sliced)
    }
  }

  @inlinable
  public mutating func withUnsafeMutableBytes<R>(
    _ body: (UnsafeMutableRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self.withUnsafeMutableBufferPointer {
      return try body(UnsafeMutableRawBufferPointer($0))
    }
  }
  @inlinable
  public func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self.withUnsafeBufferPointer {
      try body(UnsafeRawBufferPointer($0))
    }
  }
}

extension Slice: Equatable where Base: _ContiguouslyStored, Base.Element: Equatable {
  public static func == (lhs: Slice<Base>, rhs: Slice<Base>) -> Bool {
    return lhs.elementsEqual(rhs)
  }
}

extension Slice: ExpressibleByArrayLiteral 
where Base: ExpressibleByArrayLiteral, Base.ArrayLiteralElement == Base.Element {
  public typealias ArrayLiteralElement = Element
  @inlinable
  public init(arrayLiteral elements: ArrayLiteralElement...) {
    let f = unsafeBitCast(Base.init(arrayLiteral:), to: (([ArrayLiteralElement])->Base).self)
    let base = f(elements)
    self = Slice(base: base, bounds: base.startIndex..<base.endIndex)
  }
}

extension Slice where Base: RangeReplaceableCollection {
  @inlinable
  @available(swift, introduced: 4.0)
  public func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> Base {
    return try Base(base[_startIndex..<_endIndex].lazy.filter(isIncluded))
  }
}

extension RangeReplaceableCollection where SubSequence == Slice<Self> {
  @inlinable
  public subscript(bounds: Range<Index>) -> SubSequence {
    get { return Slice(base: self, bounds: bounds) }
    set { replaceSubrange(bounds, with: newValue) }
  }
}

extension RangeReplaceableCollection 
where Self: MutableCollection, SubSequence == Slice<Self> {
  @inlinable
  public subscript(bounds: Range<Index>) -> SubSequence {
    get { 
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds) 
    }
    set { 
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      replaceSubrange(bounds, with: newValue) 
    }
  }
}

