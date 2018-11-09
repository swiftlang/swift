//===----------------------------------------------------------------------===//
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

/// A collection that supports access to its underlying contiguous storage.
public protocol ContiguousCollection: Collection
where SubSequence: ContiguousCollection {
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

/// A collection that supports mutable access to its underlying contiguous
/// storage.
public protocol MutableContiguousCollection: ContiguousCollection, MutableCollection
where SubSequence: MutableContiguousCollection {
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

extension Array: MutableContiguousCollection { }
extension ArraySlice: MutableContiguousCollection { }
extension ContiguousArray: MutableContiguousCollection { }

extension Slice: ContiguousCollection where Base: ContiguousCollection {
  @inlinable
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: startIndex)
    let j = _base.distance(from: _base.startIndex, to: endIndex)
    return try _base.withUnsafeBufferPointer {
      return try body(UnsafeBufferPointer(rebasing: $0[i..<j]))
    }
  }
}

extension Slice: MutableContiguousCollection 
where Base: MutableContiguousCollection {
  @inlinable
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: startIndex)
    let j = _base.distance(from: _base.startIndex, to: endIndex)
    return try _base.withUnsafeMutableBufferPointer {
      var buffer = UnsafeMutableBufferPointer(rebasing: $0[i..<j])
      return try body(&buffer)
    }
  }
}

extension MutableContiguousCollection {
  @inlinable
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeMutableBufferPointer(body)
  }
}

extension UnsafeBufferPointer: ContiguousCollection {
  @inlinable
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    return try body(self)
  }
}

extension UnsafeMutableBufferPointer: MutableContiguousCollection {
  @inlinable
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    return try body(UnsafeBufferPointer(self))
  }

  @inlinable
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    return try body(&self)
  }
}
