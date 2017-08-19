//===--- ContiguouslyStored.swift -----------------------------------------===//
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
public protocol _ContiguouslyStoredCollection : RandomAccessCollection {
  func withUnsafeBufferPointer<R>(
    _: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

extension _ContiguouslyStoredCollection {
  @inline(__always)
  public func _withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeBufferPointer(body)
  }
}

public protocol _ContiguouslyStoredMutableCollection
  : _ContiguouslyStoredCollection, MutableCollection {
  mutating func withUnsafeMutableBufferPointer<R>(
    _: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

extension _ContiguouslyStoredMutableCollection {
  @inline(__always)
  public mutating func _withExistingUnsafeMutableBuffer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeMutableBufferPointer(body)
  }

  @inline(__always)
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R {
    var me = self
    return try me.withUnsafeMutableBufferPointer {
      try body(
        UnsafeBufferPointer(
          start: UnsafePointer($0.baseAddress), count: $0.count))
    }
  }
}

extension UnsafeBufferPointer : _ContiguouslyStoredCollection {
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer) throws -> R
  ) rethrows -> R {
    return try body(self)
  }
}

extension UnsafeMutableBufferPointer : _ContiguouslyStoredMutableCollection {
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer) throws -> R
  ) rethrows -> R {
    return try body(&self)
  }
}
extension Array : _ContiguouslyStoredMutableCollection {  }
extension ArraySlice : _ContiguouslyStoredMutableCollection {  }
