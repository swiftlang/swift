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
public protocol ContiguouslyStoredCollection : RandomAccessCollection {
  func withUnsafeBufferPointer<R>(
    _: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

extension ContiguouslyStoredCollection {
  @inline(__always)
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeBufferPointer(body)
  }
}

public protocol ContiguouslyStoredMutableCollection
  : ContiguouslyStoredCollection, MutableCollection {
  mutating func withUnsafeMutableBufferPointer<R>(
    _: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

extension ContiguouslyStoredMutableCollection {
  @inline(__always)
  public mutating func withExistingUnsafeMutableBuffer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try withUnsafeMutableBufferPointer(body)
  }
}
