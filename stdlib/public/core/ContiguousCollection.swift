//===----------------------------------------------------------------------===//
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

/// A collection that supports access to its underlying contiguous storage.
@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
public protocol ContiguousCollection: Collection
where SubSequence: ContiguousCollection {
  /// Calls a closure with a pointer to the array's contiguous storage.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
public extension ContiguousCollection {
  @inlinable
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    return try withContiguousStorageIfAvailable(body)!
  }
}

/// A collection that supports mutable access to its underlying contiguous
/// storage.
@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
public protocol MutableContiguousCollection: ContiguousCollection, MutableCollection
where SubSequence: MutableContiguousCollection {
  /// Calls the given closure with a pointer to the array's mutable contiguous
  /// storage.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension MutableContiguousCollection {
  @inlinable
  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    return try withContiguousMutableStorageIfAvailable(body)!
  }
}

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension Array: MutableContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension ContiguousArray: MutableContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension ArraySlice: MutableContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension UnsafeBufferPointer: ContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension UnsafeMutableBufferPointer: MutableContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension Slice: ContiguousCollection where Base: ContiguousCollection { }

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension Slice: MutableContiguousCollection where Base: MutableContiguousCollection { }
