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
@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
public protocol _ContiguousCollection: Collection
where SubSequence: _ContiguousCollection {
    /// Calls a closure with a pointer to the array's contiguous storage.
    func withUnsafeBufferPointer<R>(
        _ body: (UnsafeBufferPointer<Element>) throws -> R
        ) rethrows -> R
}

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
public extension _ContiguousCollection {
    func withUnsafeBufferPointer<R>(
        _ body: (UnsafeBufferPointer<Element>) throws -> R
        ) rethrows -> R {
        return try withContiguousStorageIfAvailable(body)!
    }
}

/// A collection that supports mutable access to its underlying contiguous
/// storage.
@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
public protocol _MutableContiguousCollection: _ContiguousCollection, MutableCollection
where SubSequence: _MutableContiguousCollection {
    /// Calls the given closure with a pointer to the array's mutable contiguous
    /// storage.
    mutating func withUnsafeMutableBufferPointer<R>(
        _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
        ) rethrows -> R
}

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension _MutableContiguousCollection {
    public mutating func withUnsafeMutableBufferPointer<R>(
        _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
        ) rethrows -> R {
        return try withContiguousMutableStorageIfAvailable(body)!
    }
}

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension Array: _MutableContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension ContiguousArray: _MutableContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension ArraySlice: _MutableContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension UnsafeBufferPointer: _ContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension UnsafeMutableBufferPointer: _MutableContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension Slice: _ContiguousCollection where Base: _ContiguousCollection { }

@available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *)
extension Slice: _MutableContiguousCollection where Base: _MutableContiguousCollection { }
