//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===--- ContiguousBytes --------------------------------------------------===//

/// Indicates that the conforming type is a contiguous collection of raw bytes
/// whose underlying storage is directly accessible by withUnsafeBytes.
public protocol ContiguousBytes {
    /// Calls the given closure with the contents of underlying storage.
    ///
    /// - note: Calling `withUnsafeBytes` multiple times does not guarantee that
    ///         the same buffer pointer will be passed in every time.
    /// - warning: The buffer argument to the body should not be stored or used
    ///            outside of the lifetime of the call to the closure.
    func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R
}

extension Data {
    @inlinable
    @available(swift, introduced: 5)
    public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try withContiguousUnsafeBytes(body)
    }
}

//===--- Collection Conformances ------------------------------------------===//

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension Array : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try withUnsafeBytes(body)
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension ArraySlice : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try withUnsafeBytes(body)
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension ContiguousArray : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try withUnsafeBytes(body)
    }
}

//===--- Pointer Conformances ---------------------------------------------===//

extension UnsafeRawBufferPointer : ContiguousBytes {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try body(self)
    }
}

extension UnsafeMutableRawBufferPointer : ContiguousBytes {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try body(UnsafeRawBufferPointer(self))
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension UnsafeBufferPointer : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try body(UnsafeRawBufferPointer(self))
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension UnsafeMutableBufferPointer : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try body(UnsafeRawBufferPointer(self))
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension EmptyCollection : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        return try body(UnsafeRawBufferPointer(start: nil, count: 0))
    }
}

// FIXME: When possible, expand conformance to `where Element : Trivial`.
extension CollectionOfOne : ContiguousBytes where Element == UInt8 {
    @inlinable
    public func withContiguousUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        let element = self.first!
        return try Swift.withUnsafeBytes(of: element) {
            return try body($0)
        }
    }
}

//===--- Conditional Conformances -----------------------------------------===//

extension Slice : ContiguousBytes where Base : ContiguousBytes {
    public func withContiguousUnsafeBytes<ResultType>(_ body: (UnsafeRawBufferPointer) throws -> ResultType) rethrows -> ResultType {
        let offset = base.distance(from: base.startIndex, to: self.startIndex)
        return try base.withContiguousUnsafeBytes { ptr in
            let slicePtr = ptr.baseAddress?.advanced(by: offset)
            let sliceBuffer = UnsafeRawBufferPointer(start: slicePtr, count: self.count)
            return try body(sliceBuffer)
        }
    }
}
