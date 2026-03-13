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

// NOTE: The below is necessary for fast String initialization from untyped
// memory. When we add Collection.withContiguousRawStorageIfAvailable(), we can
// deprecate this functionality.

@usableFromInline
internal protocol _HasContiguousBytes {
  @safe
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R

  var _providesContiguousBytesNoCopy: Bool { get }
}
extension _HasContiguousBytes {
  @inlinable
  var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get { return true }
  }
}
extension Array: _HasContiguousBytes {
  @inlinable
  var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get {
#if _runtime(_ObjC)
      return _buffer._isNative
#else
      return true
#endif
    }
  }
}
extension ContiguousArray: _HasContiguousBytes {}
extension UnsafeBufferPointer: @unsafe _HasContiguousBytes {
  @inlinable @inline(__always)
  @safe
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = unsafe UnsafeRawPointer(self.baseAddress)
    let len = self.count &* MemoryLayout<Element>.stride
    return try unsafe body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}
extension UnsafeMutableBufferPointer: @unsafe _HasContiguousBytes {
  @inlinable @inline(__always)
  @safe
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = UnsafeRawPointer(self.baseAddress)
    let len = self.count &* MemoryLayout<Element>.stride
    return try unsafe body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}
extension UnsafeRawBufferPointer: @unsafe _HasContiguousBytes {
  @inlinable @inline(__always)
  @safe
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try unsafe body(self)
  }
}
extension UnsafeMutableRawBufferPointer: @unsafe _HasContiguousBytes {
  @inlinable @inline(__always)
  @safe
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try unsafe body(UnsafeRawBufferPointer(self))
  }
}
extension String: _HasContiguousBytes {
  @inlinable
  internal var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get { return self._guts.isFastUTF8 }
  }

  @inlinable @inline(__always)
  @safe
  internal func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8 { return try unsafe body(UnsafeRawBufferPointer($0)) }
  }
}
extension Substring: _HasContiguousBytes {
  @inlinable
  internal var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get { return self._wholeGuts.isFastUTF8 }
  }

  @inlinable @inline(__always)
  @safe
  internal func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8 { return try unsafe body(UnsafeRawBufferPointer($0)) }
  }
}
