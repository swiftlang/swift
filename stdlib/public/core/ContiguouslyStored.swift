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

@usableFromInline
internal protocol _HasContiguousBytes {
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
extension UnsafeBufferPointer: _HasContiguousBytes {
  @inlinable @inline(__always)
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = UnsafeRawPointer(self.baseAddress._unsafelyUnwrappedUnchecked)
    let len = self.count &* MemoryLayout<Element>.stride
    return try body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}
extension UnsafeMutableBufferPointer: _HasContiguousBytes {
  @inlinable @inline(__always)
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = UnsafeRawPointer(self.baseAddress._unsafelyUnwrappedUnchecked)
    let len = self.count &* MemoryLayout<Element>.stride
    return try body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}
extension String: _HasContiguousBytes {
  @inlinable
  internal var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get { return self._guts.isFastUTF8 }
  }

  @inlinable @inline(__always)
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    if _fastPath(self._guts.isFastUTF8) {
      return try self._guts.withFastUTF8 {
        try body($0)
      }
    }

    return try ContiguousArray(self.utf8).withUnsafeBufferPointer {
      try body($0)
    }
  }

  @inlinable @inline(__always)
  internal func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self._withUTF8 { return try body(UnsafeRawBufferPointer($0)) }
  }
}
extension Substring: _HasContiguousBytes {
  @inlinable
  internal var _providesContiguousBytesNoCopy: Bool {
    @inline(__always) get { return self._wholeGuts.isFastUTF8 }
  }

  @inlinable @inline(__always)
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    if _fastPath(_wholeGuts.isFastUTF8) {
      return try _wholeGuts.withFastUTF8(range: self._offsetRange) {
        return try body($0)
      }
    }

    return try ContiguousArray(self.utf8).withUnsafeBufferPointer {
      try body($0)
    }
  }

  @inlinable @inline(__always)
  internal func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self._withUTF8 { return try body(UnsafeRawBufferPointer($0)) }
  }
}
