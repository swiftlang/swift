//===--- SmallBuffer.swift ------------------------------------------------===//
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

// A specialized type which is a buffer of trivial values utilizing in-line
// storage
//
internal struct _SmallBuffer<T: FixedWidthInteger> {
  private var _inlineStorage: (UInt64, UInt64, UInt64, UInt64) = (0,0,0,0)

  internal init() {
    _invariantCheck()
  }
}

extension _SmallBuffer {
  private var stride: Int { return MemoryLayout<T>.stride }
}

extension _SmallBuffer {
  internal var capacity: Int {  return 32 / stride }

  internal subscript(i: Int) -> T {
    get {
      _sanityCheck(i >= 0 && i < capacity)
      let capacity = self.capacity
      return withUnsafeBytes(of: _inlineStorage) {
        let rawPtr = $0.baseAddress._unsafelyUnwrappedUnchecked
        let bufPtr = UnsafeBufferPointer(
          start: rawPtr.assumingMemoryBound(to: T.self), count: capacity)
        return bufPtr[i]
      }
    }
    set {
      _sanityCheck(i >= 0 && i < capacity)
      let capacity = self.capacity
      withUnsafeMutableBytes(of: &_inlineStorage) {
        let rawPtr = $0.baseAddress._unsafelyUnwrappedUnchecked
        let bufPtr = UnsafeMutableBufferPointer(
          start: rawPtr.assumingMemoryBound(to: T.self), count: capacity)
        bufPtr[i] = newValue
      }
    }
  }
}

extension _SmallBuffer {
 #if !INTERNAL_CHECKS_ENABLED
 @inlinable @inline(__always) internal func _invariantCheck() {}
 #else
 @usableFromInline @inline(never) @_effects(releasenone)
 internal mutating func _invariantCheck() {
   _sanityCheck(MemoryLayout<_SmallBuffer<Int>>.stride == 32)
   _sanityCheck(capacity * stride == 32)
   _sanityCheck(_isPOD(T.self))
 }
 #endif // INTERNAL_CHECKS_ENABLED
}

