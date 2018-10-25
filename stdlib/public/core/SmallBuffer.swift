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
  // FIXME(rdar://45443666): The storage is currently set at 64 bytes, which
  // actually hides a normalization bug in validation-test/stdlib/String.swift,
  // where exceedingly long segments may compare only the first `n` code units
  // worth of a given segment before moving on to the next segment. We'd also
  // like to make this be 32-bytes, as commented out below. Adjusting the size
  // here was simpler than XFAILing the test, but when fixed, restore size to 32
  // bytes.
  //
  //  private var _inlineStorage: (UInt64, UInt64, UInt64, UInt64) = (0,0,0,0)
  //
  private var _inlineStorage: (
    UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64
  ) = (0,0,0,0,0,0,0,0)

  internal init() {
    _invariantCheck()
  }
}

extension _SmallBuffer {
  private var stride: Int { return MemoryLayout<T>.stride }
}

extension _SmallBuffer {
  private var byteCapacity: Int {
    return MemoryLayout.stride(ofValue: _inlineStorage)
  }
  internal var capacity: Int { return byteCapacity / stride }

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
   _sanityCheck(MemoryLayout<_SmallBuffer<Int>>.stride == byteCapacity)
   _sanityCheck(capacity * stride == byteCapacity)
   _sanityCheck(_isPOD(T.self))
 }
 #endif // INTERNAL_CHECKS_ENABLED
}

