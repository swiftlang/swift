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

/// A wrapper around a bitmap storage with room for at least `bitCount` bits.
@_fixed_layout
@usableFromInline // @testable
internal struct _UnsafeBitMap {
  @usableFromInline
  internal let values: UnsafeMutablePointer<UInt>

  @usableFromInline
  internal let bitCount: Int

  @inlinable
  @inline(__always)
  internal static func wordIndex(_ i: Int) -> Int {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return Int(bitPattern: UInt(bitPattern: i) / UInt(UInt.bitWidth))
  }

  @inlinable
  @inline(__always)
  internal static func bitIndex(_ i: Int) -> UInt {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return UInt(bitPattern: i) % UInt(UInt.bitWidth)
  }

  @inlinable
  @inline(__always)
  internal static func sizeInWords(forSizeInBits bitCount: Int) -> Int {
    return (bitCount + Int.bitWidth - 1) / Int.bitWidth
  }

  @inlinable
  @inline(__always)
  internal init(storage: UnsafeMutablePointer<UInt>, bitCount: Int) {
    self.bitCount = bitCount
    self.values = storage
  }

  @inlinable
  internal var numberOfWords: Int {
    @inline(__always)
    get {
      return _UnsafeBitMap.sizeInWords(forSizeInBits: bitCount)
    }
  }

  @inlinable
  @inline(__always)
  internal func initializeToZero() {
    values.initialize(repeating: 0, count: numberOfWords)
  }

  @inlinable
  internal subscript(i: Int) -> Bool {
    @inline(__always)
    get {
      _sanityCheck(i < Int(bitCount) && i >= 0, "index out of bounds")
      let word = values[_UnsafeBitMap.wordIndex(i)]
      let bit = word & (1 << _UnsafeBitMap.bitIndex(i))
      return bit != 0
    }
    @inline(__always)
    nonmutating set {
      _sanityCheck(i < Int(bitCount) && i >= 0, "index out of bounds")
      let wordIdx = _UnsafeBitMap.wordIndex(i)
      let bitMask = (1 as UInt) &<< _UnsafeBitMap.bitIndex(i)
      if newValue {
        values[wordIdx] = values[wordIdx] | bitMask
      } else {
        values[wordIdx] = values[wordIdx] & ~bitMask
      }
    }
  }
}
