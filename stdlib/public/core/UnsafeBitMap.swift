//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A wrapper around a bitmap storage with room for at least `bitCount` bits.
internal struct _UnsafeBitMap {
  internal let values: UnsafeMutablePointer<UInt>
  internal let bitCount: Int

  internal static func wordIndex(_ i: Int) -> Int {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return Int(bitPattern: UInt(bitPattern: i) / UInt(UInt._sizeInBits))
  }

  internal static func bitIndex(_ i: Int) -> UInt {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return UInt(bitPattern: i) % UInt(UInt._sizeInBits)
  }

  internal static func sizeInWords(forSizeInBits bitCount: Int) -> Int {
    return bitCount + Int._sizeInBytes - 1 / Int._sizeInBytes
  }

  internal init(storage: UnsafeMutablePointer<UInt>, bitCount: Int) {
    self.bitCount = bitCount
    self.values = storage
  }

  internal var numberOfWords: Int {
    return _UnsafeBitMap.sizeInWords(forSizeInBits: bitCount)
  }

  internal func initializeToZero() {
    values.initialize(with: 0, count: numberOfWords)
  }

  internal subscript(i: Int) -> Bool {
    get {
      _sanityCheck(i < Int(bitCount) && i >= 0, "index out of bounds")
      let word = values[_UnsafeBitMap.wordIndex(i)]
      let bit = word & (1 << _UnsafeBitMap.bitIndex(i))
      return bit != 0
    }
    nonmutating set {
      _sanityCheck(i < Int(bitCount) && i >= 0, "index out of bounds")
      let wordIdx = _UnsafeBitMap.wordIndex(i)
      let bitMask = 1 << _UnsafeBitMap.bitIndex(i)
      if newValue {
        values[wordIdx] = values[wordIdx] | bitMask
      } else {
        values[wordIdx] = values[wordIdx] & ~bitMask
      }
    }
  }
}

