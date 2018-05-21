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
@_fixed_layout // FIXME(sil-serialize-all)
public // @testable
struct _UnsafeBitMap {
  public // @testable
  let values: UnsafeMutablePointer<UInt>

  public // @testable
  let bitCount: Int

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  static func wordIndex(_ i: Int) -> Int {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return Int(bitPattern: UInt(bitPattern: i) / UInt(UInt.bitWidth))
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  static func bitIndex(_ i: Int) -> UInt {
    // Note: We perform the operation on UInts to get faster unsigned math
    // (shifts).
    return UInt(bitPattern: i) % UInt(UInt.bitWidth)
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  static func sizeInWords(forSizeInBits bitCount: Int) -> Int {
    return (bitCount + Int.bitWidth - 1) / Int.bitWidth
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  init(storage: UnsafeMutablePointer<UInt>, bitCount: Int) {
    self.bitCount = bitCount
    self.values = storage
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var numberOfWords: Int {
    return _UnsafeBitMap.sizeInWords(forSizeInBits: bitCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  func initializeToZero() {
    values.initialize(repeating: 0, count: numberOfWords)
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  subscript(i: Int) -> Bool {
    get {
      _sanityCheck(i < Int(bitCount) && i >= 0, "index out of bounds")
      let word = values[_UnsafeBitMap.wordIndex(i)]
      let bit = word & (1 << _UnsafeBitMap.bitIndex(i))
      return bit != 0
    }
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
