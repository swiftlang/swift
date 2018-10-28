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

import SwiftShims

extension StringProtocol {
  @inlinable
  @_specialize(where Self == String, R == String)
  @_specialize(where Self == String, R == Substring)
  @_specialize(where Self == Substring, R == String)
  @_specialize(where Self == Substring, R == Substring)
  @_effects(readonly)
  public static func == <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return lhs._gutsSlice.compare(with: rhs._gutsSlice, expecting: .equal)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func != <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(lhs == rhs)
  }

  @inlinable
  @_specialize(where Self == String, R == String)
  @_specialize(where Self == String, R == Substring)
  @_specialize(where Self == Substring, R == String)
  @_specialize(where Self == Substring, R == Substring)
  @_effects(readonly)
  public static func < <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return lhs._gutsSlice.compare(with: rhs._gutsSlice, expecting: .less)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func > <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return rhs < lhs
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func <= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(rhs < lhs)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func >= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(lhs < rhs)
  }
}

extension String : Equatable {
  @inlinable @inline(__always) // For the bitwise comparision
  @_effects(readonly)
  public static func == (lhs: String, rhs: String) -> Bool {
    if lhs._guts.rawBits == rhs._guts.rawBits { return true }
    if _fastPath(lhs._guts.isNFCFastUTF8 && rhs._guts.isNFCFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      return lhs._guts.withFastUTF8 { nfcSelf in
        return rhs._guts.withFastUTF8 { nfcOther in
          return _binaryCompare(nfcSelf, nfcOther) == 0
        }
      }
    }

    return lhs._gutsSlice.compare(with: rhs._gutsSlice, expecting: .equal)
  }
}

extension String : Comparable {
  @inlinable @inline(__always) // For the bitwise comparision
  @_effects(readonly)
  public static func < (lhs: String, rhs: String) -> Bool {
    if lhs._guts.rawBits == rhs._guts.rawBits { return false }
    if _fastPath(lhs._guts.isNFCFastUTF8 && rhs._guts.isNFCFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      return lhs._guts.withFastUTF8 { nfcSelf in
        return rhs._guts.withFastUTF8 { nfcOther in
          return _binaryCompare(nfcSelf, nfcOther) < 0
        }
      }
    }

    return lhs._gutsSlice.compare(with: rhs._gutsSlice, expecting: .less)
  }
}

extension Substring : Equatable {}
