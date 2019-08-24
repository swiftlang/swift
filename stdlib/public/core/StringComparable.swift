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
  @_specialize(where Self == String, RHS == String)
  @_specialize(where Self == String, RHS == Substring)
  @_specialize(where Self == Substring, RHS == String)
  @_specialize(where Self == Substring, RHS == Substring)
  @_effects(readonly)
  public static func == <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return _stringCompare(
      lhs._wholeGuts, lhs._offsetRange,
      rhs._wholeGuts, rhs._offsetRange,
      expecting: .equal)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func != <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return !(lhs == rhs)
  }

  @inlinable
  @_specialize(where Self == String, RHS == String)
  @_specialize(where Self == String, RHS == Substring)
  @_specialize(where Self == Substring, RHS == String)
  @_specialize(where Self == Substring, RHS == Substring)
  @_effects(readonly)
  public static func < <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return _stringCompare(
      lhs._wholeGuts, lhs._offsetRange,
      rhs._wholeGuts, rhs._offsetRange,
      expecting: .less)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func > <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return rhs < lhs
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func <= <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return !(rhs < lhs)
  }

  @inlinable @inline(__always) // forward to other operator
  @_effects(readonly)
  public static func >= <RHS: StringProtocol>(lhs: Self, rhs: RHS) -> Bool {
    return !(lhs < rhs)
  }
}

extension String: Equatable {
  @inlinable @inline(__always) // For the bitwise comparision
  @_effects(readonly)
  @_semantics("string.equals")
  public static func == (lhs: String, rhs: String) -> Bool {
    return _stringCompare(lhs._guts, rhs._guts, expecting: .equal)
  }
}

extension String: Comparable {
  @inlinable @inline(__always) // For the bitwise comparision
  @_effects(readonly)
  public static func < (lhs: String, rhs: String) -> Bool {
    return _stringCompare(lhs._guts, rhs._guts, expecting: .less)
  }
}

extension Substring: Equatable {}
