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
  @inline(__always) // de-virtualize
  public static func ==<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) == String(rhs)
  }

  @inlinable
  @inline(__always) // de-virtualize
  public static func !=<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) != String(rhs)
  }

  @inlinable
  @inline(__always) // de-virtualize
  public static func < <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) < String(rhs)
  }

  @inlinable
  @inline(__always) // de-virtualize
  public static func > <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) > String(rhs)
  }

  @inlinable
  @inline(__always) // de-virtualize
  public static func <= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) <= String(rhs)
  }

  @inlinable
  @inline(__always) // de-virtualize
  public static func >= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    // TODO(UTF8 perf): This is a horribly slow means...
    return String(lhs) >= String(rhs)
  }
}

extension String : Equatable {
  @inlinable @inline(__always) // For the bitwise comparision
  public static func ==(lhs: String, rhs: String) -> Bool {
    if lhs._guts.rawBits == rhs._guts.rawBits { return true }
    return _compareStringsEqual(lhs, rhs)
  }
}

extension String : Comparable {
  @inlinable @inline(__always) // For the bitwise comparision
  public static func < (lhs: String, rhs: String) -> Bool {
    if lhs._guts.rawBits == rhs._guts.rawBits { return false }
    return _compareStringsLess(lhs, rhs)
  }
}

extension Substring : Equatable {}
