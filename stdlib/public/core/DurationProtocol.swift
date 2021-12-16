//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public protocol DurationProtocol: Comparable, AdditiveArithmetic, Sendable {
  static func / (_ lhs: Self, _ rhs: Int) -> Self
  static func /= (_ lhs: inout Self, _ rhs: Int)
  static func * (_ lhs: Self, _ rhs: Int) -> Self
  static func *= (_ lhs: inout Self, _ rhs: Int)
  
  static func / (_ lhs: Self, _ rhs: Self) -> Double
}

extension DurationProtocol {
  public static func /= (_ lhs: inout Self, _ rhs: Int) {
    lhs = lhs / rhs
  }
  
  public static func *= (_ lhs: inout Self, _ rhs: Int) {
    lhs = lhs * rhs
  }
}
