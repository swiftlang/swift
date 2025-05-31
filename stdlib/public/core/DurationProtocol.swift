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

/// A type that defines a duration for a given `InstantProtocol` type.
@available(SwiftStdlibCurrentOS 5.7, *)
public protocol DurationProtocol: Comparable, AdditiveArithmetic, Sendable {
  static func / (_ lhs: Self, _ rhs: Int) -> Self
  static func /= (_ lhs: inout Self, _ rhs: Int)
  static func * (_ lhs: Self, _ rhs: Int) -> Self
  static func *= (_ lhs: inout Self, _ rhs: Int)

  static func / (_ lhs: Self, _ rhs: Self) -> Double
}

@available(SwiftStdlibCurrentOS 5.7, *)
extension DurationProtocol {
  @available(SwiftStdlibCurrentOS 5.7, *)
  public static func /= (_ lhs: inout Self, _ rhs: Int) {
    lhs = lhs / rhs
  }

  @available(SwiftStdlibCurrentOS 5.7, *)
  public static func *= (_ lhs: inout Self, _ rhs: Int) {
    lhs = lhs * rhs
  }
}
