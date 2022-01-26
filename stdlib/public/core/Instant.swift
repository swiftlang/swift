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

public protocol InstantProtocol: Comparable, Hashable, Sendable {
  associatedtype Interval: DurationProtocol
  func advanced(by duration: Interval) -> Self
  func duration(to other: Self) -> Interval
}

/*
disabled for now - this perturbs operator resolution
extension InstantProtocol {
  @_alwaysEmitIntoClient
  @inlinable
  public static func + (_ lhs: Self, _ rhs: Interval) -> Self {
    lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func += (_ lhs: inout Self, _ rhs: Interval) {
    lhs = lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (_ lhs: Self, _ rhs: Interval) -> Self {
    lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func -= (_ lhs: inout Self, _ rhs: Interval) {
    lhs = lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (_ lhs: Self, _ rhs: Self) -> Interval {
    rhs.duration(to: lhs)
  }
}
*/
