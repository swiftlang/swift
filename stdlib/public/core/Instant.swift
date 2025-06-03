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

// A type that defines a specific point in time for a given `Clock`.
@available(SwiftStdlibCurrentOS 5.7, *)
public protocol InstantProtocol<Duration>: Comparable, Hashable, Sendable {
  associatedtype Duration: DurationProtocol
  func advanced(by duration: Duration) -> Self
  func duration(to other: Self) -> Duration
}

/*
disabled for now - this perturbs operator resolution
extension InstantProtocol {
  @_alwaysEmitIntoClient
  @inlinable
  public static func + (_ lhs: Self, _ rhs: Duration) -> Self {
    lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func += (_ lhs: inout Self, _ rhs: Duration) {
    lhs = lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (_ lhs: Self, _ rhs: Duration) -> Self {
    lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func -= (_ lhs: inout Self, _ rhs: Duration) {
    lhs = lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (_ lhs: Self, _ rhs: Self) -> Duration {
    rhs.duration(to: lhs)
  }
}
*/
