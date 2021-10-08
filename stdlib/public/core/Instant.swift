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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
public protocol InstantProtocol: Comparable, Hashable, Sendable {
  func advanced(by duration: Duration) -> Self
  func duration(to other: Self) -> Duration
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension InstantProtocol {
  public static func + (_ lhs: Self, _ rhs: Duration) -> Self {
    lhs.advanced(by: rhs)
  }
  
  public static func += (_ lhs: inout Self, _ rhs: Duration) {
    lhs = lhs.advanced(by: rhs)
  }
  
  public static func - (_ lhs: Self, _ rhs: Duration) -> Self {
    lhs.advanced(by: .zero - rhs)
  }
  
  public static func -= (_ lhs: inout Self, _ rhs: Duration) {
    lhs = lhs.advanced(by: .zero - rhs)
  }
  
  public static func - (_ lhs: Self, _ rhs: Self) -> Duration {
    rhs.duration(to: lhs)
  }
}
