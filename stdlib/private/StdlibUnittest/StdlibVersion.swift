//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct StdlibVersion: RawRepresentable {
  public let rawValue: (Int, Int)

  public init(rawValue: (Int, Int)) {
    self.rawValue = rawValue
  }
}

extension StdlibVersion {
  public static func custom(_ version1: Int, _ version2: Int) -> StdlibVersion {
    return StdlibVersion(rawValue: (version1, version2))
  }

  public static var currentlyRunning: StdlibVersion {
    return StdlibVersion(rawValue: _stdlibDynamicVersion)
  }

  // Shipped in macOS 10.14.2, iOS 12.2, watchOS 5.2, tvOS 13.2
  public static var swift5_0: StdlibVersion {
    return StdlibVersion(rawValue: (1000, 0))
  }
}

extension StdlibVersion: CustomStringConvertible {
  public var description: String {
    return "\(rawValue)"
  }
}

extension StdlibVersion: Equatable {
  public static func == (left: StdlibVersion, right: StdlibVersion) -> Bool {
    return left.rawValue == right.rawValue
  }
}

extension StdlibVersion: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(rawValue.0)
    hasher.combine(rawValue.1)
  }
}

extension StdlibVersion: Comparable {
  public static func < (left: StdlibVersion, right: StdlibVersion) -> Bool {
    return left.rawValue < right.rawValue
  }
}
