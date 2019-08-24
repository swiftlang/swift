//===--- LifetimeTracked.swift --------------------------------------------===//
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

/// A type that tracks the number of live instances.
///
/// To be useful in more contexts, `LifetimeTracked` conforms to various
/// protocols in trivial ways.
///
/// `LifetimeTracked` is useful to check for leaks in algorithms and data
/// structures.  `StdlibUnittest` harness automatically checks that after each
/// test has done executing, no `LifetimeTracked` instances exist.
public final class LifetimeTracked {
  public init(_ value: Int, identity: Int = 0) {
    LifetimeTracked.instances += 1
    LifetimeTracked._nextSerialNumber += 1
    serialNumber = LifetimeTracked._nextSerialNumber
    self.value = value
    self.identity = identity
  }

  deinit {
    assert(serialNumber > 0, "double destruction!")
    LifetimeTracked.instances -= 1
    serialNumber = -serialNumber
  }

  public static var instances: Int = 0
  internal static var _nextSerialNumber = 0

  public let value: Int
  public var identity: Int
  public var serialNumber: Int = 0
}

extension LifetimeTracked : Equatable {
  public static func == (x: LifetimeTracked, y: LifetimeTracked) -> Bool {
    return x.value == y.value
  }
}

extension LifetimeTracked : Hashable {
  public var hashValue: Int {
    return value
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

extension LifetimeTracked : Strideable {
  public func distance(to other: LifetimeTracked) -> Int {
    return self.value.distance(to: other.value)
  }

  public func advanced(by n: Int) -> LifetimeTracked {
    return LifetimeTracked(self.value.advanced(by: n))
  }
}

extension LifetimeTracked : CustomStringConvertible {
  public var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }
}

public func < (x: LifetimeTracked, y: LifetimeTracked) -> Bool {
  return x.value < y.value
}
