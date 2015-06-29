//===--- LifetimeTracked.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public final class LifetimeTracked : ForwardIndexType, CustomStringConvertible {
  public init(_ value: Int) {
    ++LifetimeTracked.instances
    serialNumber = ++LifetimeTracked._nextSerialNumber
    self.value = value
  }

  deinit {
    assert(serialNumber > 0, "double destruction!")
    --LifetimeTracked.instances
    serialNumber = -serialNumber
  }

  public var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// Requires: the next value is representable.
  public func successor() -> LifetimeTracked {
    return LifetimeTracked(self.value.successor())
  }

  public static var instances: Int = 0
  internal static var _nextSerialNumber = 0
  
  public let value: Int
  public var serialNumber: Int = 0
}

public func == (x: LifetimeTracked, y: LifetimeTracked) -> Bool {
  return x.value == y.value
}
