//===--- LifetimeTracked.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// TODO: swift-3-indexing-model - this conformed to ForwardIndex but not sure why it did.

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


extension LifetimeTracked : Strideable {
  @warn_unused_result
  public func distance(to other: LifetimeTracked) -> Int {
    return self.value.distance(to: other.value)
  }

  @warn_unused_result
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

public func == (x: LifetimeTracked, y: LifetimeTracked) -> Bool {
  return x.value == y.value
}

public func < (x: LifetimeTracked, y: LifetimeTracked) -> Bool {
  return x.value < y.value
}
