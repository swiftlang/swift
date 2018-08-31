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

@_exported import CoreFoundation

public protocol _CFObject: class, Hashable {}
extension _CFObject {
  public var hashValue: Int {
    return Int(bitPattern: CFHash(self))
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(self.hashValue)
  }
  public static func ==(left: Self, right: Self) -> Bool {
    return CFEqual(left, right)
  }
}
