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

@_exported import Foundation // Clang module

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension Range where Bound == Int {
  public init?(_ x: NSRange) {
    guard x.location != NSNotFound else { return nil }
    self.init(uncheckedBounds: (x.location, x.location + x.length))
  }
}

extension NSRange {
  public init(_ x: Range<Int>) {
    location = x.lowerBound
    length = x.count
  }

  @available(*, deprecated, message: "Use Range(_: NSRange) initializer instead")
  public func toRange() -> Range<Int>? {
    return Range(self)
  }
}

extension NSRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["location": location, "length": length])
  }
}

extension NSRange : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .range(Int64(location), Int64(length))
  }
}
