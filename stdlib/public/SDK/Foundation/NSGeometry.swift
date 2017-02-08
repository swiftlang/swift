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
import CoreGraphics


#if os(OSX)

//===----------------------------------------------------------------------===//
// NSRectEdge
//===----------------------------------------------------------------------===//

// In the SDK, the following NS*Edge constants are defined as macros for the
// corresponding CGRectEdge enumerators.  Thus, in the SDK, NS*Edge constants
// have CGRectEdge type.  This is not correct for Swift (as there is no
// implicit conversion to NSRectEdge).

@available(*, unavailable, renamed: "NSRectEdge.MinX")
public var NSMinXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MinY")
public var NSMinYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxX")
public var NSMaxXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxY")
public var NSMaxYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}

extension NSRectEdge {
  public init(rectEdge: CGRectEdge) {
    self = NSRectEdge(rawValue: UInt(rectEdge.rawValue))!
  }
}

extension CGRectEdge {
  public init(rectEdge: NSRectEdge) {
    self = CGRectEdge(rawValue: UInt32(rectEdge.rawValue))!
  }
}

#endif
