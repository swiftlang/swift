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


#if os(macOS)

//===----------------------------------------------------------------------===//
// NSRectEdge
//===----------------------------------------------------------------------===//

extension NSRectEdge {
  @inlinable
  public init(rectEdge: CGRectEdge) {
    self = NSRectEdge(rawValue: UInt(rectEdge.rawValue))!
  }
}

extension CGRectEdge {
  @inlinable
  public init(rectEdge: NSRectEdge) {
    self = CGRectEdge(rawValue: UInt32(rectEdge.rawValue))!
  }
}

#endif
