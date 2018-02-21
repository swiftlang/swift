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

extension NSDate : CustomPlaygroundQuickLookable {
  @nonobjc
  var summary: String {
    let df = DateFormatter()
    df.dateStyle = .medium
    df.timeStyle = .short
    return df.string(from: self as Date)
  }

  @available(swift, deprecated: 3, obsoleted: 5, message: "PlaygroundQuickLook is being removed in Swift 5.0, including the Swift 3 and 4 compatibility modes; please remove or conditionalize usage on Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(summary)
  }
}
