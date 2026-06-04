//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// FIXME: Enable once SE-0499 is implemented
// @available(SwiftStdlib x.y, *)
// extension UniqueArray: CustomStringConvertible where Element: ~Copyable {
// }

@available(SwiftStdlib 6.4, *)
extension UniqueArray where Element: ~Copyable {
  public var description: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}

// FIXME: Enable once SE-0499 is implemented
// @available(SwiftStdlib x.y, *)
// extension UniqueArray: CustomDebugStringConvertible where Element: ~Copyable {
// }

@available(SwiftStdlib 6.4, *)
extension UniqueArray where Element: ~Copyable {
  public var debugDescription: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}
