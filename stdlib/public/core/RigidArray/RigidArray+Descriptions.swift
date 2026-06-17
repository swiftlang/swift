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
// extension RigidArray: CustomStringConvertible where Element: ~Copyable {
// }

@available(SwiftStdlib 6.4, *)
extension _RigidArray where Element: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  internal var description: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}

// FIXME: Enable once SE-0499 is implemented
// @available(SwiftStdlib x.y, *)
// extension RigidArray: CustomDebugStringConvertible where Element: ~Copyable {
// }

@available(SwiftStdlib 6.4, *)
extension _RigidArray where Element: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  internal var debugDescription: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}
