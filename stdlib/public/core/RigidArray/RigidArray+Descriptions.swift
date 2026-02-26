//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Collections open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#if compiler(>=6.2)

@available(SwiftStdlib 5.0, *)
extension RigidArray /*: CustomStringConvertible */ where Element: ~Copyable {
  public var description: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray /*: CustomDebugStringConvertible */ where Element: ~Copyable {
  public var debugDescription: String {
    /// FIXME: Print the item descriptions when available.
    "<\(count) items>"
  }
}

#endif
