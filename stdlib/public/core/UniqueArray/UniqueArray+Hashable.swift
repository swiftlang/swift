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

extension UniqueArray: Hashable where Element: Hashable & ~Copyable {
  @export(implementation)
  public func hash(into hasher: inout Hasher) {
    _storage.hash(into: &hasher)
  }
}
