//===--- InlineTypeExtensions.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension CollectionOfOne {

  @available(SwiftStdlib 6.1, *)
  public var storage: Span<Element> {
    get {
      let pointer = UnsafePointer<Element>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 1)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}
