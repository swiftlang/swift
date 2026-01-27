//===--- Mutex+Extensions.swift -------------------------------------------===//
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

public import Synchronization

public extension Mutex {
  init() where Value == Void {
    self.init(())
  }

  init<T>() where Value == T? {
    self.init(nil)
  }

  init<T>() where Value == [T] {
    self.init([])
  }

  init<T, U>() where Value == [T: U] {
    self.init([:])
  }
}

/// Having to write `_ in` when the value is `Void` is annoying.
public extension Mutex where Value == Void {
  borrowing func withLock<Result: ~Copyable & Sendable, E>(
    _ body: () throws(E) -> sending Result
  ) throws(E) -> sending Result {
    return try self.withLock { _ throws(E) in
      try body()
    }
  }
}
