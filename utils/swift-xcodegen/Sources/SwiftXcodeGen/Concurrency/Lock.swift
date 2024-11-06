//===--- Lock.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import os

final class Lock: @unchecked Sendable {
  private let lockPtr: UnsafeMutablePointer<os_unfair_lock>
  init() {
    self.lockPtr = UnsafeMutablePointer<os_unfair_lock>.allocate(capacity: 1)
    self.lockPtr.initialize(to: os_unfair_lock())
  }

  func lock() {
    os_unfair_lock_lock(self.lockPtr)
  }

  func unlock() {
    os_unfair_lock_unlock(self.lockPtr)
  }

  @inline(__always)
  func withLock<R>(_ body: () throws -> R) rethrows -> R {
    lock()
    defer {
      unlock()
    }
    return try body()
  }

  deinit {
    self.lockPtr.deinitialize(count: 1)
    self.lockPtr.deallocate()
  }
}
