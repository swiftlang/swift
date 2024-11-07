//===--- MutexBox.swift ---------------------------------------------------===//
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

final class MutexBox<T>: @unchecked Sendable {
  private let lock = Lock()
  private var value: T

  init(_ value: T) {
    self.value = value
  }

  @inline(__always)
  func withLock<R>(_ body: (inout T) throws -> R) rethrows -> R {
    try lock.withLock {
      try body(&value)
    }
  }
}

extension MutexBox {
  convenience init<U>() where T == U? {
    self.init(nil)
  }
  convenience init<U, V>() where T == [U: V] {
    self.init([:])
  }
  convenience init<U>() where T == [U] {
    self.init([])
  }
}
