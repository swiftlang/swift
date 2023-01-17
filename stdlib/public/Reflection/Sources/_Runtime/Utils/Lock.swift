//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@_silgen_name("_swift_reflection_lock_size")
func _lockSize() -> Int

@_silgen_name("_swift_reflection_lock_init")
func _lockInit(_: UnsafeRawPointer)

@_silgen_name("_swift_reflection_lock_lock")
func _lockLock(_: UnsafeRawPointer)

@_silgen_name("_swift_reflection_lock_unlock")
func _lockUnlock(_: UnsafeRawPointer)

class Lock<T> {
  var value: T

  var mutex: UnsafeRawPointer {
    UnsafeRawPointer(
      Builtin.projectTailElems(self, UnsafeRawPointer.self)
    )
  }

  init(_ x: T) {
    value = x
  }

  static func create(with initialValue: T) -> Lock<T> {
    let lock = Builtin.allocWithTailElems_1(
      Lock<T>.self,
      _lockSize()._builtinWordValue,
      UnsafeRawPointer.self
    )

    _lockInit(lock.mutex)

    return lock
  }

  func withLock<U>(_ body: @Sendable (inout T) throws -> U) rethrows -> U {
    _lockLock(mutex)

    defer {
      _lockUnlock(mutex)
    }

    return try body(&value)
  }
}

extension Lock: @unchecked Sendable {}
