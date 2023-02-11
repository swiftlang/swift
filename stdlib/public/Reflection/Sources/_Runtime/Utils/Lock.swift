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

struct Lock<T> {
  var storage: ManagedBuffer<T, UInt8>

  init(initialValue: T) {
    storage = .create(minimumCapacity: _lockSize()) {
      $0.withUnsafeMutablePointerToElements {
        _lockInit(UnsafeRawPointer($0))
      }

      return initialValue
    }
  }

  func withLock<U>(_ body: @Sendable (inout T) throws -> U) rethrows -> U {
    try storage.withUnsafeMutablePointers { header, elements in
      _lockLock(UnsafeRawPointer(elements))

      defer {
        _lockUnlock(UnsafeRawPointer(elements))
      }

      return try body(&header.pointee)
    }
  }
}

extension Lock: @unchecked Sendable {}
