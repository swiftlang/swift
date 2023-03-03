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


@_silgen_name("_swift_observation_lock_size")
func _lockSize() -> Int

@_silgen_name("_swift_observation_lock_init")
func _lockInit(_: UnsafeRawPointer)

@_silgen_name("_swift_observation_lock_lock")
func _lockLock(_: UnsafeRawPointer)

@_silgen_name("_swift_observation_lock_unlock")
func _lockUnlock(_: UnsafeRawPointer)

@available(SwiftStdlib 5.9, *)
internal struct _ManagedCriticalState<State> {
  final private class LockedBuffer: ManagedBuffer<State, UInt8> { }

  private let buffer: ManagedBuffer<State, UInt8>

  internal init(_ buffer: ManagedBuffer<State, UInt8>) {
    self.buffer = buffer
  }
  
  internal init(_ initial: State) {
    self.init(LockedBuffer.create(minimumCapacity: _lockSize()) { buffer in
      buffer.withUnsafeMutablePointerToElements { _lockInit(UnsafeRawPointer($0)) }
      return initial
    })
  }

  internal func withCriticalRegion<R>(
    _ critical: (inout State) throws -> R
  ) rethrows -> R {
    try buffer.withUnsafeMutablePointers { header, lock in
      _lockLock(UnsafeRawPointer(lock))
      defer { _lockUnlock(UnsafeRawPointer(lock)) }
      return try critical(&header.pointee)
    }
  }
}

@available(SwiftStdlib 5.9, *)
internal protocol _Deinitializable {
  mutating func deinitialize()
}

@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState where State: _Deinitializable {
  final private class DeinitializingLockedBuffer: 
    ManagedBuffer<State, UInt8> {
    deinit {
      withUnsafeMutablePointers { header, lock in
        header.pointee.deinitialize()
      }
    }
  }
  
  internal init(managing initial: State) {
    self.init(DeinitializingLockedBuffer.create(minimumCapacity: _lockSize()) { buffer in
      buffer.withUnsafeMutablePointerToElements { _lockInit(UnsafeRawPointer($0)) }
      return initial
    })
  }
}

@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState: @unchecked Sendable where State: Sendable { }

@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState: Identifiable {
  internal var id: ObjectIdentifier {
    ObjectIdentifier(buffer)
  }
}
