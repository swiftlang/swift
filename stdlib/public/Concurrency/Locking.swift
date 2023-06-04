//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

internal struct _ManagedCriticalState<State> {
  final private class LockedBuffer: ManagedBuffer<State, UnsafeRawPointer> { }

  private let buffer: ManagedBuffer<State, UnsafeRawPointer>

  internal init(_ buffer: ManagedBuffer<State, UnsafeRawPointer>) {
    self.buffer = buffer
  }
  
  internal init(_ initial: State) {
    let roundedSize = (_lockWordCount() + MemoryLayout<UnsafeRawPointer>.size - 1) / MemoryLayout<UnsafeRawPointer>.size 
    self.init(LockedBuffer.create(minimumCapacity: Swift.max(roundedSize, 1)) { buffer in
      buffer.withUnsafeMutablePointerToElements { _lockInit(UnsafeRawPointer($0)) }
      return initial
    })
  }

  internal func withCriticalRegion<R>(
    _ critical: (inout State) throws -> R
  ) rethrows -> R {
    try buffer.withUnsafeMutablePointers { header, lock in
      _lock(UnsafeRawPointer(lock))
      defer {
        _unlock(UnsafeRawPointer(lock))
      }
      return try critical(&header.pointee)
    }
  }
}

extension _ManagedCriticalState: @unchecked Sendable where State: Sendable { }

extension _ManagedCriticalState: Identifiable {
  internal var id: ObjectIdentifier {
    ObjectIdentifier(buffer)
  }
}

