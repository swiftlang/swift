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

#if canImport(os)
import os
#elseif canImport(Glibc)
import Glibc
#else
import ucrt
import WinSDK
#endif

@available(SwiftStdlib 9999, *)
@usableFromInline
struct Lock<T> {
#if canImport(os)
  @usableFromInline
  var mutex: OSAllocatedUnfairLock<T>
#elseif canImport(Glibc)
  var mutex: ManagedBuffer<T, pthread_mutex_t>
#else
  var mutex: ManagedBuffer<T, SRWLOCK>
#endif
}

@available(SwiftStdlib 9999, *)
extension Lock {
  @inlinable
  func withLock<U>(_ body: @Sendable (inout T) throws -> U) rethrows -> U {
#if canImport(os)
    try mutex.withLock(body)
#elseif canImport(Glibc)
    mutex.withUnsafeMutablePointers { state, mutex in
      pthread_mutex_lock(mutex)
      
      defer {
        pthread_mutex_unlock(mutex)
      }
      
      return try body(&state.pointee)
    }
#else
    mutex.withUnsafeMutablePointers { state, mutex in
      AcquireSRWLockExclusive(mutex)
      
      defer {
        ReleaseSRWLockExclusive(mutex)
      }
      
      return try body(&state.pointee)
    }
#endif
  }
}

@available(SwiftStdlib 9999, *)
extension Lock where T: Sendable {
  @usableFromInline
  init(initialState state: T) {
#if canImport(os)
    mutex = OSAllocatedUnfairLock(initialState: state)
#elseif canImport(Glibc)
    mutex = ManagedBuffer.create(minimumCapacity: 1) {
      $0.withUnsafeMutablePointerToElements {
        var attr = pthread_attr_t()
        pthread_mutexattr_init(&attr)
        
        pthread_mutex_init($0, &attr)
      }
      
      return state
    }
#else
    mutex = ManagedBuffer.create(minimumCapacity: 1) {
      $0.withUnsafeMutablePointerToElements {
        InitializeSRWLock($0)
      }
      
      return state
    }
#endif
  }
}

@available(SwiftStdlib 9999, *)
extension Lock: @unchecked Sendable {}
