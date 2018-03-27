//===--- SwiftPrivateThreadBarriers.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
import Glibc
#elseif os(Windows)
import MSVCRT
#endif

public var _stdlib_THREAD_BARRIER_SERIAL_THREAD: CInt = 1

public class _stdlib_thread_barrier {
  private var mutex: pthread_mutex_t
  private var condition: pthread_cond_t
  private var threads: CUnsignedInt = 0
  private var waiting: CUnsignedInt = 0

  public init?(threadCount: Int) {
    guard threadCount > 0 else { return nil }
    self.mutex = pthread_mutex_t()
    self.condition = pthread_cond_t()
    self.threads = CUnsignedInt(threadCount)
  }

  public func wait() -> Int {
    if pthread_mutex_lock(&mutex) != 0 {
      fatalError("pthread_mutex_lock() failed")
    }

    self.waiting += 1

    if self.waiting == self.threads {
      self.waiting = 0
      if pthread_cond_broadcast(&condition) != 0 {
        fatalError("pthread_cond_broadcast() failed")
      }
      if pthread_mutex_unlock(&mutex) != 0 {
        fatalError("pthread_mutex_unlock() failed")
      }

      return 1
    }

    if pthread_cond_wait(&condition, &mutex) != 0 {
      fatalError("pthread_cond_wait() failed")
    }
    if pthread_mutex_unlock(&mutex) != 0 {
      fatalError("pthread_mutex_unlock() failed")
    }

    return 0
  }
}

