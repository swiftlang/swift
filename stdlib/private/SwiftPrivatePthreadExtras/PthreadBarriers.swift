//===--- PthreadBarriers.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

//
// Implement pthread barriers.
//
// (OS X does not implement them.)
//

public struct _stdlib_pthread_barrierattr_t {
  public init() {}
}

public func _stdlib_pthread_barrierattr_init(
  attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>
) -> CInt {
  return 0
}

public func _stdlib_pthread_barrierattr_destroy(
  attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>
) -> CInt {
  return 0
}

public var _stdlib_PTHREAD_BARRIER_SERIAL_THREAD: CInt {
  return 1
}

public struct _stdlib_pthread_barrier_t {
  var mutex: UnsafeMutablePointer<pthread_mutex_t> = nil
  var cond: UnsafeMutablePointer<pthread_cond_t> = nil

  /// The number of threads to synchronize.
  var count: CUnsignedInt = 0

  /// The number of threads already waiting on the barrier.
  ///
  /// This shared variable is protected by `mutex`.
  var numThreadsWaiting: CUnsignedInt = 0

  public init() {}
}

public func _stdlib_pthread_barrier_init(
  barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>,
  attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>,
  count: CUnsignedInt
) -> CInt {
  barrier.memory = _stdlib_pthread_barrier_t()
  if count == 0 {
    errno = EINVAL
    return -1
  }
  barrier.memory.mutex = UnsafeMutablePointer.alloc(1)
  if pthread_mutex_init(barrier.memory.mutex, nil) != 0 {
    // FIXME: leaking memory.
    return -1
  }
  barrier.memory.cond = UnsafeMutablePointer.alloc(1)
  if pthread_cond_init(barrier.memory.cond, nil) != 0 {
    // FIXME: leaking memory, leaking a mutex.
    return -1
  }
  barrier.memory.count = count
  return 0
}

public func _stdlib_pthread_barrier_destroy(
  barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>
) -> CInt {
  if pthread_cond_destroy(barrier.memory.cond) != 0 {
    // FIXME: leaking memory, leaking a mutex.
    return -1
  }
  if pthread_mutex_destroy(barrier.memory.mutex) != 0 {
    // FIXME: leaking memory.
    return -1
  }
  barrier.memory.cond.destroy()
  barrier.memory.cond.dealloc(1)
  barrier.memory.mutex.destroy()
  barrier.memory.mutex.dealloc(1)
  return 0
}

public func _stdlib_pthread_barrier_wait(
  barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>
) -> CInt {
  if pthread_mutex_lock(barrier.memory.mutex) != 0 {
    return -1
  }
  ++barrier.memory.numThreadsWaiting
  if barrier.memory.numThreadsWaiting < barrier.memory.count {
    // Put the thread to sleep.
    if pthread_cond_wait(barrier.memory.cond, barrier.memory.mutex) != 0 {
      return -1
    }
    if pthread_mutex_unlock(barrier.memory.mutex) != 0 {
      return -1
    }
    return 0
  } else {
    // Reset thread count.
    barrier.memory.numThreadsWaiting = 0

    // Wake up all threads.
    if pthread_cond_broadcast(barrier.memory.cond) != 0 {
      return -1
    }
    if pthread_mutex_unlock(barrier.memory.mutex) != 0 {
      return -1
    }
    return _stdlib_PTHREAD_BARRIER_SERIAL_THREAD
  }
}

