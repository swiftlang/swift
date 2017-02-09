//===--- PthreadBarriers.swift --------------------------------------------===//
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

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || CYGWIN
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
  _ attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>
) -> CInt {
  return 0
}

public func _stdlib_pthread_barrierattr_destroy(
  _ attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>
) -> CInt {
  return 0
}

public var _stdlib_PTHREAD_BARRIER_SERIAL_THREAD: CInt {
  return 1
}

public struct _stdlib_pthread_barrier_t {
#if CYGWIN || os(FreeBSD)
  var mutex: UnsafeMutablePointer<pthread_mutex_t?>?
  var cond: UnsafeMutablePointer<pthread_cond_t?>?
#else
  var mutex: UnsafeMutablePointer<pthread_mutex_t>?
  var cond: UnsafeMutablePointer<pthread_cond_t>?
#endif

  /// The number of threads to synchronize.
  var count: CUnsignedInt = 0

  /// The number of threads already waiting on the barrier.
  ///
  /// This shared variable is protected by `mutex`.
  var numThreadsWaiting: CUnsignedInt = 0

  public init() {}
}

public func _stdlib_pthread_barrier_init(
  _ barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>,
  _ attr: UnsafeMutablePointer<_stdlib_pthread_barrierattr_t>?,
  _ count: CUnsignedInt
) -> CInt {
  barrier.pointee = _stdlib_pthread_barrier_t()
  if count == 0 {
    errno = EINVAL
    return -1
  }
  barrier.pointee.mutex = UnsafeMutablePointer.allocate(capacity: 1)
  if pthread_mutex_init(barrier.pointee.mutex!, nil) != 0 {
    // FIXME: leaking memory.
    return -1
  }
  barrier.pointee.cond = UnsafeMutablePointer.allocate(capacity: 1)
  if pthread_cond_init(barrier.pointee.cond!, nil) != 0 {
    // FIXME: leaking memory, leaking a mutex.
    return -1
  }
  barrier.pointee.count = count
  return 0
}

public func _stdlib_pthread_barrier_destroy(
  _ barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>
) -> CInt {
  if pthread_cond_destroy(barrier.pointee.cond!) != 0 {
    // FIXME: leaking memory, leaking a mutex.
    return -1
  }
  if pthread_mutex_destroy(barrier.pointee.mutex!) != 0 {
    // FIXME: leaking memory.
    return -1
  }
  barrier.pointee.cond!.deinitialize()
  barrier.pointee.cond!.deallocate(capacity: 1)
  barrier.pointee.mutex!.deinitialize()
  barrier.pointee.mutex!.deallocate(capacity: 1)
  return 0
}

public func _stdlib_pthread_barrier_wait(
  _ barrier: UnsafeMutablePointer<_stdlib_pthread_barrier_t>
) -> CInt {
  if pthread_mutex_lock(barrier.pointee.mutex!) != 0 {
    return -1
  }
  barrier.pointee.numThreadsWaiting += 1
  if barrier.pointee.numThreadsWaiting < barrier.pointee.count {
    // Put the thread to sleep.
    if pthread_cond_wait(barrier.pointee.cond!, barrier.pointee.mutex!) != 0 {
      return -1
    }
    if pthread_mutex_unlock(barrier.pointee.mutex!) != 0 {
      return -1
    }
    return 0
  } else {
    // Reset thread count.
    barrier.pointee.numThreadsWaiting = 0

    // Wake up all threads.
    if pthread_cond_broadcast(barrier.pointee.cond!) != 0 {
      return -1
    }
    if pthread_mutex_unlock(barrier.pointee.mutex!) != 0 {
      return -1
    }
    return _stdlib_PTHREAD_BARRIER_SERIAL_THREAD
  }
}
